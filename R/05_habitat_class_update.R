# setup ------------------------------------------------------------------

library(tidyverse)
library(sf)
library(terra)
library(here)

source(here('R', 'funcs.R'))

# keep only the polygonal parts of a layer, one feature per input row
# st_intersection() and st_difference() return GEOMETRYCOLLECTION where an
# edge is shared with the clipping geometry, which the shapefile driver
# rejects. st_collection_extract() alone would split every feature into its
# component polygons, so the parts are recombined per row.
polyonly <- function(dat) {
  geo <- st_geometry(dat)
  iscol <- st_geometry_type(geo) == 'GEOMETRYCOLLECTION'

  if (any(iscol)) {
    geo[iscol] <- st_sfc(
      lapply(geo[iscol], function(g) {
        p <- st_collection_extract(st_sfc(g), 'POLYGON')
        if (length(p) == 0) {
          return(st_multipolygon())
        }
        st_combine(p)[[1]]
      }),
      crs = st_crs(geo)
    )
  }

  st_geometry(dat) <- st_cast(geo, 'MULTIPOLYGON')

  dat[!st_is_empty(st_geometry(dat)), ]
}

in_dir <- here('data', '04_opportunities_maps')
out_dir <- here('data', '05_habitat_class_update')
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# weights from the Overview section of https://tbep-tech.github.io/tbcmp-habitat
# prioritizes vulnerable > non-vulnerable, proposed > existing, and
# native > restorable, nested in that order
wgts <- tribble(
  ~cat,                                           ~wgt,
  'Existing Conservation Native',                 0.4,
  'Existing Conservation Native, vulnerable',     0.8,
  'Existing Conservation Restorable',             0.3,
  'Existing Conservation Restorable, vulnerable', 0.7,
  'Proposed Conservation Native',                 0.6,
  'Proposed Conservation Native, vulnerable',     1.0,
  'Proposed Conservation Restorable',             0.5,
  'Proposed Conservation Restorable, vulnerable', 0.9
)

# weights for land outside the eight opportunity categories, partitioned by
# development intensity from the FLUCCS lookup (data-raw/01_inputs/
# FLUCCShabsclass.csv). Native land outside existing and proposed
# conservation (HMPU_GROUP 'Native') is undeveloped habitat and retains the
# most potential; soft developed land (HMPU_GROUP 'Restorable', e.g. pasture,
# row crops, groves, mined and reclaimed land, golf courses, open land)
# retains some; hard developed land (HMPU_GROUP 'Developed' and any code
# absent from the lookup) retains little. Values are placeholders below the
# 0.3 minimum of the opportunity categories, adjust as needed.
devwgts <- tribble(
  ~cat,                   ~wgt,
  'Moderate Opportunity', 0.25,
  'Some Opportunity',     0.2,
  'Little Opportunity',   0.0
)

# subtidal FLUCCS codes defining non-land areas, consistent with lulc_est()
# in funcs.R (bays/estuaries, major water bodies, gulf, tidal flats, oyster
# bars, submerged sand, seagrasses, attached algae, hardbottom). Freshwater
# open water codes are dropped separately below via HMPU_GROUP so lakes,
# reservoirs and streams are not classed as developed land.
cds <- c(
  5400,
  5700,
  5720,
  6510,
  6540,
  7210,
  9113,
  9115,
  9116,
  9121,
  9510,
  9511,
  9512,
  9513,
  9514,
  9515
)

# combine county outputs -------------------------------------------------

# all county opportunity maps from 04_opportunities_maps.R
fls <- list.files(in_dir, pattern = '^oppmap_.*\\.RData$', full.names = TRUE)
stopifnot(length(fls) > 0)

oppmap_all <- fls |>
  map(function(fl) {
    obj_name <- tools::file_path_sans_ext(basename(fl))
    county_lower <- gsub('^oppmap_', '', obj_name)
    message('  Loading ', obj_name)

    load(fl)

    get(obj_name) |>
      mutate(county = str_to_title(county_lower))
  }) |>
  bind_rows()

# assign weights ---------------------------------------------------------

oppmap_all <- oppmap_all |>
  left_join(wgts, by = 'cat')

# verify all categories matched the weighting scheme
chk <- oppmap_all |>
  st_drop_geometry() |>
  filter(is.na(wgt)) |>
  pull(cat) |>
  unique()
if (length(chk) > 0) {
  stop('Categories with no assigned weight: ', paste(chk, collapse = ', '))
}

# weighted layer ---------------------------------------------------------

# inverse weight as the complement on the unit scale, i.e., a cost-style
# surface where the highest priority category (Proposed Conservation Native,
# vulnerable) takes the lowest value
# use wgtinv = 1 / wgt instead for a reciprocal inverse
oppmap_all <- oppmap_all |>
  mutate(wgtinv = 1 - wgt)

# new region-wide layer dissolved across counties by category, carrying the
# weights and their inverse
oppmap_wgt <- oppmap_all |>
  group_by(cat, wgt, wgtinv) |>
  summarise(.groups = 'drop') |>
  st_make_valid() |>
  arrange(wgtinv)
oppmap_wgt <- st_sf(
  st_drop_geometry(oppmap_wgt),
  geometry = st_geometry(oppmap_wgt)
)

# developed land classes ------------------------------------------------

# land footprint from county LULC, split into soft and hard developed using
# HMPU_GROUP from the FLUCCS lookup
load(here('data', '01_inputs', 'fluccs.RData'))
load(here('data', '01_inputs', 'tbcmp_cnt.RData'))

fluccsgrp <- fluccs |>
  select(FLUCCSCODE, HMPU_GROUP) |>
  mutate(FLUCCSCODE = as.integer(FLUCCSCODE)) |>
  distinct()

# all water codes excluded from the land footprint
watcds <- fluccsgrp |>
  filter(HMPU_GROUP == 'Open Water') |>
  pull(FLUCCSCODE) |>
  union(cds)

land_all <- tbcmp_cnt$county |>
  map(function(county) {
    county_lower <- tolower(county)
    message('  Land footprint: ', county)

    load(here('data', '01_inputs', paste0('lulc_', county_lower, '.RData')))

    dat <- get(paste0('lulc_', county_lower)) |>
      mutate(FLUCCSCODE = as.integer(FLUCCSCODE)) |>
      filter(!FLUCCSCODE %in% watcds) |>
      left_join(fluccsgrp, by = 'FLUCCSCODE') |>
      mutate(
        cat = case_when(
          HMPU_GROUP == 'Native' ~ 'Moderate Opportunity',
          HMPU_GROUP == 'Restorable' ~ 'Some Opportunity',
          .default = 'Little Opportunity'
        )
      ) |>
      group_by(cat) |>
      summarise(.groups = 'drop')

    st_sf(cat = dat$cat, geometry = st_geometry(dat))
  }) |>
  bind_rows() |>
  group_by(cat) |>
  summarise(.groups = 'drop') |>
  st_make_valid()
land_all <- st_sf(cat = land_all$cat, geometry = st_geometry(land_all))

# land outside the eight opportunity categories, retained as three features
noopp <- land_all |>
  st_difference(st_union(st_geometry(oppmap_wgt))) |>
  st_make_valid() |>
  polyonly() |>
  left_join(devwgts, by = 'cat') |>
  mutate(wgtinv = 1 - wgt)
noopp <- st_sf(st_drop_geometry(noopp), geometry = st_geometry(noopp))

# acreage of each class outside the opportunity categories
noopp |>
  mutate(acres = as.numeric(st_area(geometry)) / 4046.86) |>
  st_drop_geometry() |>
  select(cat, wgt, wgtinv, acres) |>
  print()

oppmap_wgt <- bind_rows(oppmap_wgt, noopp) |>
  arrange(wgtinv)

# save -------------------------------------------------------------------

# combined county outputs with weights
save(
  oppmap_all,
  file = file.path(out_dir, 'oppmap_all.RData'),
  compress = 'xz'
)

# region-wide weighted layer
save(
  oppmap_wgt,
  file = file.path(out_dir, 'oppmap_wgt.RData'),
  compress = 'xz'
)
st_write(
  oppmap_wgt,
  file.path(out_dir, 'oppmap_wgt.shp'),
  delete_layer = TRUE
)

# county subsets of the weighted layer for display
for (county in tbcmp_cnt$county) {
  county_lower <- tolower(county)
  obj_name <- paste0('oppmap_wgt_', county_lower)
  message('  County subset: ', county)

  cnt_geom <- tbcmp_cnt |>
    filter(county == !!county) |>
    st_geometry()

  # geometry collections along the county boundary are reduced to polygons
  assign(
    obj_name,
    oppmap_wgt |>
      st_intersection(cnt_geom) |>
      st_make_valid() |>
      polyonly()
  )

  save(
    list = obj_name,
    file = file.path(out_dir, paste0(obj_name, '.RData')),
    compress = 'xz'
  )
  st_write(
    get(obj_name),
    file.path(out_dir, paste0(obj_name, '.shp')),
    delete_layer = TRUE
  )

  rm(list = obj_name)
}

# save all shapefiles in a single zipped folder called oppmap_wgt_shp
shp_files <- list.files(
  out_dir,
  pattern = '\\.(shp|dbf|shx|prj|cpg|sbn|sbx|xml)$',
  full.names = TRUE
)
zip::zip(
  zipfile = file.path(out_dir, 'oppmap_wgt_shp.zip'),
  files = shp_files,
  mode = 'cherry-pick'
)

message('  Saved oppmap_all, oppmap_wgt, county subsets')

# rasterize land-only surfaces --------------------------------------------

# template grid from the HEM vulnerability hot spot raster (10 m) so outputs
# align cell-for-cell with tbcmp-analyze products, extended to cover the full
# opportunity map surface
vuln_max <- rast(
  here('data-raw', '01_inputs', 'tbcmp_hot_spot_maps_1-2_max.tif')
)
oppmap_vect <- vect(oppmap_wgt)
stopifnot(same.crs(vuln_max, oppmap_vect))
template <- rast(vuln_max) |>
  extend(ext(oppmap_vect))

# land mask on the template grid from the land footprint built above
landrst <- rasterize(vect(st_geometry(land_all)), template, field = 1)

# weight and inverse weight rasters, masked to land portions only (non-land
# cells remain NA via the land mask); with the Some Opportunity and Little
# Opportunity features the vector layer now covers all land, so the fills
# (zero for the weight surface, one for the inverse surface, consistent with
# wgtinv = 1 - wgt) only catch edge slivers from raster/vector misalignment
oppmap_wgt_rst <- rasterize(oppmap_vect, template, field = 'wgt') |>
  subst(NA, 0) |>
  mask(landrst)
names(oppmap_wgt_rst) <- 'wgt'

oppmap_wgtinv_rst <- rasterize(oppmap_vect, template, field = 'wgtinv') |>
  subst(NA, 1) |>
  mask(landrst)
names(oppmap_wgtinv_rst) <- 'wgtinv'

writeRaster(
  oppmap_wgt_rst,
  file.path(out_dir, 'oppmap_wgt.tif'),
  overwrite = TRUE
)
writeRaster(
  oppmap_wgtinv_rst,
  file.path(out_dir, 'oppmap_wgtinv.tif'),
  overwrite = TRUE
)

message('  Saved oppmap_wgt.tif, oppmap_wgtinv.tif')

# view map ---------------------------------------------------------------

plot(oppmap_wgt['wgt'], border = NA, main = 'Weight')
plot(oppmap_wgt_rst, main = 'Weight (land only)')
plot(oppmap_wgtinv_rst, main = 'Inverse weight (land only)')
