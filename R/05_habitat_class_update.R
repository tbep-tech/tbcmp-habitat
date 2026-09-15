# setup ------------------------------------------------------------------

library(tidyverse)
library(sf)
library(terra)
library(here)

source(here('R', 'funcs.R'))

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
# FLUCCShabsclass.csv). Soft developed land (HMPU_GROUP 'Restorable', e.g.
# pasture, row crops, groves, mined and reclaimed land, golf courses, open
# land) retains restoration potential; hard developed land (all remaining
# non-opportunity land) retains little. Values are placeholders below the
# 0.3 minimum of the opportunity categories, adjust as needed.
devwgts <- tribble(
  ~cat,                 ~wgt,
  'Moderate Opportunity', 0.25,
  'Some Opportunity',   0.2,
  'Little Opportunity', 0.1
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

# inverse weight layer ---------------------------------------------------

# inverse as the complement on the unit scale, i.e., a cost-style surface
# where the highest priority category (Proposed Conservation Native,
# vulnerable) takes the lowest value
# use wgtinv = 1 / wgt instead for a reciprocal inverse
oppmap_all <- oppmap_all |>
  mutate(wgtinv = 1 - wgt)

# new region-wide layer dissolved across counties by category, carrying the
# inverse weights
oppmap_wgtinv <- oppmap_all |>
  group_by(cat, wgt, wgtinv) |>
  summarise(.groups = 'drop') |>
  st_make_valid() |>
  arrange(wgtinv)
oppmap_wgtinv <- st_sf(
  st_drop_geometry(oppmap_wgtinv),
  geometry = st_geometry(oppmap_wgtinv)
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

# land outside the eight opportunity categories, retained as two features
noopp <- land_all |>
  st_difference(st_union(st_geometry(oppmap_wgtinv))) |>
  st_make_valid() |>
  left_join(devwgts, by = 'cat') |>
  mutate(wgtinv = 1 - wgt)
noopp <- st_sf(st_drop_geometry(noopp), geometry = st_geometry(noopp))

oppmap_wgtinv <- bind_rows(oppmap_wgtinv, noopp) |>
  arrange(wgtinv)

# save -------------------------------------------------------------------

# combined county outputs with weights
save(
  oppmap_all,
  file = file.path(out_dir, 'oppmap_all.RData'),
  compress = 'xz'
)

# inverse weight layer
save(
  oppmap_wgtinv,
  file = file.path(out_dir, 'oppmap_wgtinv.RData'),
  compress = 'xz'
)
st_write(
  oppmap_wgtinv,
  file.path(out_dir, 'oppmap_wgtinv.shp'),
  delete_layer = TRUE
)

message('  Saved oppmap_all, oppmap_wgtinv')

# rasterize land-only surfaces --------------------------------------------

# template grid from the HEM vulnerability hot spot raster (10 m) so outputs
# align cell-for-cell with tbcmp-analyze products, extended to cover the full
# opportunity map surface
vuln_max <- rast(
  here('data-raw', '01_inputs', 'tbcmp_hot_spot_maps_1-2_max.tif')
)
oppmap_vect <- vect(oppmap_wgtinv)
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

# weight and inverse weight rasters, masked to land portions only (non-land
# cells remain NA via the land mask); null land areas are filled with zero
# for the weight surface and one for the inverse surface, consistent with
# wgtinv = 1 - wgt
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

plot(oppmap_wgtinv['wgt'], border = NA, main = 'Current weight')
plot(oppmap_wgtinv['wgtinv'], border = NA, main = 'Inverse weight')
plot(oppmap_wgt_rst, main = 'Weight (land only)')
plot(oppmap_wgtinv_rst, main = 'Inverse weight (land only)')
