# append to R/funcs.R

#' Map the weighted opportunity layer for one county
#'
#' @param wgtdat sf object from 05_habitat_class_update.R, with cat and wgt
#' @param county chr string of the county name
#' @param tbcmp_cnt sf object of the seven county boundaries
#' @param simplify numeric tolerance passed to sf::st_simplify, NULL for none
#'
#' @return a leaflet map with one layer per category, shaded by weight
wgtmap_leaflet <- function(wgtdat, county, tbcmp_cnt, simplify = NULL) {
  if (!is.null(simplify)) {
    wgtdat <- sf::st_simplify(wgtdat, dTolerance = simplify)
  }

  wgtdat <- wgtdat[!sf::st_is_empty(wgtdat), ]

  wgtdat_4326 <- sf::st_transform(wgtdat, 4326)
  tbcmp_cnt_4326 <- sf::st_transform(tbcmp_cnt, 4326) |>
    dplyr::filter(county == !!county)

  # categories ordered high to low weight, as shown in the layer control
  lkup <- wgtdat_4326 |>
    sf::st_drop_geometry() |>
    dplyr::distinct(cat, wgt) |>
    dplyr::arrange(dplyr::desc(wgt), cat)

  # weights span zero to one by definition, so the ramp is fixed across
  # counties and maps are comparable
  pal <- leaflet::colorNumeric(
    'viridis',
    domain = c(0, 1),
    na.color = 'transparent'
  )

  m <- leaflet::leaflet() |>
    leaflet::addProviderTiles(leaflet::providers$Esri.WorldGrayCanvas)

  for (i in seq_len(nrow(lkup))) {
    cat_nm <- lkup$cat[i]
    cat_data <- dplyr::filter(wgtdat_4326, cat == cat_nm)
    if (nrow(cat_data) == 0) {
      next
    }
    m <- leaflet::addPolygons(
      m,
      data = cat_data,
      fillColor = pal(lkup$wgt[i]),
      fillOpacity = 0.8,
      color = NA,
      weight = 0,
      label = paste0(cat_nm, ' (', lkup$wgt[i], ')'),
      group = cat_nm
    )
  }

  m |>
    leaflet::addPolygons(
      data = tbcmp_cnt_4326,
      fill = FALSE,
      color = 'black',
      weight = 1,
      opacity = 0.5
    ) |>
    leaflet::addLayersControl(
      overlayGroups = lkup$cat,
      options = leaflet::layersControlOptions(collapsed = FALSE)
    ) |>
    leaflet::addLegend(
      colors = pal(lkup$wgt),
      labels = paste0(lkup$cat, ' (', lkup$wgt, ')'),
      title = 'Weight',
      position = 'bottomright'
    )
}
