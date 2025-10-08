# should this also support richness or single species?
inat_rangemap <- function(
  poly,
  zoom,
  id_column = "id",
  taxa_selections = list()
) {
  poly_hexed <- get_h3_aoi(poly, precision = zoom, keep_cols = id_column)
}
