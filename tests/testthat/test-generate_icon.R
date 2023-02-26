test_that("Generate icon works", {

  map_name <- "col_municipalities_antioquia"

  generate_icon(map_name)
  generate_icon(map_name, subdivision = TRUE)
  generate_icon(map_name, subdivision = TRUE,
                border_width = 0.5)


  map_name <- "world_countries_america"
  generate_icon(map_name, tolerance = 0.5, border_width = 1,
                topologyPreserve = FALSE)
  generate_icon(map_name, tolerance = 0.1, topologyPreserve = FALSE)

})
