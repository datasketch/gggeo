test_that("multiplication works", {

  #opts <- dsvizopts::merge_dsviz_options(...)

  map_name <- "gtm_departments"
  data <- geodato::sample_data$gtm_muncipalities$`vacunados-muni` |>
    rename(vacunados = `Vacunados con primera dosis`)
  data <- data |>
    dplyr::summarize(vacunados = sum(vacunados), .by = `departamento`)
  var <- "vacunados"
  gg_choropleth(data, map_name, var = var)



  map_name <- "gtm_municipalities"
  data <- geodato::sample_data$gtm_muncipalities$`vacunados-muni` |>
    dplyr::rename(vacunados = `Vacunados con primera dosis`)
  var <- "vacunados"
  gg_choropleth(data, map_name,  var = var)




  var_color <- "resultado"
  var_size <- "porcentaje"
  gg_bubbles(data, map_name,  var_size = var_size, var_color = var_color)




})
