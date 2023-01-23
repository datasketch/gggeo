test_that("multiplication works", {

  #opts <- dsvizopts::merge_dsviz_options(...)

  map_name <- "col_departments"
  data <- geodato::sample_data$col_departments$plebiscito_2016_departamentos
  data <- data |>
    group_by(departamento) |>
    arrange(desc(porcentaje)) |>
    slice(1)

  var <- "resultado"
  gg_choropleth(data, map_name, var = var)

  var <- "porcentaje"
  gg_choropleth(data, map_name, var = var)

  var_size <- "porcentaje"
  gg_bubbles(data, map_name, var_size = var_size)
  var_color <- "resultado"
  gg_bubbles(data, map_name, var_size = var_size, var_color = var_color)


  map_name <- "col_municipalities"
  data <- geodato::sample_data$col_municipalities$plebiscito_2016_municipios
  data <- data |>
    group_by(departamento, municipio) |>
    arrange(desc(porcentaje)) |>
    slice(1) |>
    ungroup()

  var <- "resultado"
  gg_choropleth(data, map_name,  var = var)

  filter <- c("Bogotá", "cajicá")
  gg_choropleth(data, map_name,  var = var, filter = filter)

  var_color <- "resultado"
  var_size <- "porcentaje"
  gg_bubbles(data, map_name,  var_size = var_size, var_color = var_color)


})
