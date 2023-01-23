

#' Ggplot choropleths by numerical variable
#'
#' @name gg_choropleth
#' @param data A data.frame
#' @return ggplot viz
#' @section ctypes: Gcd-Num
#' @export
#' @examples
#' gg_choropleth_GcdNum(sample_data("Gcd-Num", nrow = 10))
gg_choropleth <- function(data = NULL, map_name = NULL, var = NULL,
                          opts = NULL, filter = NULL){


  col <- geodato::parse_col(data, var)

  #opts <- dsvizopts::merge_dsviz_options(...)
  #l <- geomagic_prep(data = data, opts = opts, by = "id")

  data$..var <- data[[col]]

  d <- geodato::gd_match(data, map_name)
  tj <- geodato::gd_tj(map_name)

  dgeo <- tj |>
    dplyr::left_join(d, by = c(id = "..gd_id", name = "..gd_name"))

  if(!is.null(filter)){
    code_or_name <- geodato:::is_code_or_name(filter, map_name)
    if(code_or_name == "name"){
      filter <- tibble::tibble(filter = filter) |>
        gd_match(map_name) |> pull(..gd_id)
    }
    dgeo <- dgeo |> filter(id %in% filter)
  }
  if (is.null(data)){
    g <- ggplot(tj) +
      geom_sf(fill = "#CCCCCC", color= "#444444")
  } else {
    g <- ggplot(dgeo) + geom_sf(aes(fill = ..var))
  }
  g <- g +
    theme_nothing() +
    labs(title = opts$title,
         subtitle = opts$subtitle,
         caption = opts$caption,
         fill = opts$legend)
  g

}





