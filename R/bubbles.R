#' Ggplot choropleths by numerical variable
#'
#' @name gg_choropleth
#' @param data A data.frame
#' @return ggplot viz
#' @section ctypes: Gcd-Num
#' @export
#' @examples
#' gg_bubbles()
gg_bubbles <- function(data = NULL, map_name = NULL,
                       var_size = NULL, var_color = NULL,
                       opts = NULL){


  var_size <- geodato::parse_col(data, var_size)
  data$..var_size <- data[[var_size]]

  var_color <- geodato::parse_col(data, var_color)
  data$..var_color <- data[[var_color]]



  d <- geodato::gd_match(data, map_name) |>
    ungroup() |>
    select(..var_size, ..var_color, id = ..gd_id, lon, lat)

  tj <- geodato::gd_tj(map_name) |> left_join(d)

  g <- ggplot() +
    geom_sf(data = tj, fill = "#CCCCCC", color= "#444444") +
    geom_point(data = d, aes(lon, lat,
                             size = ..var_size,
                             color = ..var_color))


  g <- g +
    theme_nothing() +
    labs(title = opts$title,
         subtitle = opts$subtitle,
         caption = opts$caption,
         fill = opts$legend)
  g

}
