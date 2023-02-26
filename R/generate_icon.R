
#' @export
generate_icon <- function(map_name,
                          subdivision = FALSE,
                          border_color = "#444444",
                          background_color = "transparent",
                          border_width = 1.5,
                          border_width_subdivision = NULL,
                          tolerance = NULL,
                          topologyPreserve = TRUE,
                          path = NULL,
                          width = 2,
                          height = 2
){

  if(is.null(tolerance)){
    # Get suggested tolerance
    tol <- gggeo:::map_union_tolerance[[map_name]]$result
    if(!is.null(tol)){
      tolerance <- tol
    }
  }

  tj <- geodato::gd_tj(map_name)
  union <- st_union(tj)
  union <- rgeos::gSimplify(
    methods::as( object = union, Class = "Spatial"),
    tol = tolerance, topologyPreserve = topologyPreserve) |> st_as_sf()

  g <- ggplot()
  if(subdivision){
    border_width_subdivision <- border_width_subdivision %||% border_width/4
    g <- g + geom_sf(data = tj, color = border_color,
                     linewidth = border_width_subdivision)
  }

  g <- g +
    geom_sf(data = union, fill = background_color,
            color= border_color, linewidth = border_width) +
    gggeo::theme_nothing()


  if(!is.null(path)){
    ggsave(filename = path,
           width = width, height = height)
  }
  g
}

