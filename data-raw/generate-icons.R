
library(sf)
library(geodato)
library(tidyverse)
library(ggplot2)

search_maps("quindio")



all_maps <- available_maps()
names(all_maps) <- all_maps


# map_name <- all_maps[1]
# map_name <- "col_municipalities_quindio" # 3.5k, tol = 0
# map_name <- "col_departments" #19k, tol = 0.1
# map_name <- "col_municipalities" 28k, tol = 0.2
# map_name <- "gtm_municipalities" 28k
# map_name <- "gtm_departments" 252k
# map_name <- "world_countries" # 512k, tol = 2
dataset <- data.frame(
  x = c(3500, 19000, 28000, 512000),
  y = c(0, 0.1, 0.2, 3))

fit <- lm(y~x+I(x^2), data = dataset)
fit <- lm(y~x+I(x^2)+I(x^3), data=dataset)
fit
predict_tol <- function(map_name, plot){

  tj <- gd_tj(map_name)
  union <- sf::st_union(tj)
  obj_size <- pryr::object_size(union)
  tolerance <- predict(fit, newdata = data.frame(x = as.numeric(obj_size)))
  tolerance <- unname(tolerance)
  tolerance <- ifelse(tolerance < 0, 0, tolerance)

  if(plot){
    plot(union)
    plot(union_simple)
  }

  tolerance

}

tols <- purrr::map(all_maps, safely(function(m, plot = FALSE){
  predict_tol(m, plot)
}))
errors <- keep(tols, ~ !is.null(.$error))
length(errors)
errors

map_union_tolerance <- tols

usethis::use_data(map_union_tolerance, internal = TRUE, overwrite = TRUE)


i <- 1
l <- purrr::map(all_maps, safely(function(map_name){
  message(i)
  i <<- i + 1
  message(map_name)
  # map_name <- "col_municipalities_antioquia"

  tj <- gd_tj(map_name)
  union <- sf::st_union(tj)

  tolerance <- NULL
  tol <- tols[[map_name]]$result
  if(!is.null(tol)){
    tolerance <- tol
  }
  union <- rgeos::gSimplify(
    methods::as( object = union, Class = "Spatial"),
    tol = tolerance, topologyPreserve = FALSE) |> st_as_sf()


  g <- ggplot() +
    geom_sf(data = tj, color= "#999999", linewidth = 0.5) +
    geom_sf(data = union, fill = "transparent", color= "#444444", linewidth = 1.5) +
    gggeo::theme_nothing()
  g
  ggsave(filename = paste0("data-raw/icons/", map_name, ".svg"),
         width = 2, height = 2)

  g <- ggplot() +
    geom_sf(data = union, fill = "transparent", color= "#444444", linewidth = 1.5) +
    gggeo::theme_nothing()
  g
  ggsave(filename = paste0("data-raw/icons-border/", map_name, ".svg"),
         width = 2, height = 2)
}))

errors <- l |> keep(~!is.null(.$error))
length(errors)
errors

