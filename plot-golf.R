library(osmdata)
library(sf)
library(dplyr)
library(ggplot2)
library(smoothr)

devon_golf <- 
  opq("devon uk") %>% 
  add_osm_feature(key = "leisure", value = "golf_course") %>%
  osmdata_sf()

search_string <- "Exeter"
search_string <- "Exminster"
search_string <- "Woodbury"

course <- 
  devon_golf$osm_polygons %>% 
  st_as_sf() %>% 
  filter(stringr::str_detect(name, search_string))

golf <- 
  opq(bbox = st_bbox(course)) %>% 
  add_osm_feature(key = "golf") %>% 
  osmdata_sf()

golf$osm_polygons$golf <- 
  factor(
    x = golf$osm_polygons$golf, 
    levels = rev(c(
      "clubhouse",
      "rough",
      "fairway",
      "water_hazard",
      "bunker",
      "tee",
      "green",
      "driving_range"
    ))
    )

plot_golf <- function(golf) {
  golf$osm_polygons %>% 
    st_as_sf() %>% 
    ggplot() + 
    #geom_sf(data = course, fill = "green4") +
    geom_sf(aes(fill = golf)) + 
    geom_sf(data = golf$osm_multipolygons, aes(fill = golf)) +
    scale_fill_manual(
      values = c(
        "clubhouse" = "red",
        "rough" = "darkgreen",
        "fairway" = "green3",
        "water_hazard" = "blue",
        "bunker" = "yellow",
        "tee" = "white",
        "green" = "green3",
        "driving_range" = "green4"
      ), na.value = "green3"
    ) +
    geom_sf(data = golf$osm_points %>% filter(golf == "pin")) +
    geom_sf(data = golf$osm_lines, aes(linetype = golf)) +
    #geom_sf_text(data = golf$osm_lines %>% st_centroid(), aes(label = ref)) +
    scale_linetype_manual(values = c("hole" = 1, "cartpath" = 2)) + 
    theme_void()
}

plot_golf(golf)

plot_hole <- function(golf, hole_number = 1){
  hole_line <- golf$osm_lines %>% st_as_sf() %>% filter(ref == hole_number) %>% slice(nrow(.))
  hole_polys <- golf$osm_polygons %>% st_filter(hole_line) 
  hole_area <- st_transform(st_buffer(st_transform(hole_polys, 27700), 10), 4326)
  try(golf$osm_polygons <- golf$osm_polygons %>% st_filter(hole_area) %>% smooth())
  try(golf$osm_multipolygons <- golf$osm_multipolygons %>% st_filter(hole_area) %>% smooth())
  #golf$osm_multilines <- golf$osm_multilines %>% st_filter(hole_area)
  golf$osm_points <- golf$osm_points %>% st_filter(hole_area)
  golf$osm_lines <- golf$osm_lines %>% st_filter(hole_area) %>% filter(golf == "hole")
  plot_golf(golf) + labs(
    title = paste("Hole", hole_number), 
    subtitle = paste(round(1.0936 * st_length(golf$osm_lines)), "yards")
    ) +
    theme(legend.position = "none")
  }

plot_hole(golf, 1)
plot_hole(golf, 2)
plot_hole(golf, 3)
plot_hole(golf, 4)
plot_hole(golf, 5)
plot_hole(golf, 6)
plot_hole(golf, 7)
plot_hole(golf, 8)
plot_hole(golf, 9)
plot_hole(golf, 10)
plot_hole(golf, 11)
plot_hole(golf, 12)
plot_hole(golf, 13)
plot_hole(golf, 14)
plot_hole(golf, 15)
plot_hole(golf, 16)
plot_hole(golf, 17)
plot_hole(golf, 18)
