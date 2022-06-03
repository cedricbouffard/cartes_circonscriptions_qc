library(tidyverse)
library(osmdata)
library(rayshader)

circonscription <- sf::st_read("Circonscriptions_électorales_2017_shapefile.shp") %>%
  sf::st_transform(4326) %>%
  arrange(NM_CEP) %>%
  slice(3) %>%
  sf::st_make_valid()

x <- opq(bbox = circonscription %>% sf::st_bbox()) %>%
  add_osm_feature(key = "highway") %>%
  osmdata_sf()

routes <- sf::st_intersection(x$osm_lines, circonscription) %>%
  filter(highway %in% c("secondary", "residential", "tertiary", "motorway", "primary", "unclassified")) %>%
  group_by(name, highway) %>%
  summarise() %>%
  sf::st_transform(3857) %>%
  sf::st_simplify(dTolerance = 50)




p <- ggplot() +
  # ggfx::with_inner_glow(ggspatial::layer_spatial(building %>% sf::st_sf(), aes(fill = 1), color = 'transparent'), colour = 'grey')+
  ggfx::with_outer_glow(ggspatial::layer_spatial(routes4 %>% filter(highway == "unclassified") %>% sf::st_buffer(5) %>% sf::st_transform(4326) %>% sf::st_sf(), aes(fill = 3), color = "transparent"), colour = "black") +
  ggfx::with_outer_glow(ggspatial::layer_spatial(routes4 %>% filter(highway == "residential") %>% sf::st_buffer(8) %>% sf::st_transform(4326) %>% sf::st_sf(), aes(fill = 4), color = "transparent"), colour = "black") +
  ggfx::with_outer_glow(ggspatial::layer_spatial(routes4 %>% filter(highway == "tertiary") %>% sf::st_buffer(10) %>% sf::st_transform(4326) %>% sf::st_sf(), aes(fill = 5), color = "transparent"), colour = "black") +
  ggfx::with_outer_glow(ggspatial::layer_spatial(routes4 %>% filter(highway == "secondary") %>% sf::st_buffer(12) %>% sf::st_transform(4326) %>% sf::st_sf(), aes(fill = 6), color = "transparent"), colour = "black") +
  ggfx::with_outer_glow(ggspatial::layer_spatial(routes4 %>% filter(highway == "primary") %>% sf::st_buffer(15) %>% sf::st_transform(4326) %>% sf::st_sf(), aes(fill = 7), color = "transparent"), colour = "black") +
  ggfx::with_outer_glow(ggspatial::layer_spatial(routes4 %>% filter(highway == "motorway") %>% sf::st_buffer(20) %>% sf::st_transform(4326) %>% sf::st_sf(), aes(fill = 8), color = "transparent"), colour = "black") +
  scale_fill_gradient(low = "white", high = "white") +
  theme_void() +
  theme(panel.background = element_rect(fill = "#DBDBDB", color = "#DBDBDB"), legend.position = "none")


rayshader::plot_gg(p,
  scale = 50, width = 8, height = 8, multicore = T, zoom = .66,
  phi = 75,
  theta = 0,
  fov = 0
)

render_highquality("acadie.png", samples = 300, width = 4000, height = 4000)
