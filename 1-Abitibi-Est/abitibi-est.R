library(tidyverse)
circonscription = sf::st_read('./Circonscriptions_électorales_2017_shapefile.shp') %>%
  sf::st_transform(4326) %>% 
  arrange(NM_CEP) %>%
  slice(1) %>%
  sf::st_make_valid()

library(rgee)
rgee::ee_Initialize()
circonscription_ee = circonscription %>% 
  sf::st_geometry() %>%
  sf_as_ee()

pertes = ee$Image('UMD/hansen/global_forest_change_2021_v1_9')$
  select('lossyear')$
  gt(0)$
  reduceToVectors(
  geometry= circonscription_ee,
  scale=50,
  geometryType= 'polygon',
  maxPixels = 10000000000
)$geometry()$simplify(maxError=10)

polygone_pertes=ee_as_sf(pertes, maxFeatures = 10000000)

carte = ggplot()+
  layer_spatial(circonscription, fill = '#1A1A1A', color = 'transparent')+
  ggfx::with_outer_glow(
    ggfx::with_inner_glow(
      annotation_spatial(polygone_pertes,
                         fill = 'white',
                         color = 'transparent'),
      colour="green",
      sigma=5,
      expand=.5),
    colour="darkgreen",
    sigma=5,
    expand=.1) +
  theme_void()

ggsave('abitibi-est.png',carte,width = 6000, height = 6000, units = 'px')