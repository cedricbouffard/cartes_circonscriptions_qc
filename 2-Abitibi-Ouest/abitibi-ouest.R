library(tidyverse)
library(rgee)
library(raster)
library(rayshader)
# On commence par charger les polygones des circonscription électorales et on choisi la 2e
circonscription = sf::st_read('./Circonscriptions_électorales_2017_shapefile.shp') %>%
  sf::st_transform(4326) %>% 
  arrange(NM_CEP) %>%
  slice(2) %>%
  sf::st_make_valid()

#  J'utilise Google Earth Engine pour accéder aux données d'élévation

rgee::ee_Initialize( drive = T)
#  On Envoie le polygone sur les serveur de Google Earth Engine
circonscription_ee = circonscription %>% 
  sf::st_geometry() %>%
  sf_as_ee()
# J'utilise dans ce cas l'élvation provenant de NRCan
elevation=ee$ImageCollection('NRCan/CDEM')$select('elevation')$
  filterBounds(circonscription_ee)$
  max()
# J'applique un filtre boxcar pour réduire les petites variation
smooth=elevation$convolve(ee$Kernel$square(radius=3))$clip(circonscription_ee) 

# Je télécharge la topographie, il faut jour avec le "scale" pour trouver un niveau adéquat
r=rgee::ee_as_raster(smooth, 
                     region = circonscription_ee$bounds(),
                     scale = 100)

dem = mask(r, circonscription)


# On convertit la topo en matrice
matrice = raster_to_matrix(dem)
# J'utilise la palette "earth" de futurevisions mais on peut utiliser n'importe quelle palette ici
matrice %>%   height_shade(texture = grDevices::colorRampPalette(futurevisions::futurevisions('earth'))(256)) %>% 
 
  # Il faut encore jouer avec le zscale pour obtenir un résultat intéressant
  add_shadow(ray_shade(matrice, zscale = 2), .3) %>%
  
  # Il faut encore jouer avec le zscale pour obtenir un résultat intéressant, les autres paramèetres sont pour la caméra
    plot_3d(matrice, windowsize = c(1200,1200),
          zscale = 4,
          zoom = .9,
          phi = 75,
          theta =0,
          fov = 0 )

# On enregistre, plus on a de sample plus l'image sera nette (mais plus ça prend de temps)
render_highquality('abitibiouest.png',  samples = 100, width = 3000, height = 3000 )