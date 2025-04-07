## Set repeated commands specific to the project region
## This version is parameterised for the east Greenland shelf

library(sf)

crs <- 3035                                                              # Specify the map projection for the project
SDepth <- 60
DDepth <- 400
lims <- c(xmin = 2900000, xmax = 4100000, ymin = 5250000, ymax = 6750000)# Specify limits of plotting window, also used to clip data grids

zoom <- coord_sf(xlim = c(lims[["xmin"]], lims[["xmax"]]), ylim = c(lims[["ymin"]], lims[["ymax"]])) # Specify the plotting window for SF maps in this region

ggsave_map <- function(filename, plot) {
  ggsave(filename, plot, scale = 1, width = 11, height = 10, units = "cm", dpi = 500)
  
}                             # Set a new default for saving maps in the correct size
pre <- list(scale = 1, width = 12, height = 10, units = "cm", dpi = 500) # The same settings if you need to pass them to a function in MiMeMo.tools

#### bathymetry.5 MODEL DOMAIN ####

shape <- function(matrix) {
  
  shape <-  matrix %>% 
    list() %>% 
    st_polygon() %>% 
    st_sfc() %>% 
    st_sf(Region = "Barents Sea",.)
  st_crs(shape) <- st_crs(4326)                                        
  shape <- st_transform(shape, crs = crs)
  return(shape)
  
}                      # Convert a matrix of lat-lons to an sf polygon

Region_mask <- matrix(c(-30, 69.9,
                        -18, 70,
                        0, 75,
                        0, 80,
                        -8, 81.6,
                        -18, 81.3,
                        -30, 69.9),
                      ncol = 2, byrow = T) %>% 
  list() %>% 
  st_polygon() %>% 
  st_sfc() %>% 
  st_sf(Region = "Greenland",.)
st_crs(Region_mask) <- st_crs(4326)                                          
Region_mask <- st_transform(Region_mask, crs = crs)

#### bounds.2 MAKE TRANSECTS ####

## Polygons to mark which transects are along the open ocean-inshore boundary

Inshore_Ocean1 <- matrix(c(-8, -8, -18, -18, -8,   # Longitudes
                           81.7, 81.5, 81.5, 81.7, 81.7), ncol = 2, byrow = F) %>% 
  shape()

Inshore_Ocean2 <- matrix(c(-23, -23,  -18, -18, -23,            # Longitudes
                           69.9, 70, 70, 69.9, 69.9), ncol = 2, byrow = F) %>% 
  shape()


Inshore_ocean_boundaries <- rbind(Inshore_Ocean1, Inshore_Ocean2)

rm(Inshore_Ocean1, Inshore_Ocean2)

#### expand polygon for sampling rivers ####

# Not needed for GL

# river_expansion <- matrix(c(13, 73,
#                             0, 80,
#                             0, 85,
#                             63, 85,
#                             73, 77,
#                             30, 71,
#                             13, 73),
#                           ncol = 2, byrow = T) %>% 
#   list() %>% 
#   st_polygon() %>% 
#   st_sfc() %>% 
#   st_sf(Region = "Greenland",.)
# st_crs(river_expansion) <- st_crs(4326)                                          
# river_expansion <- st_transform(river_expansion, crs = 3035)