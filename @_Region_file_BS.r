## Set repeated commands specific to the project region
## This version is parameterised for the Barents sea

library(sf)

crs <- 3035                                                              # Specify the map projection for the project
SDepth <- 60
DDepth <- 400
lims <- c(xmin = 4200000, xmax = 6200000, ymin = 5000000, ymax = 7000000)# Specify limits of plotting window, also used to clip data grids

zoom <- coord_sf(xlim = c(lims[["xmin"]], lims[["xmax"]]), ylim = c(lims[["ymin"]], lims[["ymax"]])) # Specify the plotting window for SF maps in this region

ggsave_map <- function(filename, plot) {
  ggsave(filename, plot, scale = 1, width = 12, height = 10, units = "cm", dpi = 500)
  
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

Region_mask <- matrix(c(16.23, 70,
                        20.25, 68.5,
                        41, 66.8,
                        45.3, 65.5,
                        64, 68, 
                        57.492431, 70.736206,
                        52.984071, 71.835129,
                        54.408132, 73.261126,
                        67.9, 76.7,
                        71, 80,
                        68, 83.5,
                        0, 80,
                        0, 75,
                        16.23, 70),
                      ncol = 2, byrow = T) %>% 
  list() %>% 
  st_polygon() %>% 
  st_sfc() %>% 
  st_sf(Region = "Barents Sea",.)
st_crs(Region_mask) <- st_crs(4326)                                        
Region_mask <- st_transform(Region_mask, crs = crs)


Greenland_mask <- matrix(c(-30, 69.9,
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
st_crs(Greenland_mask) <- st_crs(4326)                                          
Greenland_mask <- st_transform(Greenland_mask, crs = 3035)

#### bounds.2 MAKE TRANSECTS ####

## Polygons to mark which transects are along the open ocean-inshore boundary

Inshore_Ocean1 <- matrix(c(16.23, 20.25, 20.25, 16.23, 16.23,    # Longitudes
                           69.9, 68.4, 68.6, 70.1, 69.9), ncol = 2, byrow = F) %>% 
  shape()

Inshore_Ocean2 <- matrix(c(41, 43, 44.25, 41, 41,               # Longitudes
                           66.8, 67, 66, 66.4, 66.8), ncol = 2, byrow = F) %>% 
  shape()

Inshore_Ocean3 <- matrix(c(59.5, 55, 55, 59.5, 59.5,             # Longitudes
                           70.2, 71.5, 71.3, 70, 70.2), ncol = 2, byrow = F) %>% 
  shape()

Inshore_Ocean4 <- matrix(c(67.9, 71, 70, 66.9, 67.9,             # Longitudes
                           76.7, 77.7, 77.7, 76.7, 76.7), ncol = 2, byrow = F) %>% 
  shape()

Inshore_Ocean5 <- matrix(c(66.8, 67.1, 66.1, 65.8, 66.8,         # Longitudes
                           80.95, 81.3, 81.3, 80.95, 80.95), ncol = 2, byrow = F) %>% 
  shape()

Inshore_Ocean6 <- matrix(c(10.4, 10.7, 9.7, 9.4, 10.4,           # Longitudes
                           79.75, 80.05, 80.05, 79.75, 79.75), ncol = 2, byrow = F) %>% 
  shape()

Inshore_ocean_boundaries <- rbind(Inshore_Ocean1, Inshore_Ocean2, Inshore_Ocean3, Inshore_Ocean4, Inshore_Ocean5, Inshore_Ocean6)

rm(Inshore_Ocean1, Inshore_Ocean2, Inshore_Ocean3, Inshore_Ocean4, Inshore_Ocean5, Inshore_Ocean6)

#### expand polygon for sampling rivers ####

river_expansion <- matrix(c(13, 73,
                            0, 80,
                            0, 85,
                            63, 85,
                            73, 77,
                            30, 71,
                            13, 73),
                          ncol = 2, byrow = T) %>% 
  list() %>% 
  st_polygon() %>% 
  st_sfc() %>% 
  st_sf(Region = "Barents Sea",.)
st_crs(river_expansion) <- st_crs(4326)                                          
river_expansion <- st_transform(river_expansion, crs = 3035)
