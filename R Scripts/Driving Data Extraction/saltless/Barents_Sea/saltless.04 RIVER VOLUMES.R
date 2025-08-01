
#### Set up ####

rm(list=ls())                                                               # Wipe the brain

packages <- c("tidyverse", "sf", "furrr", "raster", "data.table")           # List packages
lapply(packages, library, character.only = TRUE)                            # Load packages
source("./Objects/@_Region file.R")                                       # Define project region 

plan(multisession)                                                          # Choose the method to paralelise by with furrr

all_files <- list.files("../Shared data/Rivers/", recursive = TRUE, full.names = TRUE, pattern = ".nc") 

domains <- readRDS("./Objects/Domains.rds")                                 # Import domain polygon

#### Check domain overlaps point estimates of river runoff ####

example <- raster(all_files[1], varname = "nav_lat") %>%                    # Get an example set of points to plot

  as.data.frame(xy = TRUE) %>%
  left_join(as.data.frame(raster(all_files[1], varname = "nav_lon"), xy = TRUE)) %>%
  left_join(as.data.frame(raster(all_files[1], varname = "sorunoff"), xy = TRUE)) %>%
  st_as_sf(coords = c("nav_lon", "nav_lat"), crs = 4326) %>%
  dplyr::select(-c(x,y)) %>%
  drop_na()
  
ggplot() +
   geom_sf(data = river_expansion, fill = "orange", colour = "orange") +    # Uncomment me once you've built your extra polygons.
   geom_sf(data = domains, fill = "red", colour = "red") +
   geom_sf(data = st_transform(example, crs = crs), aes(colour = runoff_land_into_ocean), size = 0.2) +
   theme_minimal() +
   zoom +
   labs(caption = "Check domain overlaps point estimates of river runoff") 

ggsave_map("./Figures/saltless/check.04.1.png", plot = last_plot())

#### Create larger domain polygon ####

domains <- readRDS("./Objects/Domains.rds") %>%                             # Import original domain polygon
  st_union() %>%                                                            # Join inshore and offshore zone
  st_union(river_expansion) %>%                                             # Expand the polygon to catch distributed river run off 
  nngeo::st_remove_holes() %>%                                              # Remove holes
  st_sf() %>%                                                               # reinstate class
  mutate(Keep = T) %>%    
  st_make_valid()

#### Work out which pixels are in the model domain ####

coords <- raster(all_files[1], varname = "nav_lat") %>%
  as.data.frame(xy = TRUE) %>%                                              # Get a dataframe of xy positions and latitudes
  dplyr::left_join(as.data.frame(raster(all_files[1], varname = "nav_lon"), xy = TRUE)) %>% # Bind to the same for longitudes
  dplyr::left_join(as.data.frame(area(raster(all_files[1], varname = "nav_lon"), na.rm = F, weights = F), xy = TRUE)) %>% # Bind to the same for longitudes
  st_as_sf(coords = c("nav_lon", "nav_lat"), crs = 4326) %>%                # Convert to sf
  st_transform(sf::st_crs(domains)) %>%                                     # Transform points to the same crs as domain polygon
  st_join(domains) %>%                                                      # Check which points are in the domain
  drop_na()                                                                 # Drop points outside the domain

area <- st_union(coords) %>%                                                # Combine              
  st_voronoi() %>%                                                          # And create a voronoi tesselation
  st_collection_extract(type = "POLYGON") %>%                               # Expose the polygons
  st_intersection(domains) %>% 
  sf::st_sf() %>%                                                           # Reinstate sf formatting
  st_join(coords) %>%                                                       # Rejoin meta-data from points
  arrange(x, y) %>%                                                         # Order the polygons to match the points
  mutate(Area_m2 = as.numeric(st_area(.)))  

ggplot(area) +                                                              # Check the polygons match correctly with points
  geom_sf(aes(fill = Area_m2), size = 0.05, colour = "white") +
  geom_sf(data = domains, colour = "orange", fill = NA) +
  zoom +
  theme_minimal() +
  labs(caption = "Check if the grid looks regular") +
  NULL
ggsave_map("./Figures/saltless/check.04.2.png", plot = last_plot())

ggplot(area) +
  geom_density(aes(Area_m2)) +
  geom_vline(aes(xintercept = mean(Area_m2))) +
  theme_minimal() +
  labs(caption = "Check the mean captures the centre of the distribution")

ggsave("./Figures/saltless/check.04.3.png", plot = last_plot())

coords <- st_drop_geometry(coords) %>%                                      # Drop sf formatting
  mutate(Area = mean(area$Area_m2)) %>%                                     # Work out the area of each pixel (in m^2)
  dplyr::select(-c(Keep, layer))  

#### Extract data ####

rivers <- future_map(all_files, ~{
  data <- brick(.x, varname = "sorunoff") %>%                               # Extract river runoff
  as.data.frame(na.rm = T, xy = T) %>%                                      # Convert to XY dataframe of non-empty pixels
  pivot_longer(starts_with("X9."), names_to = "Month", values_to = "Runoff") %>% # Collect all months into one column
    mutate(Month = str_replace(Month, "X9.96920996838687e.36.", ""),        # Clean character string indicating month
           Year = str_sub(.x, start = -7, end = -4))                        # Extract Year from file name
}, .progress = TRUE) %>%                        
  data.table::rbindlist()                                                   # Bind

#### Summarise ####

setDT(rivers, key = c("x", "y"))                                            #  Set to data.table for quick filtering by pixel
setDT(coords, key = c("x", "y"))

summary <- rivers[coords,][,                                                # Summarise pixels in the model domain
  .(Volume = sum(Runoff*Area, na.rm = T)),                                  # By totaling Kg of water by pixel area 
  by = .(Month, Year)] %>%                                                  # By time step
  mutate(Runoff = (Volume*86400)/1000) %>%                                  # scaled to daily from per seconds, and Kg to m^3 
  dplyr::select(-Volume) %>% 
  drop_na()                                                                 # Drop pixels which didn't contribute water for some time steps

saveRDS(summary, "./Objects/River volume input.rds")

#### Plot ####

ggplot(data = summary %>% 
         mutate(Date = as.Date(paste("01", Month, Year, sep = "/"), format = "%d/%m/%Y"))) + 
  geom_area(aes(x = Date, y = Runoff), fill = "blue") +
  theme_minimal() +
  labs(y = expression("Freshwater input (M"^{3}*".D"^{-1}*")"), 
       subtitle = "NEMO-MEDUSA input of freshwater discharge from rivers into model domain",
       caption = "Did we generate a time series?") +
  NULL

ggsave("./Figures/saltless/check.04.4.png", dpi = 500, width = 18, height = 10 , units = "cm")
