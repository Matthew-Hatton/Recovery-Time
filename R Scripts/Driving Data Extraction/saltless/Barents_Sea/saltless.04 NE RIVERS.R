
# Files contain daily mean of freshwater discharge and discharge of biogeochemical tracers

#### Set up ####

rm(list=ls())                                                               # Wipe the brain

packages <- c("tidyverse", "sf", "furrr", "terra", "ncdf4", "data.table", "tictoc") # List packages
lapply(packages, library, character.only = TRUE)                            # Load packages
source("./Objects/@_Region file.R")                                       # Define project region 
sf_use_s2(FALSE)

plan(multisession)                                                          # Choose the method to paralelise by with furrr

all_files <- list.files("I:/Science/MS-Marine/MA/Hindcast/rivers/", recursive = TRUE, full.names = TRUE, pattern = "rivers_") %>%
  as.data.frame() %>%                                                           # Turn the vector into a dataframe/tibble
  rename(value = 1) %>% 
  separate(value, into = c(NA, "Year"), 
           remove = FALSE, sep = "_y") %>%                                      # Extract the year from the file name
  mutate(Year = str_sub(Year, end = -4)) %>%                                    # Drop file extension to get number
  rename(File = "value")

domains <- readRDS("./Objects/Domains.rds")                                  # Import domain polygon

raw <- nc_open("I:/Science/MS-Marine/MA/Hindcast/rivers/domain_cfg_zps.closea.compressed.nc")

lat <- ncvar_get(raw, "nav_lat") %>% 
  reshape2::melt(varnames = c("x", "y"), value.name =  "Latitude")

lon <- ncvar_get(raw, "nav_lon") %>% 
  reshape2::melt(varnames = c("x", "y"), value.name =  "Longitude")

width <- ncvar_get(raw, "e1t") %>% 
  reshape2::melt(varnames = c("x", "y"), value.name =  "Width")

height <- ncvar_get(raw, "e2t") %>% 
  reshape2::melt(varnames = c("x", "y"), value.name =  "Height")

nc_close(raw)

grid <- left_join(lat, lon) %>% 
  mutate(Area = width$Width * height$Height)                                    # First guess this is in m, so square ms once multiplied.

# look <- nc_open(all_files[3,1])
# nc_close(look)
# 
# look_new <- nc_open("I:/Science/MS/Shared/MA/")
# nc_close(look_new)


#### Check domain overlaps point estimates of river runoff ####

buffer <- domains %>% 
  st_transform(crs = 3035) %>% 
  st_buffer(dist = 50000) %>% 
  st_transform(crs = 4326) %>% 
  st_union() %>% 
  st_sf(Domain = TRUE)

example <- rast(all_files[1,1], subds = "rorunoff", lyrs = 1) %>%                         # Get an example set of points to plot
  as.data.frame(xy = TRUE, na.rm = FALSE) %>%                                    # Drop empty cells
  left_join(grid) %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>% 
  st_join(buffer) %>% 
  drop_na(Domain)
  
ggplot() +
#   geom_sf(data = river_expansion, fill = "orange", colour = "orange") +    # Uncomment me once you've built your extra polygons.
   geom_sf(data = buffer, fill = "red", colour = "red") +
   geom_sf(data = example, aes(colour = rorunoff_1), size = 0.2) +
   theme_minimal() +
   labs(caption = "Check domain overlaps point estimates of river runoff") 

ggsave_map("./Figures/saltless/check.04.1.png", plot = last_plot())

#### Create larger domain polygon ####

domains <- readRDS("./Objects/Domains.rds") %>%                             # Import original domain polygon
  st_transform(crs = 3035) %>% 
  st_make_valid() %>% 
  st_union() %>%                                                            # Join inshore and offshore zone
#  st_union(river_expansion) %>%                                             # Expand the polygon to catch distributed river run off 
#  nngeo::st_remove_holes() %>%                                              # Remove holes
  st_sf() %>%                                                               # reinstate class
  mutate(Keep = T) %>%    
  st_make_valid() %>% 
  st_buffer(50000) %>%                                                  # A buffer was easier for the Norwegian basin than a polygon extension
  st_transform(crs = 4326)
#### Work out which pixels are in the model domain ####

coords <- example %>% 
  st_transform(sf::st_crs(domains)) %>%                                     # Transform points to the same crs as domain polygon
  st_join(domains) %>%                                                      # Check which points are in the domain
  drop_na(Keep) %>%                                                             # Drop points outside the domain
  dplyr::select(x, y, Area)

#### Extract data ####

setDT(coords, key = c("x", "y"))                                            #  Set to data.table for quick filtering by pixel

all_files <- all_files %>% 
  filter(Year >= 2010)
tic()
rivers <- map(all_files$File, ~{

test <- all_files$File[1]

  runoff <- rast(.x, subds = "rorunoff") %>%                                    # Extract river runoff
    as.data.frame(na.rm = T, xy = T) %>%                                        # Convert to XY dataframe of non-empty pixels
    pivot_longer(starts_with("rorunoff"), names_to = "Day", values_to = "Runoff") %>% # Collect all timesteps into one column
    mutate(Day = str_replace(Day, "rorunoff_", ""),                             # Clean character string indicating timestep
           Year = str_sub(.x, start = -7, end = -4))                            # Extract Year from file name

  area <- rast(.x, subds = "dA") %>%                                    # Extract river runoff
    as.data.frame(na.rm = T, xy = T) %>%                                        # Convert to XY dataframe of non-empty pixels
    mutate(Year = str_sub(.x, start = -7, end = -4))                            # Extract Year from file name

  nh4 <- rast(.x, subds = "ronh4") %>%                                          # Extract river runoff
    as.data.frame(na.rm = T, xy = T) %>%                                        # Convert to XY dataframe of non-empty pixels
    pivot_longer(starts_with("ronh4"), names_to = "Day", values_to = "NH4") %>% # Collect all timesteps into one column
    mutate(Day = str_replace(Day, "ronh4_", ""),                                # Clean character string indicating timestep
           Year = str_sub(.x, start = -7, end = -4))                            # Extract Year from file name
  
  no3 <- rast(.x, subds = "rono3") %>%                                          # Extract river runoff
    as.data.frame(na.rm = T, xy = T) %>%                                        # Convert to XY dataframe of non-empty pixels
    pivot_longer(starts_with("rono3"), names_to = "Day", values_to = "NO3") %>% # Collect all timesteps into one column
    mutate(Day = str_replace(Day, "rono3_", ""),                                # Clean character string indicating timestep
           Year = str_sub(.x, start = -7, end = -4))                            # Extract Year from file name
  
  setDT(no3, key = c("x", "y", "Year", "Day"))                                          #  Set to data.table for quick filtering by pixel
  setDT(nh4, key = c("x", "y", "Year", "Day"))                                          #  Set to data.table for quick filtering by pixel
  setDT(runoff, key = c("x", "y", "Year", "Day"))                                       #  Set to data.table for quick filtering by pixel
  setDT(area, key = c("x", "y", "Year"))                                                #  Set to data.table for quick filtering by pixel
  
  data <- no3[nh4][runoff][area]                                                  # Combine variables

  ## moved the summary step in here to reduce memory footprint

  summary <- data[coords,][,                                                    # Summarise pixels in the model domain
      .(Volume = sum(Runoff*dA, na.rm = T),                                     # By totaling Kg of water by pixel area
      NH4 = sum(NH4, na.rm = T),                                                # And totalling G per second of Nitrogen
      NO3 = sum(NO3, na.rm = T)),
    by = .(Day, Year)] %>%                                                      # By time step
    mutate(Runoff = (Volume*86400)/1e3) %>%                                     # scaled to daily from per seconds, and Kg to m^3
    mutate(NH4 = NH4*86400/Runoff,                                              # Scaled to daily, and then per unit water to get a concentration
           NO3 = NO3*86400/Runoff) %>%                                          # So grams of Nitrogen per m^3
    mutate(NH4 = (NH4/14.0067) * 1e3,                                           # Now go from grams of Nitrogen to mols, then millimols
           NO3 = (NO3/14.0067) * 1e3) %>%                                       # Concentrations are needed as m^3 so that doesn't need rescaling.
  dplyr::select(-Volume) %>%
    drop_na()                                                                   # Drop pixels which didn't contribute water for some time steps
  
  }) %>%                        
  data.table::rbindlist() %>%                                                   # Bind each year,
  mutate(Date = as.Date(as.numeric(Day)-1, origin = str_glue("{Year}-01-01")))  # Add a date column
toc() # 45 minutes

saveRDS(rivers, "./Objects/NE River input.rds")

#### Plot ####

ggplot(data = rivers) +
  geom_area(aes(x = Date, y = Runoff), fill = "blue") +
  theme_minimal() +
  labs(y = expression("Freshwater input (M"^{3}*".D"^{-1}*")"), 
       subtitle = "NEMO-ERSEM input of freshwater discharge from rivers into model domain",
       caption = "Did we generate a time series?") +
  NULL

ggsave("./Figures/saltless/check.04.4.png", dpi = 500, width = 18, height = 10 , units = "cm", bg = "white")

ggplot(data = rivers) +
  geom_area(aes(x = Date, y = NH4), fill = "darkgreen") +
  theme_minimal() +
  labs(y = expression("Ammonium concentration (mmolN.M"^{-3}*")"), 
       subtitle = "NEMO-ERSEM nutrient concentration of freshwater discharge into model domain",
       caption = "Did we generate a time series?") +
  NULL

ggsave("./Figures/saltless/check.04.5.png", dpi = 500, width = 18, height = 10 , units = "cm", bg = "white")

ggplot(data = rivers) +
  geom_area(aes(x = Date, y = NO3), fill = "darkgreen") +
  theme_minimal() +
  labs(y = expression("Nitrate concentration (mmolN.M"^{-3}*")"), 
       subtitle = "NEMO-ERSEM nutrient concentration of freshwater discharge into model domain",
       caption = "Did we generate a time series?") +
  NULL

ggsave("./Figures/saltless/check.04.6.png", dpi = 500, width = 18, height = 10 , units = "cm", bg = "white")


# ##test##
# test <- all_files$File[1]
# runoff <- rast(test, subds = "rorunoff") %>%                                    # Extract river runoff
#   as.data.frame(na.rm = T, xy = T) %>%                                        # Convert to XY dataframe of non-empty pixels
#   pivot_longer(starts_with("rorunoff"), names_to = "Day", values_to = "Runoff") %>% # Collect all timesteps into one column
#   mutate(Day = str_replace(Day, "rorunoff_", ""),                             # Clean character string indicating timestep
#          Year = str_sub(test, start = -7, end = -4))                            # Extract Year from file name
# 
# area <- rast(test, subds = "dA") %>%                                    # Extract river runoff
#   as.data.frame(na.rm = T, xy = T) %>%                                        # Convert to XY dataframe of non-empty pixels
#   mutate(Year = str_sub(test, start = -7, end = -4))                            # Extract Year from file name
# 
# nh4 <- rast(test, subds = "ronh4") %>%                                          # Extract river runoff
#   as.data.frame(na.rm = T, xy = T) %>%                                        # Convert to XY dataframe of non-empty pixels
#   pivot_longer(starts_with("ronh4"), names_to = "Day", values_to = "NH4") %>% # Collect all timesteps into one column
#   mutate(Day = str_replace(Day, "ronh4_", ""),                                # Clean character string indicating timestep
#          Year = str_sub(test, start = -7, end = -4))                            # Extract Year from file name
# 
# no3 <- rast(test, subds = "rono3") %>%                                          # Extract river runoff
#   as.data.frame(na.rm = T, xy = T) %>%                                        # Convert to XY dataframe of non-empty pixels
#   pivot_longer(starts_with("rono3"), names_to = "Day", values_to = "NO3") %>% # Collect all timesteps into one column
#   mutate(Day = str_replace(Day, "rono3_", ""),                                # Clean character string indicating timestep
#          Year = str_sub(test, start = -7, end = -4))                            # Extract Year from file name
# 
# setDT(no3, key = c("x", "y", "Year", "Day"))                                          #  Set to data.table for quick filtering by pixel
# setDT(nh4, key = c("x", "y", "Year", "Day"))                                          #  Set to data.table for quick filtering by pixel
# setDT(runoff, key = c("x", "y", "Year", "Day"))                                       #  Set to data.table for quick filtering by pixel
# setDT(area, key = c("x", "y", "Year"))                                                #  Set to data.table for quick filtering by pixel
# 
# data <- no3[nh4][runoff][area]
# 
# summary <- data[coords,][,                                                    # Summarise pixels in the model domain
#                          .(Volume = sum(Runoff*dA, na.rm = T),                                     # By totaling Kg of water by pixel area
#                            NH4 = sum(NH4, na.rm = T),                                                # And totalling G per second of Nitrogen
#                            NO3 = sum(NO3, na.rm = T)),
#                          by = .(Day, Year)] %>%                                                      # By time step
#   mutate(Runoff = (Volume*86400)/1e3) %>%                                     # scaled to daily from per seconds, and Kg to m^3
#   mutate(NH4 = NH4*86400/Runoff,                                              # Scaled to daily, and then per unit water to get a concentration
#          NO3 = NO3*86400/Runoff) %>%                                          # So grams of Nitrogen per m^3
#   mutate(NH4 = (NH4/14.0067) * 1e3,                                           # Now go from grams of Nitrogen to mols, then millimols
#          NO3 = (NO3/14.0067) * 1e3) %>%                                       # Concentrations are needed as m^3 so that doesn't need rescaling.
#   dplyr::select(-Volume) %>%
#   drop_na()
