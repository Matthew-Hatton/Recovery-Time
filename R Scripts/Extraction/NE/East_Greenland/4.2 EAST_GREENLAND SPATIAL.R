
# Average the data pulled from NEMO - MEDUSA for creating decadal maps (this is just for plotting, not for data extraction!)
# readRDS("./Objects/Months/.")  # Marker so network script can see where the data is being saved too, it's buried in a function

#### Set up ####

rm(list=ls())                                                               # Wipe the brain

packages <- c("tidyverse", "nemoRsem", "furrr", "tictoc")                                 # List packages
lapply(packages, library, character.only = TRUE)                            # Load packages
plan(multisession)                                                          # Instructions for parallel processing

##!! drop cells that are bathymetyry = 0.
## Keep bathymetry so I can look at the plot.

#### Average by decade spatially ####

Months <- list.files("./Objects/NE_Months/East_Greenland/", full.names = T) %>%            # Get list of NE files
  future_map(decadal, .progress = TRUE) %>%                                 # Read in data, remove unnecessary columns and create a decade column
  data.table::rbindlist() %>%                                               # Combine dataframes
  mutate(Decade = as.factor(Decade)) %>%                                    # Change decade to factor     
  split(., f = list(paste(.$Decade, .$slab_layer, .$Forcing, .$SSP))) %>%          # Split into a large dataframe per decade (and depth to help plotting)
  lapply(NE_decadal_summary, dt = T) %>%                                    # Average the variables per decade, dt method is faster
  lapply(select, c("x", "y", "Month", "Temperature"))     # Make life easier for the eventual left_join (some imperfect matching in lat/lon)

Days <- list.files("./Objects/NE_Days/East_Greenland/", full.names = T) %>%                # Get list of NE files
  future_map(decadal, .progress = TRUE) %>%                                 # Read in data, remove unnecessary columns and create a decade column
  data.table::rbindlist() %>% 
  mutate(Decade = as.factor(Decade),                                        # Change decade to factor
         Speed = vectors_2_direction(Zonal, Meridional)[,"uvSpeed"]) %>%      # Convert currents to speed     
  split(., f = list(paste(.$Decade, .$slab_layer, .$Forcing, .$SSP))) %>%          # Split into a large dataframe per decade (and depth to help plotting)
  lapply(NE_decadal_summary, dt = T)                                        # Average the variables per decade, dt method is faster

SP <- future_map2(Months, Days, left_join, by = c("x", "y", "Month")) 

saveRDS(SP, "./Objects/SPATIAL_East_Greenland.rds")                                          # Save out spatial file

