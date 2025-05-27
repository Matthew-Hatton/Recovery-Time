
# Pull a time series from the monthly data files extracted from NEMO - ERSEM on the idrive
# readRDS("./Objects/Months/.")  # Marker so network script can see where the data is being saved too, it's buried in a function

#### Set up ####

rm(list=ls())                                                                   # Wipe the brain

packages <- c("tidyverse", "nemoRsem", "furrr", "tictoc")                       # List packages
lapply(packages, library, character.only = TRUE)                                # Load packages
plan(multisession)                                                              # Instructions for parallel processing

#### Extract time series ####

tic("Creating time series by compartment")                                      # Time the data extraction

Months <- list.files("./Objects/NE_Months/East_Greenland/", full.names = T) %>%                # Get list of NEMO-MEDUSA files
  future_map(NE_volume_summary,                                                 # Read in the months and calculate mean compartments
             ice = FALSE,
             .progress = T) %>%      
  data.table::rbindlist() %>%                                                   # Combine timesteps into series
  select(-c(x_avg, y_avg, longitude_avg, latitude_avg, Bathymetry_avg, weights_avg))                                                                  # Drop uneccessary columns

TS <- list.files("./Objects/NE_Days/East_Greenland/", full.names = T) %>%                      # Get list of NEMO-MEDUSA files
  future_map(NE_volume_summary,                                                 # Read in the months and calculate mean compartments
             ice = FALSE,
             .progress = T) %>%      
  data.table::rbindlist() %>%                                                   # Combine timesteps into series
  left_join(Months, by = c("Month", "Year", "Forcing", "SSP", "slab_layer", "Shore")) %>%  # Add in separately extracted temperature data
  select(-c(x_avg, y_avg, longitude_avg, latitude_avg, Bathymetry_avg, weights_avg)) %>%                                                                  # Drop uneccessary columns
  mutate(date = as.Date(paste("15", Month, Year, sep = "/"), format = "%d/%m/%Y"), # Create a single date column for plotting
         Compartment = paste(Shore, slab_layer, sep = " ")) %>%                 # Build a single compartment column for plotting by
  filter(Compartment != "Inshore D")                                            # A non-existant combination gets introduced when extracting data because the GEBCO and NM bathymetries differ

saveRDS(TS, "./Objects/TS_East_Greenland.rds")                                                 # Save out time series in the folder above
toc()                                                                           # Stop timing

