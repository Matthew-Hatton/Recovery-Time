rm(list=ls())                                                                 # Wipe the brain

library(MiMeMo.tools)                              
library(furrr)
plan(multisession)                                                            # Instructions for parallel processing

#### Extract time series ####

tic("Creating time series by compartment")                                    # Time the data extraction

TS <- list.files("../Objects/NM_Months/", full.names = T) %>%                     # Get list of NEMO-MEDUSA files
  future_map(NM_volume_summary,                                               # Read in the months and calculate mean compartments
             ice_threshold = 0.15,                                            # Treating pixels with ice concentrations below 15% as ice_free
             .progress = T) %>%      
  data.table::rbindlist() %>%                                                 # Combine timesteps into series
  mutate(date = as.Date(paste("15", Month, Year, sep = "/"), format = "%d/%m/%Y"), # Create a single date column for plotting
         Compartment = paste(Shore, slab_layer, sep = " ")) %>%               # Build a single compartment column for plotting by
  filter(Compartment != "Inshore D") %>%                                      # A non-existant combination gets introduced when extracting data because the GEBCO and NM bathymetries differ
  saveRDS("../Objects/NM BS TS.rds")                                                   # Save out time series in the folder above
toc()   