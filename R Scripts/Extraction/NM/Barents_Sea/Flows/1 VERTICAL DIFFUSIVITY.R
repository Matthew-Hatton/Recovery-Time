
# Summarise the data extracted from NEMO-MEDUSA, dealing with deep convection issues
# readRDS("./Objects/vertical boundary/.")  # Marker so network script can see where the data is being pulled from

#### Setup ####

rm(list=ls())                                                                   # Wipe the brain

Packages <- c("tidyverse", "data.table", "furrr")                               # List packages
lapply(Packages, library, character.only = TRUE)                                # Load packages

plan(multisession)

deep_convection_is <- 0.14                                                      # Threshold above which vertical diffusivity = deep convection

#### Quantify the amount of deep convection ####

## For more discussion see the appropriate entry in ./Notes

total_mixing <- list.files("../Objects/Barents_Sea/NM/vertical boundary/", full.names = T) %>%  # Import
  future_map(readRDS) %>% 
  rbindlist() %>% 
  group_by(Month) %>%                                                                   
  summarise(Deep_convection_proportion = mean(Vertical_diffusivity > deep_convection_is)) # What proportion of values are deep convection?

ggplot(total_mixing) +
  geom_line(aes(x = Month, y = Deep_convection_proportion)) +
  theme_minimal() +
  ylim(0,1) +
  labs(y = "Proportion of model domain as deep convection")

#### Mean vertical diffusivity ignoring deep convection ####

normal_mixing <- list.files("../Objects/Barents_Sea/NM/vertical boundary/", full.names = T) %>% # Import data
  future_map(readRDS) %>% 
  rbindlist() %>% 
  dplyr::select(Vertical_diffusivity, Year, Month) %>%                                 # Discard excess variables
  filter(Vertical_diffusivity < deep_convection_is) %>%                         # Remove deep convection
  group_by(Year, Month) %>%                                                     # Create a monthly time series
  summarise(Vertical_diffusivity = mean(Vertical_diffusivity, na.rm = T)) %>% 
  ungroup()

saveRDS(normal_mixing, "../Objects/Barents_Sea/NM/vertical diffusivity.rds")
