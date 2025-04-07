
## Visualise summarised nemo-medusa model output

#### Set up ####

rm(list=ls(all.names = TRUE))                                               # Wipe the brain

packages <- c("tidyverse", "nemoRsem", "furrr", "tictoc")                       # List packages
lapply(packages, library, character.only = TRUE)                                # Load packages
plan(multisession)                                                          # Instructions for parallel processing

TS <- readRDS("./Objects/TS_East_Greenland.rds") #%>% 
#  filter(Year < 2051)

ggplot() +
  #geom_smooth(data = TS,aes(x = date,y = Temperature_avg)) +
  geom_line(data = TS,aes(x = date,y = Temperature_avg,color = SSP)) +
  facet_wrap(~Compartment,ncol = 1) +
  labs(title = "East_Greenland Temperature avg") +
  NULL
