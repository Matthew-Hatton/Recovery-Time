#Average the data pulled from NEMO-MEDUSA for creating decadal maps

####Set Up####

rm(list = ls())
library(MiMeMo.tools)
library(furrr)
plan(multisession)

#### Average by decade spatially ####
tic()

SP <- list.files("../Objects/NM_Months",full.names = T) %>% 
  future_map(decadal, .progress = TRUE) %>% #read in data and create decade column
  data.table::rbindlist() %>% #combine df's
  mutate(Decade = as.factor(Decade), #change decade to factor
         Speed = vectors_2_direction(Zonal,Meridional)[,"uvSpeed"]) %>%  #converts currents to speed
  split(.,f = list(.$Decade,.$slab_layer)) %>% 
  lapply(strip_ice,dt=T) %>% 
  lapply(NM_decadal_summary,dt=T) %>% 
  saveRDS("../Objects/NM SPATIAL BS.rds")

toc()

