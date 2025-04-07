## Script to format NE TS data into a usable form for exploring transient system dynamics in StrathE2EPolar for the East Greenland

rm(list = ls()) # Start again
library(tidyverse)

bs_ts <- readRDS("./Objects/TS_East_Greenland.rds") %>%  # Load Barents Sea time series
  split(.$Forcing) # Splits TS data by Forcing

# Split between forcings
bs_ts_CNRM <- bs_ts[[1]]

# Split between SSP's
hist_CNRM <- bs_ts_CNRM %>% filter(SSP == "hist")

#SSP 126
ssp126_CNRM <- bs_ts_CNRM %>% filter(SSP == "ssp126")
ssp126_CNRM <- rbind(hist_CNRM,ssp126_CNRM) %>% split(.$Year)

physics_ssp126_CNRM_list <- list()
# We need one value per month for all variables in each compartment (SI,SO,D)
for (i in 1:length(ssp126_CNRM)) {
  yearly <- ssp126_CNRM[[i]]
  physics <- data.frame(
    SO_temp = na.omit(ifelse(yearly$Shore == "Offshore" & yearly$slab_layer == "S", yearly$Temperature_avg, NA)),
    D_temp  = na.omit(ifelse(yearly$Shore == "Offshore" & yearly$slab_layer == "D", yearly$Temperature_avg, NA)),
    SI_temp = na.omit(ifelse(yearly$Shore == "Inshore" & yearly$slab_layer == "S", yearly$Temperature_avg, NA))
  ) %>% mutate(month = seq(1,12),
               year = unique(yearly$Year))
  physics_ssp126_CNRM_list[[i]] <- physics
}

saveRDS(physics_ssp126_CNRM_list,"./Objects/Transient/East_Greenland/CNRM/physics_SSP126_CNRM.rds")

#SSP 370
ssp370_CNRM <- bs_ts_CNRM %>% filter(SSP == "ssp370")
ssp370_CNRM <- rbind(hist_CNRM,ssp370_CNRM) %>% split(.$Year)

physics_ssp370_CNRM_list <- list()
# We need one value per month for all variables in each compartment (SI,SO,D)
for (i in 1:length(ssp370_CNRM)) {
  yearly <- ssp370_CNRM[[i]]
  physics <- data.frame(
    SO_temp = na.omit(ifelse(yearly$Shore == "Offshore" & yearly$slab_layer == "S", yearly$Temperature_avg, NA)),
    D_temp  = na.omit(ifelse(yearly$Shore == "Offshore" & yearly$slab_layer == "D", yearly$Temperature_avg, NA)),
    SI_temp = na.omit(ifelse(yearly$Shore == "Inshore" & yearly$slab_layer == "S", yearly$Temperature_avg, NA))
  ) %>% mutate(month = seq(1,12),
               year = unique(yearly$Year))
  physics_ssp370_CNRM_list[[i]] <- physics
}
saveRDS(physics_ssp370_CNRM_list,"./Objects/Transient/East_Greenland/CNRM/physics_SSP370_CNRM.rds")
