## Script to format NE TS data into a usable form for exploring transient system dynamics in StrathE2EPolar for East Greenland

rm(list = ls()) # Start again
library(tidyverse)

bs_ts <- readRDS("./Objects/TS_East_Greenland.rds") %>%  # Load Barents Sea time series
  split(.$Forcing) # Splits TS data by Forcing

# Split between forcings
bs_ts <- bs_ts[[2]]

# Split between SSP's
hist_GFDL <- bs_ts %>% filter(SSP == "hist")

#SSP 126
ssp126 <- bs_ts %>% filter(SSP == "ssp126")
ssp126 <- rbind(hist_GFDL,ssp126) %>% split(.$Year)

physics_ssp126_list <- list()
# We need one value per month for all variables in each compartment (SI,SO,D)
for (i in 1:length(ssp126)) {
  yearly <- ssp126[[i]]
  physics <- data.frame(
    SO_temp = na.omit(ifelse(yearly$Shore == "Offshore" & yearly$slab_layer == "S", yearly$Temperature_avg, NA)),
    D_temp  = na.omit(ifelse(yearly$Shore == "Offshore" & yearly$slab_layer == "D", yearly$Temperature_avg, NA)),
    SI_temp = na.omit(ifelse(yearly$Shore == "Inshore" & yearly$slab_layer == "S", yearly$Temperature_avg, NA))
  ) %>% mutate(month = seq(1,12),
               year = unique(yearly$Year))
  physics_ssp126_list[[i]] <- physics
}

saveRDS(physics_ssp126_list,"./Objects/Transient/East_Greenland/GFDL/physics_SSP126_GFDL.rds")

#SSP 370
ssp370 <- bs_ts %>% filter(SSP == "ssp370")
ssp370 <- rbind(hist_GFDL,ssp370) %>% split(.$Year)

physics_ssp370_list <- list()
# We need one value per month for all variables in each compartment (SI,SO,D)
for (i in 1:length(ssp370)) {
  yearly <- ssp370[[i]]
  physics <- data.frame(
    SO_temp = na.omit(ifelse(yearly$Shore == "Offshore" & yearly$slab_layer == "S", yearly$Temperature_avg, NA)),
    D_temp  = na.omit(ifelse(yearly$Shore == "Offshore" & yearly$slab_layer == "D", yearly$Temperature_avg, NA)),
    SI_temp = na.omit(ifelse(yearly$Shore == "Inshore" & yearly$slab_layer == "S", yearly$Temperature_avg, NA))
  ) %>% mutate(month = seq(1,12),
               year = unique(yearly$Year))
  physics_ssp370_list[[i]] <- physics
}
saveRDS(physics_ssp370_list,"./Objects/Transient/East_Greenland/GFDL/physics_SSP370_GFDL.rds")
