## Script to extract chemistry data for StrathE2EPolar in the Barents_Sea

rm(list = ls()) # Start again
library(tidyverse)

bs_ts <- readRDS("./Objects/TS_Barents_Sea.rds") %>%  # Load Barents Sea time series
  split(.$Forcing) # Splits TS data by Forcing

# Split between forcings
bs_ts_GFDL <- bs_ts[[2]]

# Split between SSP's
hist_GFDL <- bs_ts_GFDL %>% filter(SSP == "hist")

#SSP 126
ssp126_GFDL <- bs_ts_GFDL %>% filter(SSP == "ssp126")
ssp126_GFDL <- rbind(hist_GFDL,ssp126_GFDL) %>% split(.$Year)

chemistry_ssp126_GFDL_list <- list()
# We need one value per month for all variables in each compartment (SI,SO,D)
for (i in 1:length(ssp126_GFDL)) {
  yearly <- ssp126_GFDL[[i]]
  chemistry <- data.frame(
    SO_nitrate = na.omit(ifelse(yearly$Shore == "Offshore" & yearly$slab_layer == "S", yearly$NO3_avg, NA)),
    SO_ammonia = na.omit(ifelse(yearly$Shore == "Offshore" & yearly$slab_layer == "S", yearly$NH4_avg, NA)),
    SO_phyt = na.omit(ifelse(yearly$Shore == "Offshore" & yearly$slab_layer == "S", yearly$Diatoms_avg + yearly$Other_phytoplankton_avg, NA)),
    SO_detritus = na.omit(ifelse(yearly$Shore == "Offshore" & yearly$slab_layer == "S", yearly$Detritus_avg, NA)),
    D_nitrate = na.omit(ifelse(yearly$Shore == "Offshore" & yearly$slab_layer == "D", yearly$NO3_avg, NA)),
    D_ammonia = na.omit(ifelse(yearly$Shore == "Offshore" & yearly$slab_layer == "D", yearly$NH4_avg, NA)),
    D_phyt = na.omit(ifelse(yearly$Shore == "Offshore" & yearly$slab_layer == "D", yearly$Diatoms_avg + yearly$Other_phytoplankton_avg, NA)),
    D_detritus = na.omit(ifelse(yearly$Shore == "Offshore" & yearly$slab_layer == "D", yearly$Detritus_avg, NA)),
    SI_nitrate = na.omit(ifelse(yearly$Shore == "Inshore" & yearly$slab_layer == "S", yearly$NO3_avg, NA)),
    SI_ammonia = na.omit(ifelse(yearly$Shore == "Inshore" & yearly$slab_layer == "S", yearly$NH4_avg, NA)),
    SI_phyt = na.omit(ifelse(yearly$Shore == "Inshore" & yearly$slab_layer == "S", yearly$Diatoms_avg + yearly$Other_phytoplankton_avg, NA)),
    SI_detritus = na.omit(ifelse(yearly$Shore == "Inshore" & yearly$slab_layer == "S", yearly$Detritus_avg, NA))
  ) %>% mutate(month = seq(1,12),
               year = unique(yearly$Year))
  chemistry_ssp126_GFDL_list[[i]] <- chemistry
}

saveRDS(chemistry_ssp126_GFDL_list,"./Objects/Transient/Barents_Sea/GFDL/chemistry_SSP126_GFDL.rds")

#SSP 370
ssp370_GFDL <- bs_ts_GFDL %>% filter(SSP == "ssp370")
ssp370_GFDL <- rbind(hist_GFDL,ssp370_GFDL) %>% split(.$Year)

chemistry_ssp370_GFDL_list <- list()
# We need one value per month for all variables in each compartment (SI,SO,D)
for (i in 1:length(ssp370_GFDL)) {
  yearly <- ssp370_GFDL[[i]]
  chemistry <- data.frame(
    SO_nitrate = na.omit(ifelse(yearly$Shore == "Offshore" & yearly$slab_layer == "S", yearly$NO3_avg, NA)),
    SO_ammonia = na.omit(ifelse(yearly$Shore == "Offshore" & yearly$slab_layer == "S", yearly$NH4_avg, NA)),
    SO_phyt = na.omit(ifelse(yearly$Shore == "Offshore" & yearly$slab_layer == "S", yearly$Diatoms_avg + yearly$Other_phytoplankton_avg, NA)),
    SO_detritus = na.omit(ifelse(yearly$Shore == "Offshore" & yearly$slab_layer == "S", yearly$Detritus_avg, NA)),
    D_nitrate = na.omit(ifelse(yearly$Shore == "Offshore" & yearly$slab_layer == "D", yearly$NO3_avg, NA)),
    D_ammonia = na.omit(ifelse(yearly$Shore == "Offshore" & yearly$slab_layer == "D", yearly$NH4_avg, NA)),
    D_phyt = na.omit(ifelse(yearly$Shore == "Offshore" & yearly$slab_layer == "D", yearly$Diatoms_avg + yearly$Other_phytoplankton_avg, NA)),
    D_detritus = na.omit(ifelse(yearly$Shore == "Offshore" & yearly$slab_layer == "D", yearly$Detritus_avg, NA)),
    SI_nitrate = na.omit(ifelse(yearly$Shore == "Inshore" & yearly$slab_layer == "S", yearly$NO3_avg, NA)),
    SI_ammonia = na.omit(ifelse(yearly$Shore == "Inshore" & yearly$slab_layer == "S", yearly$NH4_avg, NA)),
    SI_phyt = na.omit(ifelse(yearly$Shore == "Inshore" & yearly$slab_layer == "S", yearly$Diatoms_avg + yearly$Other_phytoplankton_avg, NA)),
    SI_detritus = na.omit(ifelse(yearly$Shore == "Inshore" & yearly$slab_layer == "S", yearly$Detritus_avg, NA))
  ) %>% mutate(month = seq(1,12),
               year = unique(yearly$Year))
  chemistry_ssp370_GFDL_list[[i]] <- chemistry
}

saveRDS(chemistry_ssp370_GFDL_list,"./Objects/Transient/Barents_Sea/GFDL/chemistry_SSP370_GFDL.rds")
