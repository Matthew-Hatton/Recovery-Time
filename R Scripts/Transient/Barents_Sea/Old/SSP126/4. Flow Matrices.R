## Using values from preference matrix, analyse the flows from top level predators

rm(list = ls()) # Reset
library(tidyverse)

# By flow matrix, maritime mammals (bears) like to eat seals, birds, cetaceans, and corpses
# let's check how that changes over the years we've ran the experiments.
# Required: flow matrices from the experiments

transient_years <- seq(2010,2050)
unfished <- readRDS("../Objects/biomass_transients.RDS")[[1]][["Flow_Matrices"]] # get unfished system
fished <- readRDS("../Objects/biomass_transients.RDS")[[2]][["Flow_Matrices"]] # get unfished system


crash <- readRDS("../Objects/Shifting_Baseline_Decadal_0_fishing.RDS")
crash_flow <- crash[["Flow_Matrices"]]

# Function to extract fluxes
extract_fluxes <- function(year,dat) {
  dat <- dat[[as.character(year)]]
  
  # flux values for target species
  seal <- dat["seal", "bear"]
  birds <- dat["bird", "bear"]
  cet <- dat["ceta", "bear"]
  corpses <- dat["corpses", "bear"]

  return(data.frame(
    Species = c("seal", "bird", "ceta", "corpses"),
    Flux = c(seal, birds, cet, corpses),
    Year = year
  ))
}

# apply function over transient_years
results_fished <- do.call(rbind, lapply(transient_years, function(year) extract_fluxes(year, fished)))
results_unfished <- do.call(rbind, lapply(transient_years, function(year) extract_fluxes(year, unfished)))
results_no_crash <- do.call(rbind, lapply(transient_years, function(year) extract_fluxes(year, crash_flow)))

saveRDS(results_fished,"../Objects/Flux_To_Bears_fished.RDS")
saveRDS(results_unfished,"../Objects/Flux_To_Bears_unfished.RDS")
saveRDS(results_no_crash,"../Objects/Flux_To_Bears_noCrash.RDS")
