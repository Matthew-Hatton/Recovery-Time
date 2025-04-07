## Using values from preference matrix, analyse the flows from top level predators

rm(list = ls()) # Reset
library(tidyverse)

# By flow matrix, maritime mammals (bears) like to eat seals, birds, cetaceans, and corpses
# let's check how that changes over the years we've ran the experiments.
# Required: flow matrices from the experiments

transient_years <- seq(2010,2050)
unfished <- readRDS("../Objects/biomass_transients.RDS")[[1]][["Flow_Matrices"]] # get unfished system
fished <- readRDS("../Objects/biomass_transients.RDS")[[2]][["Flow_Matrices"]] # get unfished system

# Function to extract fluxes
extract_fluxes <- function(year) {
  dat <- all_data[[as.character(year)]]
  
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
results <- do.call(rbind, lapply(transient_years, extract_fluxes))

saveRDS(results,"../Objects/Flux_To_Bears.RDS")

## Need to also see the flow into Demersal Fish
## Preference matrix identifies: benths, plankt, planklar
extract_fluxes_dem <- function(year) {
  dat <- fished[[as.character(year)]]
  
  # flux values for target species
  benths <- dat["benths", "dfish"]
  pfish <- dat["pfish", "dfish"]
  pfishlar <- dat["pfishlar", "dfish"]
  
  return(data.frame(
    Species = c("benths", "pfish","pfishlar"),
    Flux = c(benths, pfish, pfishlar),
    Year = year
  ))
}
res_unfished <- do.call(rbind, lapply(transient_years, extract_fluxes_dem))
res_fished <- do.call(rbind, lapply(transient_years, extract_fluxes_dem))

res_unfished <- res_unfished %>% mutate(marker = "unfished")
res_fished <- res_fished %>% mutate(marker = "fished")

final <- rbind(res_unfished,res_fished)
ggplot() +
  geom_line(data = final,aes(x = Year,y = Flux,color = marker)) +
  facet_wrap(~Species,ncol = 1)
