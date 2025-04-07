## Script to calculate initial biomass of guilds in the Barents Sea model

rm(list = ls()) # Reset

library(tidyverse)
library(StrathE2EPolar)

# Read in model
model <- e2ep_read(model.name = "Barents_Sea",
                   model.variant = "2011-2019")
results <- e2ep_run(model,nyears = 50)
initial_biomass <- results[["final.year.outputs"]][["mass_results_wholedomain"]] # Get all baseline biomasses
saveRDS(initial_biomass,"./Objects/Barents_Sea initial biomass.RDS")
