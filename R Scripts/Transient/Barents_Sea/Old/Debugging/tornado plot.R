
model <- e2ep_read("Barents_Sea","2011-2019")
model[["data"]][["fleet.model"]][["HRscale_vector_multiplier"]] <- rep(0,length(model[["data"]][["fleet.model"]][["HRscale_vector_multiplier"]]))
results_no_fishing <- e2ep_run(model,nyears = 50)
## Check Biomass ## 
no_fishing_biomass <- results_no_fishing[["final.year.outputs"]][["mass_results_wholedomain"]] %>% 
  mutate(Marker = "Not Fishing")


model[["data"]][["fleet.model"]][["HRscale_vector_multiplier"]] <- rep(0,length(model[["data"]][["fleet.model"]][["HRscale_vector_multiplier"]]))
model[["data"]][["fleet.model"]][["HRscale_vector_multiplier"]][2] <- 10 # demersal fish set high
results_fishing <- e2ep_run(model,nyears = 50)
fishing_biomass <- results_fishing[["final.year.outputs"]][["mass_results_wholedomain"]] %>% 
  mutate(Marker = "Fishing")

e2ep_compare_runs_bar(results1 = results_no_fishing,results2 = results_fishing)
