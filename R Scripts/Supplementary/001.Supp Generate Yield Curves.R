## script to run yield curves for supplementary figure within manuscript

rm(list = ls()) # reset

library(StrathE2EPolar)
library(furrr)
library(MiMeMo.tools)

plan(multisession)

focal <- c("DEMERSAL","PLANKTIV")

main <- function(focal){
  model <- e2ep_read("Barents_Sea","2011-2019")
  res <- e2ep_run_ycurve(model,selection = focal,HRvector = seq(0,5,0.2),nyears = 50)
}

results <- future_map(.x = focal,
                      ~{
   main(focal = .x)})

saveRDS(results,"../Objects/Supplementary/yield_curves.RDS")

model <- e2ep_read("Barents_Sea","2011-2019")

## PLOT
e2ep_plot_ycurve(model = model,results = results[[1]],selection = "DEMERSAL")

e2ep_plot_ycurve(model = model,results = results[[2]],selection = "PLANKTIV")
