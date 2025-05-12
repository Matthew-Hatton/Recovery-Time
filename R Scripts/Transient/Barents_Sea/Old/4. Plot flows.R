## We want to see how the diet of some guilds is shifting ie. what is eating what

rm(list = ls())
library(tidyverse)

transient_years <- as.character(seq(2010,2099))
transient <- readRDS("../Objects/Experiments/Crash/Demersal_crash_10_relax_0.RDS")[["Flow_Matrices"]] # load transients
baseline <- readRDS("../Objects/Experiments/Baseline/50_Baseline_0_fishing_Demersal_fish.RDS")[["Flow_Matrices"]]



## Want to see how the flows into Cetaceans is changing over time
## From the Preference Matrix, high preference for planktivorous fish and Migratory fish (biomass is low here)

cetacean_flows <- data.frame(Year = NA,
                             Flow = NA,
                             Guild = NA)

for (i in 1:length(transient)) {
  transient_cet_pfish <- data.frame(Year = transient_years[i],
                              Flow = transient[[transient_years[i]]]["pfish","ceta"],
                              Guild = "Planktivorous Fish")
  transient_cet_dfish <- data.frame(Year = transient_years[i],
                                    Flow = transient[[transient_years[i]]]["dfish","ceta"],
                                    Guild = "Demersal Fish")
  cetacean_flows <- rbind(transient_cet_dfish,transient_cet_pfish,cetacean_flows)
}

cetacean_baseline_flows <- data.frame(Year = NA,
                             Flow = NA,
                             Guild = NA)

for (i in 1:length(transient)) {
  transient_cet_pfish <- data.frame(Year = transient_years[i],
                                    Flow = baseline[[transient_years[i]]]["pfish","ceta"],
                                    Guild = "Planktivorous Fish")
  transient_cet_dfish <- data.frame(Year = transient_years[i],
                                    Flow = baseline[[transient_years[i]]]["dfish","ceta"],
                                    Guild = "Demersal Fish")
  cetacean_baseline_flows <- rbind(transient_cet_dfish,transient_cet_pfish,cetacean_baseline_flows)
}

cetacean_flows <- na.omit(cetacean_flows)
cetacean_baseline_flows <- na.omit(cetacean_baseline_flows)

ggplot() +
  geom_line(data = cetacean_flows,aes(x = as.numeric(Year),y = Flow,color = Guild)) + 
  geom_line(data = cetacean_baseline_flows,aes(x = as.numeric(Year),y = Flow,color = Guild),linetype = "dashed") + 
  labs(title = "Flow from Guilds into Cetaceans")
