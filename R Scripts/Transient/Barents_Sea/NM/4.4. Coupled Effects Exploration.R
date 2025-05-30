## Script to plot the transient fishing scenarios, transient baseline (dashed), and steady state baseline (solid)

rm(list = ls())
library(patchwork)
library(tidyverse)
library(purrr)
library(progressr)

transient_years <- seq(2020,2099)
interval <- seq(2020,2085,5)

all <- readRDS("../Objects/Experiments/Coupled Effects/Baseline__fishing_Demersal_fish.RDS")
baseline_non_ss <- readRDS("../Objects/Experiments/Baseline/Baseline_0_fishing_Demersal_fish_1year.RDS")


## BASELINES
baseline_non_ss_df <- data.frame(
  year = transient_years[1:length(baseline_non_ss[["Biomasses"]])],
  baseline = map_dbl(baseline_non_ss[["Biomasses"]], ~ .x$Model_annual_mean[27])) %>% #extract DF biomass
  mutate(MSC = baseline * 0.8)

## Experiment
biomass_df <- data.frame(
  year = transient_years[1:length(all[["Biomasses"]])],
  baseline = map_dbl(all[["Biomasses"]], ~ .x$Model_annual_mean[27])) # extract DF biomass

ggplot() +
  geom_line(data = biomass_df,aes(x = year,y = baseline),color = "red") +
  geom_line(data = baseline_non_ss_df,aes(x = year,y = baseline))


change <- data.frame(year = transient_years,
                     change = baseline_non_ss_df$baseline - biomass_df$baseline)
ggplot() +
  geom_line(data = change,aes(x = year,y = change),color = "red") +
  geom_ribbon(data = change,aes(x = year,ymin = change - (change * 0.05),ymax = change + (change * 0.05)),alpha = 0.2) +
  scale_y_continuous(limits = c(0,2))
