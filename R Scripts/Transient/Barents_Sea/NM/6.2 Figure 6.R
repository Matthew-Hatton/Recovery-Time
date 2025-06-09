## Script to plot the transient fishing scenarios, transient baseline (dashed), and steady state baseline (solid)

rm(list = ls())
library(patchwork)
library(tidyverse)
library(purrr)
library(progressr)

transient_years <- seq(2020,2099)

all <- readRDS("../Objects/Experiments/Intermittent_Consistent/Consistent_1x_fishing_Demersal_fish.RDS")
baseline_non_ss <- readRDS("../Objects/Experiments/Baseline/Baseline_0_fishing_Demersal_fish_1year.RDS")

baseline_non_ss_df <- data.frame(
  year = transient_years[1:length(baseline_non_ss[["Biomasses"]])],
  baseline = map_dbl(baseline_non_ss[["Biomasses"]], ~ .x$Model_annual_mean[27])) %>% #extract DF biomass
  mutate(MSC = baseline * 0.8)

consistent_fishing <- data.frame(
  year = transient_years[1:length(all[["Biomasses"]])],
  biomass = map_dbl(all[["Biomasses"]], ~ .x$Model_annual_mean[27]))

ggplot() +

  geom_line(
    data = baseline_non_ss_df,
    aes(x = year, y = baseline), inherit.aes = FALSE,alpha = 1,color = "black"
  ) +
  geom_ribbon(data = baseline_non_ss_df,
              aes(x = year,ymin = baseline - (baseline * 0.2),ymax = baseline),
              alpha = 0.1) +
  geom_line(data = consistent_fishing, aes(x = year, y = biomass),color = "#C03F9B") +
  labs(
    x = "Year", y = "Demersal Fish Biomass (mmN/m2)") +
  theme_minimal() +
  NULL

# ggsave("../Figures/Transient/Barents_Sea/NM/Draft 1/Figure 2/Figure 2.png",
#        height = 1080,
#        width = 1920,
#        units = "px",
#        dpi = 200,
#        bg = "white")
