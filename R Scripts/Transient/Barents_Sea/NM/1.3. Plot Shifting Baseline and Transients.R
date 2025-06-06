## Script to plot the transient fishing scenarios, transient baseline (dashed), and steady state baseline (solid)

rm(list = ls())
library(patchwork)
library(tidyverse)
library(purrr)
library(progressr)

transient_years <- seq(2020,2099)
interval <- seq(2020,2085,5)

all <- readRDS("../Objects/Experiments/Rolling Crash/Rolling_Crash_and_MSY_Demersal.RDS")
baseline <- readRDS("../Objects/Experiments/Baseline/Baseline_0_fishing_Demersal_fish.RDS")
baseline_non_ss <- readRDS("../Objects/Experiments/Baseline/Baseline_0_fishing_Demersal_fish_1year.RDS")


## BASELINES
baseline_df <- data.frame(
  year = transient_years[1:length(baseline[["Biomasses"]])],
  baseline = map_dbl(baseline[["Biomasses"]], ~ .x$Model_annual_mean[27])) %>% #extract DF biomass
  mutate(MSC = baseline * 0.8)

baseline_non_ss_df <- data.frame(
  year = transient_years[1:length(baseline_non_ss[["Biomasses"]])],
  baseline = map_dbl(baseline_non_ss[["Biomasses"]], ~ .x$Model_annual_mean[27])) %>% #extract DF biomass
  mutate(MSC = baseline * 0.8)

## SCENARIOS
master <- data.frame(Baseline = numeric(0),
                     MSC = numeric(0),
                     Crash_Year = numeric(0),
                     Biomass = numeric(0),
                     year = numeric(0),
                     HR = character(0))

for (i in 1:length(all)) {
  ## DEBUG
  # i = 1
  current <- all[[i]][["Biomasses"]][[1]]
  
  for (k in 1:3) {
    ## DEBUG
    # k=1
    hrs <- current[[k]]
    df <- data.frame(
      year = (interval[i] + 1):max(transient_years),
      Biomass = map_dbl(hrs, ~ .x$Model_annual_mean[27])) %>% #extract DF biomass
      mutate(Crash_Year = interval[i],
             HR = case_when(
               k == 1 ~ "Baseline",
               k == 2 ~ "MSY",
               k == 3 ~ "2x MSY"
             ))
    base <- baseline_df %>% filter(year > interval[i])
    df <- df %>% mutate(baseline = base$baseline,
                        MSC = base$baseline * 0.8)
    master <- rbind(df,master)
  }
}

master_2025 <- master %>% filter(Crash_Year == 2025)

ggplot() +
  geom_line(data = master_2025, aes(x = year, y = Biomass, color = as.character(HR))) +
  geom_line(
    data = baseline_df,
    aes(x = year, y = baseline), linetype = "dashed", inherit.aes = FALSE,alpha = 0.4
  ) +
  geom_line(
    data = baseline_non_ss_df,
    aes(x = year, y = baseline), inherit.aes = FALSE,alpha = 1,color = "black"
  ) +
  geom_ribbon(data = baseline_non_ss_df,
              aes(x = year,ymin = baseline - (baseline * 0.05),ymax = baseline),
              alpha = 0.2) +
  geom_ribbon(data = baseline_non_ss_df,
              aes(x = year,ymin = baseline - (baseline * 0.2),ymax = baseline),
              alpha = 0.2) +
  geom_vline(xintercept = c(2036,2046,2057),linetype = "solid",alpha = 0.6,color = c("#7CAE00","#00BFC4","#F8768D")) +
  facet_wrap(~ Crash_Year, ncol = 3, scales = "free_x",strip.position = "top") +
  labs(
    x = "Year", y = "Demersal Fish Biomass (mmN/m2)", color = "Harvest Rate"
  ) +
  scale_x_continuous(limits = c(2020,2099),breaks = c(2020,2036,2040,2046,2057,2060,2080,2100)) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"),
        legend.position = "top",
        legend.text = element_text(size = 12),
        axis.text.x = element_text(size = 8),
        panel.grid.minor = element_blank()) +
  NULL

ggsave("../Figures/Transient/Barents_Sea/NM/Draft 1/Figure 2/Figure 2.png",
       height = 1080,
       width = 1920,
       units = "px",
       dpi = 200,
       bg = "white")
