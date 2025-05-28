## Script to calculate Demersal stock recovery time using a rolling MSY value

rm(list = ls())

library(purrr)
library(dplyr)
library(tibble)
library(ggplot2)
library(patchwork)
library(slider)
library(zoo)
library(progressr)

progressr::handlers("debug")


transient_years <- seq(2020,2099)
interval <- seq(2020,2085,5)

all <- readRDS("../Objects/Experiments/Rolling Crash/Rolling_Crash_and_MSY_Demersal.RDS")
baseline <- readRDS("../Objects/Experiments/Baseline/Baseline_0_fishing_Demersal_fish_1year.RDS")

baseline_df <- data.frame(
  year = transient_years[1:length(baseline[["Biomasses"]])],
  baseline = map_dbl(baseline[["Biomasses"]], ~ filter(.x,Description == "Birds")$Model_annual_mean)) %>% #extract DF biomass
  mutate(MSC = baseline * 0.8)


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
      Biomass = map_dbl(hrs, ~ filter(.x,Description == "Birds")$Model_annual_mean)) %>% #extract DF biomass
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


ggplot() +
  geom_line(data = master, aes(x = year, y = Biomass, color = as.character(HR))) +
  geom_line(
    data = baseline_df,
    aes(x = year, y = baseline), inherit.aes = FALSE,alpha = 1
  ) +
  # geom_smooth(se = FALSE) +
  geom_ribbon(
    data = baseline_df,
    aes(x = year, y = baseline,ymin = baseline - (baseline * 0.05),ymax = baseline + (baseline * 0.05)),alpha = 0.1) +
  facet_wrap(~ Crash_Year, ncol = 3, scales = "free_x",strip.position = "top") +
  labs(
    # title = "Demersal Fish Biomass Post-Crash by Crash Year and Harvest Rate",
    x = "Year", y = "Bird Biomass (mmN/m2)", color = "Harvest Rate",
    caption = "
    Harvesting at Baseline releases predation pressure from their highest weighted preference, Planktivorous Fish.
    \n Harvesting at MSY does more damage to bird biomass than harvesting at 2x MSY."
  ) +
  scale_x_continuous(limits = c(2020,2099)) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"),
        legend.position = "top",
        legend.text = element_text(size = 12)) +
  NULL
ggsave("../Figures/Transient/Barents_Sea/NM/Draft 1/Figure 4/Figure 4b Bird Biomass.png",
       dpi = 1200,width = 35,unit = "cm",bg = "white") # will need cleaning up for publication


