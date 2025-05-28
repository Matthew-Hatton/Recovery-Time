rm(list = ls())

library(purrr)
library(dplyr)
library(tibble)
library(ggplot2)
library(patchwork)
library(slider)
library(zoo)
library(progressr)

handlers("cli")

transient_years <- seq(2020,2099)
interval <- seq(2020,2085,5)


all <- readRDS("../Objects/Experiments/Rolling Crash/Rolling_Crash_and_MSY_Demersal.RDS")
baseline <- readRDS("../Objects/Experiments/Baseline/Baseline_0_fishing_Demersal_fish_1year.RDS")
example <- all[[1]][["Network_Indicators"]][["2020"]][["HR = 1"]][["2021"]] %>% 
  mutate(row_number = seq(1,nrow(.)))


baseline_df <- data.frame(
  year = transient_years[1:length(baseline[["Biomasses"]])],
  baseline = map_dbl(baseline[["Network_Indicators"]], ~ .x["bird_omnivoryindex",])
)

master <- data.frame(Baseline = numeric(0),
                     MSC = numeric(0),
                     Crash_Year = numeric(0),
                     Biomass = numeric(0),
                     year = numeric(0),
                     HR = character(0))

for (i in 1:length(all)) {
  ## DEBUG
  # i = 1
  current <- all[[i]][["Network_Indicators"]][[1]]
  
  for (k in 1:3) {
    ## DEBUG
    # k=1
    hrs <- current[[k]]
    df <- data.frame(
      year = (interval[i] + 1):max(transient_years),
      Biomass = map_dbl(hrs, ~ .x["bird_omnivoryindex",])) %>% #extract DF biomass
      mutate(Crash_Year = interval[i],
             HR = case_when(
               k == 1 ~ "Baseline",
               k == 2 ~ "MSY",
               k == 3 ~ "2x MSY"
             ),
             HR_Prior = names(current)[k])
    base <- baseline_df %>% filter(year > interval[i])
    df <- df %>% mutate(baseline = base$baseline)
    master <- rbind(df,master)
  }
}

ggplot(master, aes(x = year, y = Biomass, color = as.character(HR))) +
  geom_line() +
  geom_line(
    data = baseline_df,
    aes(x = year, y = baseline), inherit.aes = FALSE,alpha = 1
  ) +
  facet_wrap(~ Crash_Year, ncol = 3, scales = "free_x",strip.position = "top") +
  labs(x = "Year", y = "Bird Omnivory Index", color = "Harvest Rate"
  ) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"),
        legend.position = "top",
        legend.text = element_text(size = 12)) +
  NULL
ggsave("../Figures/Transient/Barents_Sea/NM/Draft 1/Figure 4/Figure 4c Bird Omnivory Index.png",
       dpi = 1200,width = 25,unit = "cm",bg = "white")