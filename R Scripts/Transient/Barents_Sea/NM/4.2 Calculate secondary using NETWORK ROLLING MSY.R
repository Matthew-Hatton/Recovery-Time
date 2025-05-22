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
baseline <- readRDS("../Objects/Experiments/Baseline/Baseline_0_fishing_Demersal_fish.RDS")
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
    # k=2
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

# Plot with facet per crash year
ggplot(master, aes(x = year, y = Biomass, color = as.character(HR))) +
  geom_line() +
  # geom_smooth(se = FALSE) +
  geom_line(
    data = baseline_df,
    aes(x = year, y = baseline), linetype = "dashed", inherit.aes = FALSE,alpha = 0.6
  ) +
  # geom_line(
  #   data = baseline_df,
  #   aes(x = year, y = MSC), linetype = "dashed", inherit.aes = FALSE,alpha = 0.2
  # ) +
  facet_wrap(~ Crash_Year, ncol = 3, scales = "free_x",strip.position = "top") +
  labs(
    # title = "Demersal Fish Biomass Post-Crash by Crash Year and Harvest Rate",
    x = "Year", y = "Bird Omnivory Index", color = "Harvest Rate"
  ) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"),
        legend.position = "top",
        legend.text = element_text(size = 12)) +
  NULL
ggsave("../Figures/Transient/Barents_Sea/NM/Draft 1/Figure 2 BIRD OMNIVORY ROLLING MSY.png",
       dpi = 1200,width = 25,unit = "cm",bg = "white")


## logical next step is to look at the flux into and out of birds to try and explain this
# 
# differences <- biomass_df %>% 
#   group_by(HR,crash) %>% 
#   summarise(diff = mean(baseline - biomass))
# 
# ggplot(differences, aes(x = crash, y = diff, color = as.character(HR))) +
#   geom_line(linewidth = 1) +
#   geom_point(size = 2) +
#   # geom_smooth(se = FALSE, alpha = 0.4, linewidth = 0.5) +
#   # geom_hline(yintercept = 20, linetype = "dashed") +
#   labs(x = "Release Year", y = "Mean Change in Baseline Bird Omniovry Index", color = "Harvest Rate",
#        title = "Bird Omnivory Index as a proxy for Ecosystem Health",
#        subtitle = "System not as robust due to decrease in Bird Omnivory Index") +
#   scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +  # y-axis starts at 0
#   theme_minimal(base_size = 14) +
#   theme(legend.position = "top") +
#   NULL
