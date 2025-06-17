rm(list = ls())

library(tidyverse)
library(ggrepel)
library(patchwork)

## present
all <- readRDS("../Objects/Experiments/Rolling Crash/Rolling_Crash_Static_MSY_Demersal.RDS")
baseline <- readRDS("../Objects/Experiments/Baseline/Baseline_0_fishing_Demersal_fish_1year.RDS")
example <- all[[1]][["Network_Indicators"]][["2020"]][["HR = 1"]][["2021"]] %>% 
  mutate(row_number = seq(1,nrow(.)))
transient_years <- seq(2020,2099)

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

color_scale <- scale_color_manual(
  values = c("2020s Baseline" = "#1b9e77", "2020s MSY" = "#7570b3", "2020s 2x MSY" = "#d95f02"),
  name = "Harvest Rate"
)

master <- master %>% mutate(HR = case_when(
  HR == "Baseline" ~ "2020s Baseline",
  HR == "MSY" ~ "2020s MSY",
  HR == "2x MSY" ~ "2020s 2x MSY"
))

master$HR <- factor(master$HR, levels=c('2020s Baseline', '2020s MSY', '2020s 2x MSY')) # reorder legend


labelled_points <- merged[round(merged$HR, 1) %in% c(0, 1, 2.8, 5.6), ]


ggplot(data = merged, aes(x = catch, y = Recovery_Time_MSC, color = as.character(Decade))) +
  geom_hline(yintercept = 20, linetype = "dashed") +
  
  geom_path(
    arrow = arrow(type = "closed", length = unit(0.15, "inches")),
    lineend = "round"
  ) +
  
  geom_point(
    data = labelled_points,
    aes(x = catch, y = Recovery_Time_MSC),
    size = 2,
    show.legend = FALSE
  ) +
  
  geom_text_repel(
    data = labelled_points,
    aes(label = HR, color = as.character(Decade)),
    size = 5,
    max.overlaps = 10,
    segment.color = "grey50"
  ) +
  
  labs(color = "Decade",
       x = "Demersal Catch (N mmol⋅m¯³)",
       y = "Recovery Time (Years)") +
  
  scale_color_manual(values = c("grey40", "#592DD2", "#D2592D")) +
  
  # Override legend: smaller line and no point
  guides(color = guide_legend(override.aes = list(size = 1.5, shape = NA))) +
  
  theme_bw()
ggsave("./Figures/Figure 5 Catch vs. Recovery Time.png",
       dpi = 1200,width = 30,height = 25,unit = "cm",bg = "white")

