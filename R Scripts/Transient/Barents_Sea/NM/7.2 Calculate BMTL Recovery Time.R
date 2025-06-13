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


all <- readRDS("../Objects/Experiments/Rolling Crash/BMTL.RDS")
baseline <- readRDS("../Objects/Experiments/Baseline/Baseline_0_fishing_Demersal_fish_1year.RDS")
# example <- all[[1]][["Network_Indicators"]][["2020"]][["HR = 1"]][["2021"]] %>% 
#   mutate(row_number = seq(1,nrow(.)))


baseline_df <- data.frame(
  year = transient_years[1:length(baseline[["Biomasses"]])],
  baseline = map_dbl(baseline[["BMTL"]], ~ .x)
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
  current <- all[[i]][["BMTL"]][[1]]
  
  for (k in 1:3) {
    ## DEBUG
    # k=1
    hrs <- current[[k]]
    df <- data.frame(
      year = (interval[i] + 1):max(transient_years),
      Biomass = map_dbl(hrs, ~ .x)) %>% #extract DF biomass
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
  values = c("Baseline" = "#1b9e77", "MSY" = "#7570b3", "2x MSY" = "#d95f02"),
  name = "Harvest Rate"
)

master$HR <- factor(master$HR, levels=c('Baseline', 'MSY', '2x MSY')) # reorder legend

MTL <- ggplot() +
  geom_line(data = master, aes(x = year, y = Biomass, color = as.character(HR))) +
  geom_line(
    data = baseline_df,
    aes(x = year, y = baseline), inherit.aes = FALSE,alpha = 1
  ) +
  # geom_ribbon(data = baseline_df,
  #             aes(x = year,ymin = baseline - (baseline * 0.2),ymax = baseline + (baseline * 0.2)),
  #             alpha = 0.1) +
  facet_wrap(~ Crash_Year, ncol = 3, scales = "free_x",strip.position = "top") +
  color_scale +
  labs(x = "Year", y = "Biomass Weighted Ecosystem Mean Trophic Level", color = "Harvest Rate"
  ) +
  theme_minimal(base_size = 14) +
  theme(strip.text = element_text(face = "bold"),
        legend.position = "top",
        legend.text = element_text(size = 12),axis.text.x = element_text(size = 6)) +
  NULL
MTL
ggsave("../Figures/Transient/Barents_Sea/NM/Draft 1/Figure 5/Figure X Biomass Weighted Ecosystem Mean Trophic Level.png",
       dpi = 1200,width = 25,unit = "cm",bg = "white",plot = MTL)

############ RECOVER TO BASELINE
tolerance <- 0.20  # ±5% around the baseline

recovery_time <- master %>%
  group_by(HR, Crash_Year) %>%
  arrange(year) %>%
  mutate(
    lower_bound = baseline * (1 - tolerance),
    upper_bound = baseline * (1 + tolerance),
    in_band = Biomass >= lower_bound & Biomass <= upper_bound
  ) %>%
  summarise(
    Recovery_Year = if (first(in_band)) first(year) else first(year[in_band]),
    Recovery_Time = Recovery_Year - first(Crash_Year),
    .groups = "drop"
  ) %>% 
  mutate(Recovery_Time = replace(Recovery_Time, Recovery_Time == 1, 0))

bird_recovery <- ggplot(recovery_time, aes(x = Crash_Year, y = Recovery_Time, color = as.character(HR))) +
  geom_line(linewidth = 1,alpha = 1) +
  geom_point(size = 2,alpha = 1) +
  geom_hline(yintercept = 20, linetype = "dashed") +
  color_scale +
  labs(x = "Collapse Year", y = "Recovery Time (Years)", color = "Harvest Rate"
  ) +
  scale_y_continuous(limits = c(0, NA)) +  # y-axis starts at 0
  scale_x_continuous(limits = c(2020,2085)) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top",
        legend.text = element_text(size = 12)) +
  NULL
# ggsave("../Figures/Transient/Barents_Sea/NM/Draft 1/Figure 5/Figure 5d Bird Omnivory Index Recovery.png",
#        dpi = 1200,width = 25,unit = "cm",bg = "white",plot = bird_recovery)

bird_omniv + bird_recovery
ggsave("../Figures/Transient/Barents_Sea/NM/Draft 1/Figure 4/Figure 4.png",
       dpi = 1200,width = 35,height = 20,unit = "cm",bg = "white") # will need cleaning up for publication
