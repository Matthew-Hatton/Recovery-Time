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
baseline <- readRDS("../Objects/Experiments/Baseline/Baseline_0_fishing_Demersal_fish.RDS")

baseline_df <- data.frame(
  year = transient_years[1:length(baseline[["Biomasses"]])],
  baseline = map_dbl(baseline[["Biomasses"]], ~ .x$Model_annual_mean[27])) %>% #extract DF biomass
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

# Define parameters
threshold_val <- 1     # E.g., 0.8 if threshold is 80% of baseline
tolerance <- 0.05      # ±5%
n_consecutive <- 5     # Require at least 3 consecutive years within range

# Add threshold bands and check if values are within range OR above baseline
master_threshold <- master %>%
  group_by(HR, Crash_Year) %>%
  mutate(
    threshold = threshold_val * baseline,
    lower_bound = threshold * (1 - tolerance),
    upper_bound = threshold * (1 + tolerance),
    in_range = (Biomass >= lower_bound & Biomass <= upper_bound) | (Biomass >= baseline)
  )

# Use sliding window to detect first year with sufficient consecutive recovery
recovery_baseline <- master_threshold %>%
  group_by(HR, Crash_Year) %>%
  arrange(year) %>%
  mutate(
    recovered = slide_lgl(in_range, ~all(.x), .before = 0, .after = n_consecutive - 1)
  ) %>%
  filter(recovered) %>%
  slice_min(year, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(Recovery_Time_Baseline = year - Crash_Year) %>%
  dplyr::select(HR, Crash_Year, Recovery_Time_Baseline)


ggplot(recovery_baseline, aes(x = Crash_Year, y = Recovery_Time_Baseline, color = as.character(HR))) +
  geom_line(linewidth = 1,alpha = 0.4) +
  geom_point(size = 2,alpha = 0.4) +
  # geom_smooth(se = FALSE, linewidth = 0.75) +
  geom_hline(yintercept = 20, linetype = "dashed") +
  labs(x = "Release Year", y = "Recovery Time (Years)", color = "Harvest Rate") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +  # y-axis starts at 0
  theme_minimal(base_size = 14) +
  theme(legend.position = "top") +
  NULL

ggsave("../Figures/Transient/Barents_Sea/NM/Draft 1/Figure 3 rolling MSY.png",
       dpi = 1200,width = 25,unit = "cm",bg = "white")

ggplot() +
  geom_line(data = master, aes(x = year, y = Biomass, color = as.character(HR))) +
  geom_line(
    data = baseline_df,
    aes(x = year, y = baseline), linetype = "dashed", inherit.aes = FALSE,alpha = 1
  ) +
  # geom_smooth(se = FALSE) +
  geom_ribbon(
    data = baseline_df,
    aes(x = year, y = baseline,ymin = baseline - (baseline * 0.05),ymax = baseline + (baseline * 0.05)),alpha = 0.1) +
  facet_wrap(~ Crash_Year, ncol = 3, scales = "free_x",strip.position = "top") +
  labs(
    # title = "Demersal Fish Biomass Post-Crash by Crash Year and Harvest Rate",
    x = "Year", y = "Demersal Fish Biomass (mmN/m2)", color = "Harvest Rate"
  ) +
  scale_x_continuous(limits = c(2020,2099)) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"),
        legend.position = "top",
        legend.text = element_text(size = 12)) +
  NULL
ggsave("../Figures/Transient/Barents_Sea/NM/Draft 1/Figure 2 rolling MSY.png",
       dpi = 1200,width = 25,unit = "cm",bg = "white")
# 
# 
# ## EXPLORATION
# explore <- biomass_df %>% 
#   filter(crash == 2075)
# ggplot(explore, aes(x = year, y = biomass, color = as.character(HR))) +
#   geom_line(
#     data = baseline_df,
#     aes(x = year, y = baseline), linetype = "dashed", inherit.aes = FALSE,alpha = 0.6
#   ) +
#   geom_line(linewidth = 1) +
#   labs(x = "Release Year", y = "Recovery Time (Years)", color = "Harvest Rate") +
#   scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +  # y-axis starts at 0
#   theme_minimal(base_size = 14) +
#   theme(legend.position = "top") +
#   NULL
