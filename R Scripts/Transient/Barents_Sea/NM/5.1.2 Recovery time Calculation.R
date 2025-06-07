## calculate recovery time of varying HR's

rm(list = ls()) #reset

# load packages
library(purrr)
library(dplyr)
library(tibble)
library(ggplot2)
library(patchwork)
library(slider)
library(zoo)
library(progressr)

progressr::handlers("cli") # progress bars are nice

# where are we calculating to?
transient_years <- seq(2020,2099)
HR <- seq(0,5.6,0.2) # 2x MSY

# load
all <- readRDS("../Objects/Experiments/Crash/Paper/Recovery_Time_Road_To_Recovery.RDS")
baseline_non_ss <- readRDS("../Objects/Experiments/Baseline/Baseline_0_fishing_Demersal_fish_1year.RDS")

# extract
baseline_df <- data.frame(
  year = transient_years[1:length(baseline_non_ss[["Biomasses"]])],
  baseline = map_dbl(baseline_non_ss[["Biomasses"]], ~ .x$Model_annual_mean[27])) %>% #extract DF biomass
  mutate(MSC = baseline * 0.8)
# Crash year (e.g., year when disturbance happens)
crash_year <- 2020  # Adjust this as needed


# Generalised recovery time function
get_recovery_time <- function(sim, threshold_vals) {
  biomasses <- sim$Biomasses
  model_vals <- sapply(biomasses, function(df) df$Model_annual_mean[27])
  
  years <- baseline_df$year[1:length(model_vals)]
  thresholds <- threshold_vals[1:length(model_vals)]
  
  above_threshold <- which(model_vals >= thresholds)
  
  if (length(above_threshold) == 0) {
    return(NA)
  } else {
    recovery_year <- years[above_threshold[1]]
    return(recovery_year - crash_year)
  }
}

# Apply to all simulations for both baseline and MSC
recovery_results <- tibble(
  HR = HR,
  recovery_time_baseline = map_dbl(all, ~ get_recovery_time(.x, baseline_df$baseline)),
  recovery_time_MSC      = map_dbl(all, ~ get_recovery_time(.x, baseline_df$MSC))
)

# Save
saveRDS(recovery_results, "../Objects/Experiments/Crash/Paper/Recovery_Time_Aggregation.RDS")

recovery_results_long <- recovery_results %>%
  pivot_longer(cols = starts_with("recovery_time"),
               names_to = "threshold", values_to = "recovery_time") %>%
  mutate(threshold = recode(threshold,
                            recovery_time_baseline = "Baseline",
                            recovery_time_MSC = "MSC"))

ggplot(recovery_results_long, aes(x = HR, y = recovery_time, color = threshold)) +
  geom_point() +
  geom_line() +
  labs(x = "Harvest Rate", y = "Recovery Time (years)", color = "Threshold") +
  theme_minimal(base_size = 14)
