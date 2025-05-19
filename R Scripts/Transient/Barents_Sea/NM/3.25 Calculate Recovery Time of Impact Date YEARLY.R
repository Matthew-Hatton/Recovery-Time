rm(list = ls())

library(purrr)
library(dplyr)
library(tibble)
library(ggplot2)
library(patchwork)

transient_years <- seq(2020,2099)

all <- readRDS("../Objects/Experiments/Rolling Crash/Rolling_Crash_base_MSY_2xMSY_Demersal_crash_YEARLY.RDS")
baseline <- readRDS("../Objects/Experiments/Baseline/Baseline_0_fishing_Demersal_fish.RDS")

baseline_df <- data.frame(
  year = transient_years[1:length(baseline[["Biomasses"]])],
  baseline = map_dbl(baseline[["Biomasses"]], ~ .x$Model_annual_mean[27])) %>% 
    mutate(MSC = baseline*0.8)

annotate_transient <- function(transient_list) {
  imap(transient_list[["Biomasses"]], ~ {
    crash_year <- as.integer(.y)
    imap(.x, ~ mutate(.x,
                      crash_year = crash_year,
                      year = as.integer(.y)))
  })
}

transient_base <- annotate_transient(all[[1]])
transient_MSY  <- annotate_transient(all[[2]])
transient_2x_MSY <- annotate_transient(all[[3]])

compute_recovery <- function(transient_list, baseline_df, label, threshold_val,threshold_col = "baseline") {
  map2_dfr(transient_list, names(transient_list), function(sublist, crash_year) {
    df <- sublist %>%
      bind_rows() %>%
      filter(Description == "Demersal_fish", year <= 2089) %>%
      left_join(baseline_df, by = "year")
    
    threshold <- threshold_val * df[[threshold_col]]
    
    recovery_row <- df %>%
      filter(Model_annual_mean >= threshold) %>%
      slice(1)
    
    recovery_time <- if (nrow(recovery_row) == 0) {
      NA
    } else {
      recovery_row$year[1] - as.integer(crash_year)
    }
    
    data.frame(
      crash_year = as.integer(crash_year),
      Recovery_Time = recovery_time,
      HR = label
    )
  })
}




threshold_val <- 0.99
baseline_MSC <- "baseline" # will use for plotting later

base_recovery    <- compute_recovery(transient_base, baseline_df, "Baseline",baseline_MSC,
                                     threshold_val = threshold_val)
msy_recovery     <- compute_recovery(transient_MSY, baseline_df, "MSY",baseline_MSC,
                                     threshold_val = threshold_val)
double_msy_recovery <- compute_recovery(transient_2x_MSY, baseline_df, "2x MSY",baseline_MSC,
                                        threshold_val = threshold_val)

combined_recovery <- bind_rows(base_recovery, msy_recovery, double_msy_recovery)


base <- ggplot(combined_recovery, aes(x = crash_year, y = Recovery_Time, color = HR)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(x = "Release Year", y = "Recovery Time (Years)", color = "Harvest Rate",
       title = "0 Fishing Baseline") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top") +
  NULL

baseline_MSC <- "MSC" # will use for plotting later

base_recovery    <- compute_recovery(transient_base,    baseline_df, "Baseline",baseline_MSC,
                                     threshold_val = threshold_val)
msy_recovery     <- compute_recovery(transient_MSY,     baseline_df, "MSY",baseline_MSC,
                                     threshold_val = threshold_val)
double_msy_recovery <- compute_recovery(transient_2x_MSY, baseline_df, "2x MSY",baseline_MSC,
                                        threshold_val = threshold_val)

combined_recovery <- bind_rows(base_recovery, msy_recovery, double_msy_recovery)

MSC <- ggplot(combined_recovery, aes(x = crash_year, y = Recovery_Time, color = HR)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(x = "Release Year", y = "Recovery Time (Years)", color = "Harvest Rate",
       title = "MSC Fishing Baseline") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top") +
  NULL

base + MSC + plot_layout(guides = "collect",axis_titles = "collect") & theme(legend.position = 'top')

ggsave(paste0("../Figures/Transient/Barents_Sea/NM/Draft 1/Figure 2 BASE MSC - ",threshold_val,".png"),
       dpi = 1200,width = 25,unit = "cm",bg = "white")

## Get biomasses for supplementary (?) figure
extract_biomass <- function(scenario, crash_year, label) {
  scenario_data <- scenario[[as.character(crash_year)]]
  
  data.frame(
    year = (crash_year + 1):(crash_year + length(scenario_data)),
    bio = map_dbl(scenario_data, ~ .x$Model_annual_mean[27]),
    HR = label,
    crash_year = crash_year
  )
}

# All crash years
crash_years <- seq(2020, 2090, 5)

# Combine data across all crash years and harvest rates
biomass_df <- map_dfr(crash_years, function(cy) {
  bind_rows(
    extract_biomass(transient_base, cy, "Baseline"),
    extract_biomass(transient_MSY, cy, "MSY"),
    extract_biomass(transient_2x_MSY, cy, "2x MSY")
  )
})

# Plot with facet per crash year
ggplot(biomass_df, aes(x = year, y = bio, color = HR)) +
  geom_line() +
  # geom_smooth(se = FALSE) +
  geom_line(
    data = baseline_df,
    aes(x = year, y = baseline), linetype = "dashed", inherit.aes = FALSE,alpha = 0.6
  ) +
  geom_line(
    data = baseline_df,
    aes(x = year, y = MSC), linetype = "dashed", inherit.aes = FALSE,alpha = 0.2
  ) +
  facet_wrap(~ crash_year, ncol = 3, scales = "free_x",strip.position = "top") +
  labs(
    # title = "Demersal Fish Biomass Post-Crash by Crash Year and Harvest Rate",
    x = "Year", y = "Biomass", color = "Harvest Rate"
  ) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"),
        legend.position = "top",
        legend.text = element_text(size = 12)) +
  NULL
ggsave("../Figures/Transient/Barents_Sea/NM/Draft Supplementary/Figure 2 Supplementary.png",
       dpi = 1200,width = 25,unit = "cm",bg = "white")
