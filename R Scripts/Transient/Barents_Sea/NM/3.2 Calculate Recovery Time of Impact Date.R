## Calculate Recovery Time based on date of impact

#### Setup ####
rm(list=ls())                                                                                              # Wipe the brain
Packages <- c("MiMeMo.tools", "exactextractr", "raster", "lubridate","StrathE2EPolar","furrr","tictoc","progressr")                     # List packages
lapply(Packages, library, character.only = TRUE)   

transient_years <- seq(2020,2089)

# compute recovery time for one scenario list
compute_recovery <- function(transient_list, baseline_df, HR_label) {
  map_dfr(transient_list, function(crash_list) {
    current <- bind_rows(crash_list) %>%
      filter(Description == "Demersal_fish", year <= 2089) %>%
      left_join(baseline_df, by = "year")
    
    crash_year <- unique(current$crash_year)
    if (length(crash_year) != 1) stop("Multiple crash years in one scenario") # error handler
    
    threshold <- current$baseline
    total_years <- nrow(current)
    
    if (current$Model_annual_mean[1] >= threshold[1]) {
      above_count <- sum(current$Model_annual_mean >= threshold)
      if (above_count / total_years > 0.5) {
        recovery_time <- 0
      } else {
        below_years <- current %>% filter(Model_annual_mean < threshold)
        recovery_time <- if (nrow(below_years) > 0) {
          -(below_years$year[1] - crash_year)
        } else NA
      }
    } else {
      recovery_row <- current %>% filter(Model_annual_mean >= threshold) %>% slice(1)
      if (nrow(recovery_row) == 0) {
        recovery_time <- NA
      } else {
        recovery_year <- recovery_row$year[1]
        after_recovery <- current %>% filter(year >= recovery_year)
        years_above <- sum(after_recovery$Model_annual_mean >= 0.99 * after_recovery$baseline)
        years_total <- nrow(after_recovery)
        
        recovery_time <- if (years_above / years_total > 0.5) {
          recovery_year - crash_year
        } else NA
      }
    }
    
    tibble(Recovery_Time = recovery_time, crash_year = crash_year, HR = HR_label)
  })
}

all <- readRDS("../Objects/Experiments/Rolling Crash/Rolling_Crash_base_MSY_2xMSY_Demersal_crash.RDS")

# prep inputs
transient_base <- imap(all[[1]][["Biomasses"]], ~ {
  crash_year <- as.integer(.y)
  imap(.x, ~ mutate(.x, crash_year = crash_year, year = as.integer(.y)))
})

transient_MSY <- imap(all[[2]][["Biomasses"]], ~ {
  crash_year <- as.integer(.y)
  imap(.x, ~ mutate(.x, crash_year = crash_year, year = as.integer(.y)))
})

transient_2x_MSY <- imap(all[[3]][["Biomasses"]], ~ {
  crash_year <- as.integer(.y)
  imap(.x, ~ mutate(.x, crash_year = crash_year, year = as.integer(.y)))
})

baseline <- readRDS("../Objects/Experiments/Baseline/Baseline_0_fishing_Demersal_fish.RDS")
baseline_vec <- sapply(baseline[["Biomasses"]], function(x) x$Model_annual_mean[27])

baseline_df <- data.frame(
  baseline = baseline_vec,
  MSC = baseline_vec * 0.8,
  year = transient_years[1:length(baseline_vec)]
)


# Run the recovery simulation
base_df     <- compute_recovery(transient_base, baseline_df, "Baseline")
msy_df      <- compute_recovery(transient_MSY, baseline_df, "MSY")
double_df   <- compute_recovery(transient_2x_MSY, baseline_df, "2x MSY")

recovery_all <- bind_rows(base_df, msy_df, double_df)

# Plot
ggplot(recovery_all, aes(x = crash_year, y = Recovery_Time, color = HR)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  theme_minimal(base_size = 14) +
  labs(title = "Recovery Time of Demersal Fish Under Different Fishing Pressures",
       x = "Crash Year", y = "Recovery Time (Years)",
       color = "Harvest Rate") +
  theme(legend.position = "top")


## DEBUG
## Recovery time being the same in the initial crash year can't be correct
## let's check that
years <- 2021:2091  # 72 years

# Extract year-27 biomass for each crash year scenario
base_vec <- map_dbl(transient_base[[1]], ~ .x$Model_annual_mean[27])
msy_vec  <- map_dbl(transient_MSY[[1]], ~ .x$Model_annual_mean[27])
msy2_vec <- map_dbl(transient_2x_MSY[[1]], ~ .x$Model_annual_mean[27])

# Combine into one tidy data frame
biomass_df <- tibble(
  year = years,
  Baseline = base_vec,
  MSY = msy_vec,
  `2x MSY` = msy2_vec
) %>%
  pivot_longer(cols = -year, names_to = "HR", values_to = "Model_annual_mean")

ggplot() +
  geom_line(data = biomass_df,aes(x = year,y = Model_annual_mean,color = HR)) +
  geom_line(data = baseline_df %>% filter(year >= 2021),aes(x = year,y = baseline),linetype = "dashed")

## seems okay - guess there's something wrong with that recovery time calculation