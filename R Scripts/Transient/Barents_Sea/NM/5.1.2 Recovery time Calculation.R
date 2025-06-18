calculate_recovery_time <- function(all_file,
                                    baseline_file,
                                    description = "Demersal_fish",
                                    threshold_multiplier = 0.8,
                                    start_year,
                                    save_path = NULL) {
  library(purrr)
  library(dplyr)
  library(tibble)
  library(ggplot2)
  library(slider)
  library(zoo)
  library(progressr)
  library(patchwork)
  
  handlers("cli")  # Enable progress bar
  
  # Define years and HR values
  transient_years <- seq(2020,2099)
  HR <- seq(0, 5.6, 0.2)
  
  ## DEBUG
  idx = 23
  start_year = 2070
  i = 29
  threshold_multiplier = 0.8
  all <- readRDS("../Objects/Experiments/Crash/Paper/Recovery_Time_Road_To_Recovery_2070.RDS")
  baseline_non_ss <- readRDS("../Objects/Experiments/Baseline/Baseline_0_fishing_Demersal_fish_1year.RDS")
  
  # Load input data
  all <- readRDS(all_file)
  baseline_non_ss <- readRDS(baseline_file)
  
  # Find the index for the specified guild description
  example_df <- baseline_non_ss[["Biomasses"]][[1]]
  if (!"Description" %in% names(example_df)) stop("Missing 'Description' in Biomasses data frame.")
  idx <- which(example_df$Description == description)
  if (length(idx) == 0) stop(paste("Description", description, "not found."))
  
  # Create baseline time series and apply threshold - baseline is correct
  baseline_df <- data.frame(
    year = transient_years[1:length(baseline_non_ss[["Biomasses"]])],
    baseline = map_dbl(baseline_non_ss[["Biomasses"]], ~ .x$Model_annual_mean[idx]),
    bird_omnivory = map_dbl(baseline_non_ss[["Network_Indicators"]], ~ .x$NetworkData[63])
  ) %>%
    mutate(threshold = baseline * threshold_multiplier) %>% 
    filter(year > start_year)
  
  # Initialize output
  recovery_time <- data.frame(HR = rep(0, length(all)),
                              Recovery_Time = rep(NA_integer_, length(all)),
                              Recovery_Time_bird = rep(NA_integer_, length(all)))
  
  # Run analysis with progress bar
  with_progress({
    p <- progressor(steps = length(all))
    for (i in seq_along(all)) { #each loop is a single HR
      p()
      biomasses <- all[[i]]$Biomasses
      model_vals <- sapply(biomasses, function(df) df$Model_annual_mean[idx])
      baseline <- baseline_df$threshold[(nrow(baseline_df) - (length(all) - 1)):nrow(baseline_df)]
      
      if (model_vals[1] >= baseline[1]) {
        recovery_time$HR[i] <- HR[i]
        recovery_time$Recovery_Time[i] <- 0
      } else {
        recovery_time$HR[i] <- HR[i]
        recovery_time$Recovery_Time[i] <- which(model_vals >= baseline)[1]
      }
      
      n_years <- length(all[[i]]$Network_Indicators) # how many years are there
      bird_vals <- sapply(all[[i]]$Network_Indicators, function(df) df$NetworkData[63]) # extract omnivory
      baseline_bird <- tail(baseline_df$bird_omnivory, n_years) # extract the baseline values
      years_vector <- tail(baseline_df$year, n_years) # take the years as well
      
      lower_bound <- baseline_bird * 0.8 # what's the bottom of our ribbon?
      upper_bound <- baseline_bird * 1.2 # what's the top?
      in_band <- bird_vals >= lower_bound & bird_vals <= upper_bound # which values are in there? ie recovery time = 0
      
      # Assume crash year is 2020 (i.e., first year of trajectory)
      crash_year <- start_year
      
      if (in_band[1]) {
        recovery_time$Recovery_Time_bird[i] <- 0 # if first value is already in band, then we start recovered
      } else if (any(in_band)) {
        recovery_year <- years_vector[which(in_band)[1]] # recovery = first year in which we are in the band...
        recovery_time$Recovery_Time_bird[i] <- recovery_year - crash_year # then time is that year - the year we crashed
      } else {
        recovery_time$Recovery_Time_bird[i] <- NA # else it doesn't recover
      }
      
    }
  })
  
  # Save result
  if (is.null(save_path)) {
    save_path <- file.path(
      "../Objects/Experiments/Crash/Paper",
      paste0("Recovery_Time_", gsub(" ", "_", description), "_",
             format(threshold_multiplier, nsmall = 1), "_", start_year, ".RDS")
    )
  }
  
  saveRDS(recovery_time, save_path)
  
  # Return plot
  p <- ggplot(recovery_time, aes(x = HR, y = Recovery_Time)) +
    geom_point() +
    geom_line() +
    labs(x = "Harvest Rate", y = "Recovery Time (years)",
         title = paste("Recovery Time for", description,"in",start_year)) +
    theme_minimal(base_size = 14)
  q <- ggplot(recovery_time, aes(x = HR, y = Recovery_Time_bird)) +
    geom_point() +
    geom_line() +
    labs(x = "Harvest Rate", y = "Recovery Time (years)",
         title = paste("Recovery Time for Bird Omnivory Index in",start_year)) +
    theme_minimal(base_size = 14)
  return(p + q)
}

map(c(2070), function(.x) {
  calculate_recovery_time(
    all_file = paste0("../Objects/Experiments/Crash/Paper/Recovery_Time_Road_To_Recovery_",.x,".RDS"),
    baseline_file = "../Objects/Experiments/Baseline/Baseline_0_fishing_Demersal_fish_1year.RDS",
    description = "Demersal_fish",
    threshold_multiplier = 0.8,
    start_year = .x
  )
})
