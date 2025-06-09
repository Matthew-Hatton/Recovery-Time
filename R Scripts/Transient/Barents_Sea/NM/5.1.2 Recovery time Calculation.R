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
  
  handlers("cli")  # Enable progress bar
  
  # Define years and HR values
  transient_years <- seq(2020,2099)
  HR <- seq(0, 5.6, 0.2)
  
  # Load input data
  all <- readRDS(all_file)
  baseline_non_ss <- readRDS(baseline_file)
  
  # Find the index for the specified guild description
  example_df <- baseline_non_ss[["Biomasses"]][[1]]
  if (!"Description" %in% names(example_df)) stop("Missing 'Description' in Biomasses data frame.")
  idx <- which(example_df$Description == description)
  if (length(idx) == 0) stop(paste("Description", description, "not found."))
  
  # Create baseline time series and apply threshold
  baseline_df <- data.frame(
    year = transient_years[1:length(baseline_non_ss[["Biomasses"]])],
    baseline = map_dbl(baseline_non_ss[["Biomasses"]], ~ .x$Model_annual_mean[idx])
  ) %>%
    mutate(threshold = baseline * threshold_multiplier)
  
  # Initialize output
  recovery_time <- data.frame(HR = rep(0, length(all)),
                              Recovery_Time = rep(NA_integer_, length(all)))
  
  # Run analysis with progress bar
  with_progress({
    p <- progressor(steps = length(all))
    for (i in seq_along(all)) {
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
  
  return(p)
}


calculate_recovery_time(
  all_file = "../Objects/Experiments/Crash/Paper/Recovery_Time_Road_To_Recovery.RDS",
  baseline_file = "../Objects/Experiments/Baseline/Baseline_0_fishing_Demersal_fish_1year.RDS",
  description = "Demersal_fish",
  threshold_multiplier = 0.8,
  start_year = 2020
)
