## Calculate recovery time from pre-post crash experiment (2.1)

rm(list = ls()) # reset

Packages <- c("MiMeMo.tools", "exactextractr", "raster", "lubridate","StrathE2EPolar","furrr","tictoc")                     # List packages
lapply(Packages, library, character.only = TRUE)   
source("../@_Region_file_BS.R")

transient_years <- seq(2023,2089)
pre_post <- readRDS("../Objects/Experiments/Crash/PRE_POST_Demersal_crash.RDS")

## Need to also have a vector for the 80% threshold
#baseline_1x <- readRDS("../Objects/Experiments/Baseline/1X_Baseline_1_fishing_Demersal_fish.RDS")

baseline_1x <- readRDS("../Objects/Experiments/Baseline/Baseline_0_fishing_Demersal_fish.RDS")


# Keep only years 2023 and beyond
baseline_1x[["Biomasses"]] <- baseline_1x[["Biomasses"]][names(baseline_1x[["Biomasses"]]) >= "2023"]

baseline_1x_vec <- c()
for (k in 1:length(transient_years)) {
  baseline_1x_vec <- append(baseline_1x_vec,baseline_1x[["Biomasses"]][[k]]$Model_annual_mean[27])
}

baseline_1x_df <- data.frame(baseline = baseline_1x_vec,
                             MSC = baseline_1x_vec * 0.8)


master <- list() # where to store?

for (i in 1:length(pre_post)) {
   # i <- 1
  
  current <- pre_post[[i]]
  combination <- data.frame(year = transient_years,
                            pre = rep(current[["Combination"]]$pre,length(transient_years)),
                            post = rep(current[["Combination"]]$post,length(transient_years)))
  biomasses <- c()
  for (j in 1:length(transient_years)) {
    biomasses <- append(biomasses,current[["Biomasses"]][[j]]$Model_annual_mean[27])
  }
  final <- cbind(combination,biomasses,baseline_1x_df)
  master[[i]] <- final
}

pre <- c()
post <- c()
recovery <- c()

for (i in seq_along(master)) {
  current <- master[[i]]
  
  pre <- append(pre, current$pre[1])
  post <- append(post, current$post[1])
  
  threshold <- 1 * current$MSC
  total_years <- nrow(current)
  
  # CASE: Starts above MSC
  if (current$biomasses[1] >= threshold[1]) {
    above_count <- sum(current$biomasses >= threshold)
    
    if (above_count / total_years > 0.5) {
      recovery <- append(recovery, 0)  # stable
    } else {
      # Determine first sustained drop
      below_years <- current %>% filter(biomasses < threshold)
      if (nrow(below_years) > 0) {
        collapse_year <- below_years$year[1]
        recovery <- append(recovery, -(collapse_year - 2023))
      } else {
        recovery <- append(recovery, NA)
      }
    }
    
  } else {
    # CASE: Starts below → look for recovery year
    recovery_row <- current %>% filter(biomasses >= threshold) %>% slice(1)
    
    if (nrow(recovery_row) == 0) {
      # Never recovers
      recovery <- append(recovery, NA)
    } else {
      recovery_year <- recovery_row$year[1]
      after_recovery <- current %>% filter(year >= recovery_year)
      years_above <- sum(after_recovery$biomasses >= 0.99 * after_recovery$MSC)
      years_total <- nrow(after_recovery)
      
      if (years_above / years_total > 0.5) {
        recovery <- append(recovery, recovery_year - 2023)
      } else {
        recovery <- append(recovery, NA)
      }
    }
  }
}

# Final dataframe
recovery_time <- data.frame(
  Recovery = recovery,
  pre = pre,
  post = post
)

ggplot() +
  geom_raster(data = recovery_time,aes(x = pre,y = post,fill = Recovery),
              interpolate = F) + 
  scale_fill_viridis_c() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0))

############ DEBUG ############

debug <- data.frame(Year = transient_years,
                    baseline = baseline_1x_df)
recov <- c()
for (i in 1:length(transient_years)) {
  recov <- append(recov,foo[["Biomasses"]][[i]]$Model_annual_mean[27])
}
debug$recov <- recov

weird <- master[[54]]
ggplot() + 
  geom_line(data = weird,aes(x = year,y = biomasses)) +
  geom_line(data = weird,aes(x = year,y = baseline),linetype = "dashed") +
  geom_line(data = weird,aes(x = year,y = MSC),linetype = "dashed",color = "red")
## value drops below