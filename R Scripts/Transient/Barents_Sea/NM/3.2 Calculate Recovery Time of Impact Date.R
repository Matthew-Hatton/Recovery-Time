## Calculate Recovery Time based on date of impact

#### Setup ####
rm(list=ls())                                                                                              # Wipe the brain
Packages <- c("MiMeMo.tools", "exactextractr", "raster", "lubridate","StrathE2EPolar","furrr","tictoc","progressr")                     # List packages
lapply(Packages, library, character.only = TRUE)   
# source("../@_Region_file_BS.R")
handlers("cli")
handlers(global = TRUE)

plan(multisession,workers = availableCores()-1) # parallel processing is good, but not that good

transient_years <- seq(2020,2089)
baseline <- readRDS("../Objects/Experiments/Baseline/Baseline_0_fishing_Demersal_fish.RDS")
# Keep only years 2023 and beyond
# baseline[["Biomasses"]] <- baseline[["Biomasses"]][names(baseline[["Biomasses"]]) >= "2010"]

baseline_vec <- c()
for (k in 1:length(baseline[["Biomasses"]])) {
  baseline_vec <- append(baseline_vec,baseline[["Biomasses"]][[k]]$Model_annual_mean[27])
}

baseline_df <- data.frame(baseline = baseline_vec,
                          MSC = baseline_vec * 0.8,
                          year = transient_years[1:length(baseline_vec)])


all <- readRDS("../Objects/Experiments/Rolling Crash/Rolling_Crash_base_MSY_2xMSY_Demersal_crash.RDS")
transient_base <- imap(all[[1]][["Biomasses"]], ~ {
  crash_year <- as.integer(.y)
  imap(.x,~ mutate(.x, crash_year = crash_year,
                   current_year = as.integer(.y)))
}) # adds new column to each entry which has the crash year

transient_MSY <- imap(all[[2]][["Biomasses"]], ~ {
  crash_year <- as.integer(.y)
  imap(.x,~ mutate(.x, crash_year = crash_year,
                   current_year = as.integer(.y)),
       baseline = baseline_vec,
       MSC = baseline_vec * 0.8)
})

transient_2x_MSY <- imap(all[[3]][["Biomasses"]], ~ {
  crash_year <- as.integer(.y)
  imap(.x,~ mutate(.x, crash_year = crash_year,
                   current_year = as.integer(.y),
                   baseline = baseline_vec,
                   MSC = baseline_vec * 0.8))
})

## For each list element, compute recovery time, leaves with just a dataframe of:
## 'crash_year', 'Recovery Time', 'Fishing Rate'

for (i in 1:length(transient_base)) {
  
  ## DEBUG
  i <- 1
  current <- transient_base[[i]] %>% 
    do.call(rbind.data.frame, .) %>% 
    filter(Description == "Demersal_fish" & current_year <= 2089)
  base <- baseline_df %>% filter(year %in% current$current_year)
  both <- cbind(current,base)

}
