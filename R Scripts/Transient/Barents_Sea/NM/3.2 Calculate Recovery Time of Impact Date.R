## Calculate Recovery Time based on date of impact

#### Setup ####
rm(list=ls())                                                                                              # Wipe the brain
Packages <- c("MiMeMo.tools", "exactextractr", "raster", "lubridate","StrathE2EPolar","furrr","tictoc","progressr")                     # List packages
lapply(Packages, library, character.only = TRUE)   
source("../@_Region_file_BS.R")
handlers("cli")
handlers(global = TRUE)

plan(multisession,workers = availableCores()-1) # parallel processing is good, but not that good

all <- readRDS("../Objects/Experiments/Rolling Crash/Rolling_Crash_base_MSY_2xMSY_Demersal_crash.RDS")
transient_base <- all[[1]]
transient_MSY <- all[[2]]
transient_2x_MSY <- all[[3]]
