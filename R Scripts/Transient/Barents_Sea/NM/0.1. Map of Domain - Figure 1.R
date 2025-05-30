## Script to plot the model domain

#### Setup ####
rm(list=ls()) # reset
Packages <- c("MiMeMo.tools", "exactextractr", "raster", "lubridate",
              "StrathE2EPolar","furrr","tictoc","progressr")    # List packages
lapply(Packages, library, character.only = TRUE)   
source("../@_Region_file_BS.R")
handlers(global = T)
handlers("cli") # progress bar

