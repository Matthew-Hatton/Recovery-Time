## Run batches of R scripts. Handy if you want scripts to run after another finishes while you're away from the machine
rm(list = ls()) #reset

library(tidyverse)
library(MiMeMo.tools)
library(tictoc)

#### Batch process scripts ####

scripts <- c(                                           # List scripts in the order you want to run them
  #"./R scripts/NE/1.1 NE EXTRACT BARENTS_SEA.R",
  #"./R scripts/NE/1.2 NE EXTRACT EAST_GREENLAND.R",
  #"./R scripts/NE/2.1 NE EXTRACT ICE BARENTS_SEA.R",
  #"./R scripts/NE/2.2 NE EXTRACT ICE EAST_GREENLAND.R",
  # "./R scripts/NE/3.1 NE EXTRACT DAILY BARENTS_SEA.R",
  # "./R scripts/NE/3.2 NE EXTRACT DAILY EAST_GREENLAND.R",
  # "./R scripts/Transient/Barents_Sea/SSP126/1.1. Yearly Transients.R",
  #"./R scripts/Transient/Barents_Sea/SSP126/1.2. Decadal Smoothed Transients.R",
  # "./R scripts/Transient/Barents_Sea/SSP126/2.1. Yearly Shifting Baseline.R",
  # "./R scripts/Transient/Barents_Sea/SSP126/2.2. Decadal Shifting Baseline.R"
  #"./R scripts/Transient/Barents_Sea/SSP126/3. Plot shifting Baseline and Transients.R",
  "./R scripts/Transient/Barents_Sea/NM/1.2. Decadal Smoothed Transients.R",
  "./R scripts/Transient/Barents_Sea/NM/2.2. Decadal Shifting Baseline.R"
) %>% 
  map(MiMeMo.tools::execute)                                                           # Run the scripts

# #### Plot run times ####

# timings <- tictoc::tic.log(format = F) %>%                                             # Get the log of timings
#   lapply(function(x) data.frame("Script" = x$msg, Minutes = (x$toc - x$tic)/60)) %>%   # Get a dataframe of scripts and runtimes in minutes
#   bind_rows() %>%                                                                      # Get a single dataframe
#   separate(Script, into = c(NA, "Script"), sep = "/R scripts/") %>%
#   separate(Script, into = c("Type", NA, NA), sep = "[.]", remove = F) %>%
#   mutate(Script = factor(Script, levels = Script[order(rownames(.), decreasing = T)])) # Order the scripts
# saveRDS(timings, "../Recovery Time Manuscript/Objects/Batch Run time.rds")