## Run batches of R scripts. Handy if you want scripts to run after another finishes while you're away from the machine
rm(list = ls()) #reset

library(tidyverse)
library(MiMeMo.tools)
library(tictoc)

#### Batch process scripts ####

scripts <- c(                                           # List scripts in the order you want to run them
  # "./R scripts/Transient/Barents_Sea/NM/1.1. Decadal Smoothed Transients.R",
  # "./R scripts/Transient/Barents_Sea/NM/1.2. Decadal Smoothed Transients.R",
  "./R scripts/Transient/Barents_Sea/NM/2.1 Changing impact date.R",
  # "./R scripts/Transient/Barents_Sea/NM/2.2 Calculate Recovery Time of Impact Date.R",
  # "./R scripts/Transient/Barents_Sea/NM/3.1 Extract Bird Omnivory Index.R",
  # "./R scripts/Transient/Barents_Sea/NM/3.2 Flow into Birds.R",
  # "./R scripts/Transient/Barents_Sea/NM/3.3 Bird Biomass.R",
  # "./R scripts/Transient/Barents_Sea/NM/4.1. Coupled Effects Baseline.R",
  # "./R scripts/Transient/Barents_Sea/NM/4.2. Coupled Effects MSY.R",
  # "./R scripts/Transient/Barents_Sea/NM/4.3. Coupled Effects 2x_MSY.R",
  # "./R scripts/Transient/Barents_Sea/NM/5.1.1 Recovery time Extraction.R",
  # "./R scripts/Transient/Barents_Sea/NM/6.1.1 Consistent Fishing Extraction.R",
  # "./R scripts/Transient/Barents_Sea/NM/6.1.2 Intermittent Fishing Extraction.R",
  "./R scripts/Transient/Barents_Sea/NM/5.1.3 FIGURE 5 BIRD TRAJECTORY.R"
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