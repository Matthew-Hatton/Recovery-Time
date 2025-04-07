rm(list = ls())
library(patchwork)
library(tidyverse)

transient_years <- seq(2010,2050)
all_data <- readRDS("../Recovery Time Manuscript/Objects/Shifting_Baseline_Decadal.RDS")