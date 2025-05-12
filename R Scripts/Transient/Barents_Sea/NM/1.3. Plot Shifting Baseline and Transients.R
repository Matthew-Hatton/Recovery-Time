rm(list = ls())
library(patchwork)
library(tidyverse)
library(purrr)

transient_years <- seq(2010,2089)
useful <- c(
  #Omnivorous_zooplankton",
            #"Carnivorous_zooplankton",
            "Planktivorous_fish",
            #"Migratory_fish",
            "Demersal_fish",
            #"Birds",
            #"Pinnipeds",
            #"Cetaceans",
            #"Maritime_mammals",
            NULL
  ) # Name the guilds we will actually care about

transient_list <- readRDS("../Objects/Experiments/Crash/MULTIPLE_Demersal_crash.RDS")
transient_msy_0 <- transient_list[[1]]
transient_msy_1 <- transient_list[[2]]

baseline_0x <- readRDS("../Objects/Experiments/Baseline/50_Baseline_0_fishing_Demersal_fish.RDS")
baseline_1x <- readRDS("../Objects/Experiments/Baseline/1X_Baseline_1_fishing_Demersal_fish.RDS")
# Extract biomasses

transient_bio_10 <- readRDS("../Objects/Experiments/Crash/Demersal_crash_10_relax_0.RDS") %>% 
  .[["Biomasses"]] %>% 
  map2_dfr(., transient_years, ~ 
             .x %>%
             mutate(Year = .y,
                    Relax = 0) %>%
             dplyr::select(-Units)) %>% 
  filter(Description %in% useful) %>% 
  filter(Year >= 2010)

transient_years <- seq(2023,2089)
transient_bio_msy_0 <- transient_msy_0[["Biomasses"]] %>% 
  map2_dfr(., transient_years, ~ 
             .x %>%
             mutate(Year = .y,
                    Relax = 0) %>%
             dplyr::select(-Units)) %>% 
  filter(Description %in% useful)

transient_bio_msy_1 <- transient_msy_1[["Biomasses"]] %>% 
  map2_dfr(., transient_years, ~ 
             .x %>%
             mutate(Year = .y,
                    Relax = 1) %>%
             dplyr::select(-Units)) %>% 
  filter(Description %in% useful)

transient_years <- seq(2010,2089)
baseline_bio <- baseline_0x[["Biomasses"]] %>% 
  map2_dfr(., transient_years, ~ 
             .x %>%
             mutate(Year = .y,
                    Relax = 0,
                    MSC = Model_annual_mean * 0.8) %>%
             dplyr::select(-Units)) %>% 
  filter(Description %in% useful) %>% 
  filter(Year >= 2023)

baseline_1x_bio <- baseline_1x[["Biomasses"]] %>% 
  map2_dfr(., transient_years, ~ 
             .x %>%
             mutate(Year = .y,
                    Relax = 1,
                    MSC = Model_annual_mean * 0.8) %>%
             dplyr::select(-Units)) %>% 
  filter(Description %in% useful) %>% 
  filter(Year >= 2023)



ggplot() +
  geom_line(data = transient_bio_msy_0,aes(x = Year,y = Model_annual_mean,color = as.character(Relax)),alpha = 0.8) +
  geom_line(data = transient_bio_msy_1,aes(x = Year,y = Model_annual_mean,color = as.character(Relax)),alpha = 0.8) +
  # geom_line(data = baseline_1x_bio,aes(x = Year,y = Model_annual_mean,color = as.character(Relax)),alpha = 0.8) + # continue as is
  geom_line(data = baseline_bio,aes(x = Year,y = Model_annual_mean),alpha = 0.8,color = "black") + # stop all fishing
  # geom_line(data = baseline_1x_bio,aes(x = Year,y = MSC),alpha = 0.3,color = "black") + # continue as is
  geom_line(data = baseline_bio,aes(x = Year,y = MSC),alpha = 0.3,color = "black") + # stop all fishing  - MSC line
  geom_vline(xintercept = 2043,alpha = 0.1,linetype = "dashed") +
       labs(y = "Guild Biomass",
       color = "Post-MSY Fishing Rate\n (DFHR)") +
  theme(legend.position = "top") +
  facet_wrap(~ Description,scales = "free_y",ncol = 1,axes = "all_y") +
  theme(plot.caption = element_text(size = 6)) +
  NULL
ggsave("../Figures/Transient/Barents_Sea/Fishing at MSY_PF_DF.png",
       height = 1080,
       width = 1920,
       units = "px",
       dpi = 200)
