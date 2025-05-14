rm(list = ls())
library(patchwork)
library(tidyverse)
library(purrr)

transient_years <- seq(2020,2099-10) # define transient years. cutoff is -10 due to decadal average
useful <- c(
  #Omnivorous_zooplankton",
            #"Carnivorous_zooplankton",
            # "Planktivorous_fish",
            #"Migratory_fish",
            "Demersal_fish",
            #"Birds",
            #"Pinnipeds",
            #"Cetaceans",
            #"Maritime_mammals",
            NULL
  ) # Name the guilds we will actually care about and want to plot

transient_list <- readRDS("../Objects/Experiments/Crash/Paper/base_MSY_2xMSY_Demersal_crash.RDS")
transient_base <- transient_list[[1]]
transient_msy <- transient_list[[2]]
transient_2x <- transient_list[[3]]

baseline_0x <- readRDS("../Objects/Experiments/Baseline/Baseline_0_fishing_Demersal_fish.RDS")
# Extract biomasses

transient_base_bio <- transient_base[["Biomasses"]] %>% 
  map2_dfr(., transient_years, ~ 
             .x %>%
             mutate(Year = .y,
                    HR = "Present Day") %>%
             dplyr::select(-Units)) %>% 
  filter(Description %in% useful)

transient_MSY_bio <- transient_msy[["Biomasses"]] %>% 
  map2_dfr(., transient_years, ~ 
             .x %>%
             mutate(Year = .y,
                    HR = "MSY") %>%
             dplyr::select(-Units)) %>% 
  filter(Description %in% useful)

transient_2x_MSY_bio <- transient_2x[["Biomasses"]] %>% 
  map2_dfr(., transient_years, ~ 
             .x %>%
             mutate(Year = .y,
                    HR = "2x MSY") %>%
             dplyr::select(-Units)) %>% 
  filter(Description %in% useful)

baseline_bio <- baseline_0x[["Biomasses"]] %>% 
  map2_dfr(., transient_years, ~ 
             .x %>%
             mutate(Year = .y,
                    Relax = 0,
                    MSC = Model_annual_mean * 0.8) %>%
             dplyr::select(-Units)) %>% 
  filter(Description %in% useful) %>% 
  filter(Year >= 2020)

transient_all <- rbind(transient_base_bio,transient_MSY_bio,transient_2x_MSY_bio)


ggplot() +
  geom_line(data = transient_all,aes(x = Year,y = Model_annual_mean,color = HR),alpha = 0.8) +
  geom_line(data = baseline_bio,aes(x = Year,y = Model_annual_mean),alpha = 0.8,color = "black") + # stop all fishing
  #geom_line(data = baseline_bio,aes(x = Year,y = MSC),alpha = 0.3,color = "black") + # stop all fishing  - MSC line
  geom_vline(xintercept = 2040,linetype = "dashed") +     
  labs(y = "Demersal Fish Biomass (mmN/m2)",
       color = "Fishing Rate\n (DFHR)") +
  theme(legend.position = "top") +
  #facet_wrap(~ Description,scales = "free_y",ncol = 1,axes = "all_y") +
  theme(plot.caption = element_text(size = 6)) +
  NULL
ggsave("../Figures/Transient/Barents_Sea/NM/Draft 1/Figure 1.png",
       height = 1080,
       width = 1920,
       units = "px",
       dpi = 200)
