rm(list = ls())
library(patchwork)
library(tidyverse)

all_guild_biomass <- readRDS("./Objects/Barents_Sea initial biomass.RDS")

B_initial <- all_guild_biomass$Model_annual_mean[27] # Get initial Biomass DF

top_level <- readRDS("./Objects/TRANSIENT_TEST.RDS")
transient_years <- seq(2010,2050)

biomass <- sapply(transient_years, function(y) {
  sapply(top_level, function(tl) tl[["Biomasses"]][[as.character(y)]]$Model_annual_mean[27])
})

colnames(biomass) <- transient_years

# Convert the matrix to a dataframe
biomass_df <- as.data.frame(biomass)
biomass_df$FishingRate <- 0:3  # Assign fishing rates (corresponding to list indices 1-4)

# Convert to long format
biomass_long <- biomass_df %>%
  pivot_longer(cols = -FishingRate, names_to = "Year", values_to = "Biomass")

ggplot() +
  geom_line(data = biomass_long,aes(x = as.numeric(Year),y = Biomass,color = as.character(FishingRate),group = FishingRate)) +
  geom_hline(yintercept = B_initial,linetype = "dashed",color = "black") +
  #geom_hline(yintercept = B_X,linetype = "dashed",color = "darkred") +
  #geom_vline(xintercept = 2025,linetype = "dashed") +
  # geom_vline(xintercept = 2040,linetype = "dashed") +
  #geom_vline(xintercept = 2050,linetype = "dashed") +
  #stat_smooth(data = df_bio,aes(x = Year,y = Biomass),alpha = 0.6,geom = "line",color = "blue",span = 0.4) +
  labs(x = "Year",
       y = "Demersal Fish Biomass",
       color = "Fishing Reintroduction Rate\n (DFHR and PFHR)") +
  theme(legend.position = "top") +
  NULL
ggsave("../Recovery Time Manuscript/Figures/Preliminary/DF Biomass.png")

network <- sapply(transient_years, function(y) {
  sapply(top_level, function(tl) tl[["Network_Indicators"]][[as.character(y)]]["bird_omnivoryindex",])
})
colnames(network) <- transient_years

network_df <- as.data.frame(network)
network_df$FishingRate <- 0:3  # Assign fishing rates (corresponding to list indices 1-4)

# Convert to long format
network_long <- network_df %>%
  pivot_longer(cols = -FishingRate, names_to = "Year", values_to = "network")

bird <- ggplot() +
  geom_line(data = network_long,aes(x = as.numeric(Year),y = network,color = as.character(FishingRate),group = FishingRate)) +
  scale_y_continuous(limits = c(0,1.3)) +
  labs(x = "Year",
       y = "Bird Omnivery Index",
       color = "Fishing Reintroduction Rate") +
  geom_vline(xintercept = 2025,linetype = "dashed") +
  geom_vline(xintercept = 2040,linetype = "dashed") +
  geom_hline(yintercept = 6.840249e-01,linetype = "dashed") +
  theme(legend.position = "top") +
  NULL

bird
ggsave("../Recovery Time Manuscript/Figures/Preliminary/bird omniv.png")
