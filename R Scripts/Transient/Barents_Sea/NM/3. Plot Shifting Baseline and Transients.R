rm(list = ls())
library(patchwork)
library(tidyverse)

transient_years <- seq(2010,2099)
useful <- c("Omnivorous_zooplankton",
            "Carnivorous_zooplankton",
            "Planktivorous_fish",
            "Migratory_fish",
            "Demersal_fish",
            "Birds",
            "Pinnipeds",
            "Cetaceans",
            "Maritime_mammals") # Name the guilds we will actually care about

transient <- readRDS("../Objects/Experiments/Crash/Demersal_crash_10_relax_0.RDS")
transient_bio <- transient[["Biomasses"]]
transient_df <- data.frame(Model_annual_mean = NA,
                           Description = NA,
                           Year = NA)

for (i in 1:length(transient[["Biomasses"]])) {
  biomass <- transient_bio[[i]] %>% # select yr
    mutate(Year = transient_years[i]) %>% # add yr col
    subset(select = -c(Units))

  # store
  transient_df <- rbind(transient_df,biomass)
}

biomass_guilds <- transient_df %>% filter(Description %in% useful)

ggplot() +
  geom_line(data = biomass_guilds,aes(x = Year,y = Model_annual_mean),color = "black",alpha = 0.8) +
       labs(y = "Guild Biomass",
       color = "Fishing Reintroduction Rate\n (DFHR and PFHR)") +
  theme(legend.position = "top") +
  facet_wrap(~ Description,scales = "free_y") +
  theme(plot.caption = element_text(size = 6)) +
  NULL
# ggsave("../Figures/Preliminary/all_together_with_baseline.png",
#        height = 1080,
#        width = 1920,
#        units = "px",
#        dpi = 200)


