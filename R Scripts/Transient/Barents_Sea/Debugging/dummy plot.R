#rm(list = ls())
library(patchwork)
library(tidyverse)

transient_years <- seq(2010,2050)

useful <- c("Omnivorous_zooplankton",
            "Carnivorous_zooplankton",
            "Planktivorous_fish",
            "Migratory_fish",
            "Demersal_fish",
            "Birds",
            "Pinnipeds",
            "Cetaceans",
            "Maritime_mammals") # Name the guilds we will actually care about

top_level_smooth <- readRDS("../Objects/Individual Crash Demersal Fish.RDS")

final_changing_smooth <- data.frame(FishingRate = NA,
                                    Year = NA,
                                    Biomass = NA,
                                    Guild = NA)

for (i in 1:length(top_level_smooth[[1]][["Biomasses"]])) {

  biomass_smoothed <- sapply(transient_years, function(y) {
    sapply(top_level_smooth, function(tl) tl[["Biomasses"]][[as.character(y)]]$Model_annual_mean[i])
  })

  colnames(biomass_smoothed) <- transient_years
  
  biomass_smooth_df <- biomass_smoothed %>% as.data.frame()
  biomass_smooth_df$FishingRate <- 0:(length(top_level_smooth)-1)
  
  biomass_smooth_long <- biomass_smooth_df %>%
    pivot_longer(cols = -FishingRate, names_to = "Year", values_to = "Biomass") %>% 
    mutate(Guild = top_level_smooth[[1]][["Biomasses"]][["2010"]]$Description[i] # add the name - ordering the same each year
    )

  final_changing_smooth <- rbind(final_changing_smooth,biomass_smooth_long)
}


biomass_smooth_guilds <- final_changing_smooth %>% filter(Guild %in% useful)

crashed_0x_return_smooth <- biomass_smooth_guilds %>% filter(FishingRate == 0) #take 0x fishing

#Have to turn on what you need here
ggplot() +
  geom_line(data = crashed_0x_return_smooth,aes(x = as.numeric(Year),y = Biomass),color = "black",alpha = 1) +
  labs(x = "Year",
       y = "Guild Biomass",
       color = "Fishing Reintroduction Rate\n (DFHR and PFHR)") +
  theme(legend.position = "top") +
  facet_wrap(~ Guild,scales = "free_y") +
  # labs(caption = 
  #     "Maroon: Crashed System - Recovered at 0x Fishing. Red/Dashed: No Crash - 0x Fishing \n
  #      Light Blue: Crashed System - Recovered at 1x Fishing. Dark Blue/Dashed: No Crash - 1x Fishing \n
  #      Dark Green: No Crash - 10x Fishing (where we would be if we continue to fish at crash level)"
  #      ) +
  labs(caption =
         "Solid: Crashed System - Recovered at 0x Fishing. Dashed: No Crash - 0x Fishing",
       title = "Demersal Fish Crash"
  ) +
  theme(plot.caption = element_text(size = 6)) +
  NULL
# ggsave("../Figures/Preliminary/all_together_with_baseline.png",
#        height = 1080,
#        width = 1920,
#        units = "px",
#        dpi = 200)


