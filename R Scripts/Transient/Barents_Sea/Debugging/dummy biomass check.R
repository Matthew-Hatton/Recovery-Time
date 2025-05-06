rm(i)
no_fishing <- top_level_smooth[[1]][["Biomasses"]]
fishing <- top_level_smooth[[2]][["Biomasses"]]

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
  
  # # Convert to long format
  # biomass_long <- biomass_df %>%
  #   pivot_longer(cols = -FishingRate, names_to = "Year", values_to = "Biomass") %>% 
  #   mutate(Guild = top_level[[1]][["Biomasses"]][["2010"]]$Description[i] # add the name - ordering the same each year
  #   )
  
  biomass_smooth_long <- biomass_smooth_df %>%
    pivot_longer(cols = -FishingRate, names_to = "Year", values_to = "Biomass") %>% 
    mutate(Guild = top_level_smooth[[1]][["Biomasses"]][["2010"]]$Description[i] # add the name - ordering the same each year
    )
  # store
  # final_changing <- rbind(final_changing,biomass_long)
  final_changing_smooth <- rbind(final_changing_smooth,biomass_smooth_long)
}
