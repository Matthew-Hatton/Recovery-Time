## Script to plot the intermittent vs. consistent fishing

rm(list = ls())
library(patchwork)
library(tidyverse)
library(purrr)
library(progressr)

transient_years <- seq(2020,2099)

all_fishing_consistent <- readRDS("../Objects/Experiments/Intermittent_Consistent/Consistent_1x_fishing_Demersal_fish.RDS")
all_fishing_intermittent <- readRDS("../Objects/Experiments/Intermittent_Consistent/Intermittent_2.6x_fishing_Demersal_fish.RDS")
baseline_non_ss <- readRDS("../Objects/Experiments/Baseline/Baseline_0_fishing_Demersal_fish_1year.RDS")

baseline_non_ss_df <- data.frame(
  year = transient_years[1:length(baseline_non_ss[["Biomasses"]])],
  baseline = map_dbl(baseline_non_ss[["Biomasses"]], ~ .x$Model_annual_mean[27])) %>% #extract DF biomass
  mutate(MSC = baseline * 0.8)


## Biomasses
consistent_fishing <- data.frame(
  year = transient_years[1:length(all_fishing_consistent[["Biomasses"]])],
  biomass = map_dbl(all_fishing_consistent[["Biomasses"]], ~ .x$Model_annual_mean[27])) %>% 
  mutate(Harvest_Rate = "Consistent 2020s Fishing")

intermittent_fishing <- data.frame(
  year = transient_years[1:length(all_fishing_intermittent[["Biomasses"]])],
  biomass = map_dbl(all_fishing_intermittent[["Biomasses"]], ~ .x$Model_annual_mean[27])) %>% 
  mutate(Harvest_Rate = "Intermittent MSY")

fishing <- rbind(consistent_fishing,intermittent_fishing)

biomass <- ggplot() +
  geom_line(
    data = baseline_non_ss_df,
    aes(x = year, y = baseline), inherit.aes = FALSE,alpha = 1,color = "black"
  ) +
  geom_ribbon(data = baseline_non_ss_df,
              aes(x = year,ymin = baseline - (baseline * 0.2),ymax = baseline),
              alpha = 0.1) +
  # geom_line(data = consistent_fishing, aes(x = year, y = biomass),color = "#2DA6D2") +
  # geom_line(data = intermittent_fishing, aes(x = year, y = biomass),color = "#D2592D") + # these colors are nice, but not good for this stuff
  geom_line(data = fishing, aes(x = year, y = biomass,color = Harvest_Rate)) +
  scale_color_manual(values = c("#00BA38","#2DA6D2")) +
  labs(
    x = "Year", y = "Demersal Fish Biomass",
    color = "Harvest Rate"
    ) +
  scale_y_continuous(limits = c(0,10)) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"),
        legend.position = "top",
        legend.text = element_text(size = 12),
        axis.text.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        panel.grid.minor = element_blank()) +
  NULL
# biomass

## Landings

demersal_rows <- c(2, 3)  # Demersal location
years <- names(all_fishing_consistent$inshore_land_mat)

# Get all years from one of the lists
demersal_landings_consistent <- map_dfr(years, function(yr) {
  inshore_mat <- all_fishing_consistent$inshore_land_mat[[yr]]
  offshore_mat <- all_fishing_consistent$offshore_land_mat[[yr]]
  
  total_landings <- sum(inshore_mat[demersal_rows, ], na.rm = TRUE) +
    sum(offshore_mat[demersal_rows, ], na.rm = TRUE)
  
  data.frame(
    year = as.integer(yr),
    annual_demersal_landings = total_landings
  )
}) %>%
  arrange(year) %>%
  mutate(cumulative_demersal_landings = cumsum(annual_demersal_landings),
         Harvest_Rate = "Consistent 2020s Fishing")

demersal_landings_intermittent <- map_dfr(years, function(yr) {
  inshore_mat <- all_fishing_intermittent$inshore_land_mat[[yr]]
  offshore_mat <- all_fishing_intermittent$offshore_land_mat[[yr]]
  
  total_landings <- sum(inshore_mat[demersal_rows, ], na.rm = TRUE) +
    sum(offshore_mat[demersal_rows, ], na.rm = TRUE)
  
  data.frame(
    year = as.integer(yr),
    annual_demersal_landings = total_landings
  )
}) %>%
  arrange(year) %>%
  mutate(cumulative_demersal_landings = cumsum(annual_demersal_landings),
         Harvest_Rate = "Intermittent MSY")

demersal_landings <- rbind(demersal_landings_consistent,
                           demersal_landings_intermittent)

landings_cumulative <- ggplot() +
  geom_line(data = demersal_landings, aes(x = year, y = cumulative_demersal_landings,color = Harvest_Rate)) +
  #geom_line(data = demersal_landings, aes(x = year, y = annual_demersal_landings,color = Harvest_Rate),alpha = 0.5) +
  scale_color_manual(values = c("#00BA38","#2DA6D2")) +
  labs(
    x = "Year", y = "Cumulative\n Demersal Fish Landings",
    color = "Harvest Rate") +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"),
        legend.position = "top",
        legend.text = element_text(size = 12),
        axis.text.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        panel.grid.minor = element_blank()) +
  NULL
# landings_cumulative

landings_annual <- ggplot() +
  #geom_line(data = demersal_landings, aes(x = year, y = cumulative_demersal_landings,color = Harvest_Rate)) +
  geom_line(data = demersal_landings, aes(x = year, y = annual_demersal_landings,color = Harvest_Rate)) +
  scale_color_manual(values = c("#00BA38","#2DA6D2")) +
  labs(
    x = "Year", y = "Annual\n Demersal Fish Landings",
    color = "Harvest Rate") +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"),
        legend.position = "top",
        legend.text = element_text(size = 12),
        axis.text.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        panel.grid.minor = element_blank()) +
  NULL
# landings_annual

biomass / landings_cumulative / landings_annual + plot_layout(guides = "collect", axes = "collect_x") + plot_annotation(
  caption = "All values measured in mmN/m²",
  theme = theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12)
  )
) & theme(
  legend.position = "top",              # Legend at the top of the plot
  legend.direction = "horizontal",      # Legend entries flow horizontally
  legend.box = "vertical",              # Stack the title ABOVE the entries
  legend.title = element_text(face = "bold", size = 12),
  legend.text = element_text(size = 10)
)



ggsave("../Figures/Transient/Barents_Sea/NM/Draft 1/Figure 6/Figure 6.png",
       height = 1080,
       width = 1920,
       units = "px",
       dpi = 200,
       bg = "white")
