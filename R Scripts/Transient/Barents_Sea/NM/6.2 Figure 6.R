## Script to plot the intermittent vs. consistent fishing

rm(list = ls())
library(patchwork)
library(tidyverse)
library(purrr)
library(progressr)
set.seed(0710)

transient_years <- seq(2020,2099)

all_fishing_consistent <- readRDS("../Objects/Experiments/Intermittent_Consistent/Intermittent_1x_fishing_Demersal_fish.RDS")
all_fishing_intermittent_MSY <- readRDS("../Objects/Experiments/Intermittent_Consistent/Intermittent_2.6x_fishing_Demersal_fish.RDS")
all_fishing_intermittent_2x_MSY <- readRDS("../Objects/Experiments/Intermittent_Consistent/Intermittent_5.2x_fishing_Demersal_fish.RDS")
all_fishing_intermittent_2x_MSY_95 <- readRDS("../Objects/Experiments/Intermittent_Consistent/Intermittent_5.2x_fishing_Demersal_fish_95th_percentile.RDS")
baseline_non_ss <- readRDS("../Objects/Experiments/Baseline/Baseline_0_fishing_Demersal_fish_1year.RDS")
B_msy_data <- readRDS("../Objects/Experiments/Intermittent_Consistent/Consistent_2.6x_fishing_Demersal_fish.RDS") 
B_msy <- data.frame(
  year = transient_years[1:length(B_msy_data[["Biomasses"]])],
  biomass = map_dbl(B_msy_data[["Biomasses"]], ~ .x$Model_annual_mean[27]))  # pulls a vector of biomasses as MSY - will need to calculate the B_trigger and B_stop values
bounds <- data.frame(year = transient_years,
                     biomass = numeric(nrow(B_msy)),
                     upper = numeric(nrow(B_msy)),
                     lower = numeric(nrow(B_msy)))

for (i in 1:(nrow(B_msy))) {
  bounds$biomass[i] <- B_msy$biomass[i] # but this is B_msy
  bounds$upper[i] = quantile(rnorm(10000, mean=B_msy$biomass[i], sd=0.2),0.95)
  bounds$lower[i] = quantile(rnorm(10000, mean=B_msy$biomass[i], sd=0.2),0.05)
}
baseline_non_ss_df <- data.frame(
  year = transient_years[1:length(baseline_non_ss[["Biomasses"]])],
  baseline = map_dbl(baseline_non_ss[["Biomasses"]], ~ .x$Model_annual_mean[27])) %>% #extract DF biomass
  mutate(MSC = baseline * 0.8)


## Biomasses
consistent_fishing <- data.frame(
  year = transient_years[1:length(all_fishing_consistent[["Biomasses"]])],
  biomass = map_dbl(all_fishing_consistent[["Biomasses"]], ~ .x$Model_annual_mean[27])) %>% 
  mutate(Harvest_Rate = "Baseline")

intermittent_fishing_MSY <- data.frame(
  year = transient_years[1:length(all_fishing_intermittent_MSY[["Biomasses"]])],
  biomass = map_dbl(all_fishing_intermittent_MSY[["Biomasses"]], ~ .x$Model_annual_mean[27])) %>% 
  mutate(Harvest_Rate = "MSY")

intermittent_fishing_2x_MSY <- data.frame(
  year = transient_years[1:length(all_fishing_intermittent_2x_MSY[["Biomasses"]])],
  biomass = map_dbl(all_fishing_intermittent_2x_MSY[["Biomasses"]], ~ .x$Model_annual_mean[27])) %>% 
  mutate(Harvest_Rate = "2x MSY")

intermittent_fishing_2x_MSY_95 <- data.frame(
  year = transient_years[1:length(all_fishing_intermittent_2x_MSY_95[["Biomasses"]])],
  biomass = map_dbl(all_fishing_intermittent_2x_MSY_95[["Biomasses"]], ~ .x$Model_annual_mean[27])) %>% 
  mutate(Harvest_Rate = "2x MSY. 95th")

fishing <- rbind(consistent_fishing,intermittent_fishing_MSY,intermittent_fishing_2x_MSY,intermittent_fishing_2x_MSY_95)

color_scale <- scale_color_manual(
  values = c("Baseline" = "#1b9e77", "MSY" = "#7570b3", "2x MSY" = "#d95f02"),
  name = "Harvest Rate",
  labels = c("2020s Baseline","2020s MSY","2020s 2x MSY")
)

fishing$Harvest_Rate <- factor(fishing$Harvest_Rate, levels=c('Baseline', 'MSY', '2x MSY', "2x MSY. 95th")) # reorder legend

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
  geom_line(data = filter(fishing, Harvest_Rate != c("2x MSY. 95th")), aes(x = year, y = biomass,color = Harvest_Rate),linewidth = 0.9) +
  # geom_line(data = baseline_non_ss_df,aes(x = year,y = MSC),linetype = "dashed",alpha = 0.6) +
  geom_line(data = bounds,aes(x = year,y = lower),linetype = "dashed",alpha = 0.6) +
  color_scale +
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
         Harvest_Rate = "Baseline")

demersal_landings_intermittent_MSY <- map_dfr(years, function(yr) {
  inshore_mat <- all_fishing_intermittent_MSY$inshore_land_mat[[yr]]
  offshore_mat <- all_fishing_intermittent_MSY$offshore_land_mat[[yr]]

  total_landings <- sum(inshore_mat[demersal_rows, ], na.rm = TRUE) +
    sum(offshore_mat[demersal_rows, ], na.rm = TRUE)

  data.frame(
    year = as.integer(yr),
    annual_demersal_landings = total_landings
  )
}) %>%
  arrange(year) %>%
  mutate(cumulative_demersal_landings = cumsum(annual_demersal_landings),
         Harvest_Rate = "MSY")

demersal_landings_intermittent_2x_MSY <- map_dfr(years, function(yr) {
  inshore_mat <- all_fishing_intermittent_2x_MSY$inshore_land_mat[[yr]]
  offshore_mat <- all_fishing_intermittent_2x_MSY$offshore_land_mat[[yr]]
  
  total_landings <- sum(inshore_mat[demersal_rows, ], na.rm = TRUE) +
    sum(offshore_mat[demersal_rows, ], na.rm = TRUE)
  
  data.frame(
    year = as.integer(yr),
    annual_demersal_landings = total_landings
  )
}) %>%
  arrange(year) %>%
  mutate(cumulative_demersal_landings = cumsum(annual_demersal_landings),
         Harvest_Rate = "2x MSY")

demersal_landings_intermittent_2x_MSY_95 <- map_dfr(years, function(yr) {
  inshore_mat <- all_fishing_intermittent_2x_MSY_95$inshore_land_mat[[yr]]
  offshore_mat <- all_fishing_intermittent_2x_MSY_95$offshore_land_mat[[yr]]
  
  total_landings <- sum(inshore_mat[demersal_rows, ], na.rm = TRUE) +
    sum(offshore_mat[demersal_rows, ], na.rm = TRUE)
  
  data.frame(
    year = as.integer(yr),
    annual_demersal_landings = total_landings
  )
}) %>%
  arrange(year) %>%
  mutate(cumulative_demersal_landings = cumsum(annual_demersal_landings),
         Harvest_Rate = "2x MSY. 95th")

demersal_landings <- rbind(demersal_landings_consistent,
                           demersal_landings_intermittent_MSY,
                           demersal_landings_intermittent_2x_MSY,
                           demersal_landings_intermittent_2x_MSY_95)
demersal_landings$Harvest_Rate <- factor(demersal_landings$Harvest_Rate, levels=c('Baseline', 'MSY', '2x MSY', "2x MSY. 95th")) # reorder legend

landings_cumulative <- ggplot() +
  geom_line(data = filter(demersal_landings, Harvest_Rate != c("2x MSY. 95th")), aes(x = year, y = cumulative_demersal_landings,color = Harvest_Rate),linewidth = 0.9) +
  #geom_line(data = demersal_landings, aes(x = year, y = annual_demersal_landings,color = Harvest_Rate),alpha = 0.5) +
  color_scale +
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
  geom_line(data = filter(demersal_landings, Harvest_Rate != c("2x MSY. 95th")), aes(x = year, y = annual_demersal_landings,color = Harvest_Rate),linewidth = 0.9) +
  color_scale +
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

p_MSC_Recovery <- (biomass / landings_annual / landings_cumulative) +
  plot_layout(guides = "collect", axes = "collect_x") + plot_annotation(
  caption = "Simulated dynamic fisheries closures. Recovery to MSC 80% of unfished system threshold. All values measured in mmN/m².",
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
) +
  NULL

################### 95th percentile recovery

fishing_95 <- fishing %>% filter(Harvest_Rate != c("2x MSY")) %>% 
  mutate(Harvest_Rate = case_when(
    Harvest_Rate == "Baseline" ~ "Baseline",
    Harvest_Rate == "MSY" ~ "MSY",
    Harvest_Rate == "2x MSY. 95th" ~ "2x MSY"
  ))

demersal_landings_95 <- demersal_landings %>% filter(Harvest_Rate != c("2x MSY")) %>% 
  mutate(Harvest_Rate = case_when(
    Harvest_Rate == "Baseline" ~ "Baseline",
    Harvest_Rate == "MSY" ~ "MSY",
    Harvest_Rate == "2x MSY. 95th" ~ "2x MSY"
  ))

fishing_95$Harvest_Rate <- factor(fishing_95$Harvest_Rate, levels=c('Baseline', 'MSY', '2x MSY')) # reorder legend
demersal_landings_95$Harvest_Rate <- factor(demersal_landings_95$Harvest_Rate, levels=c('Baseline', 'MSY', '2x MSY')) # reorder legend

biomass <- ggplot() +
  geom_line(
    data = baseline_non_ss_df,
    aes(x = year, y = baseline), inherit.aes = FALSE,alpha = 1,color = "black"
  ) +
  geom_ribbon(data = baseline_non_ss_df,
              aes(x = year,ymin = baseline - (baseline * 0.2),ymax = baseline),
              alpha = 0.1) +
  geom_line(data = fishing_95, aes(x = year, y = biomass,color = Harvest_Rate),linewidth = 0.9) +
  # geom_line(data = baseline_non_ss_df,aes(x = year,y = MSC),linetype = "dashed",alpha = 0.6) +
  geom_line(data = bounds,aes(x = year,y = lower),linetype = "dashed",alpha = 0.6) +
  geom_line(data = bounds,aes(x = year,y = upper),linetype = "dashed",alpha = 0.6) +
  color_scale +
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

landings_cumulative <- ggplot() +
  geom_line(data = demersal_landings_95, aes(x = year, y = cumulative_demersal_landings,color = Harvest_Rate),linewidth = 0.9) +
  #geom_line(data = demersal_landings, aes(x = year, y = annual_demersal_landings,color = Harvest_Rate),alpha = 0.5) +
  color_scale +
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
  geom_line(data = demersal_landings_95, aes(x = year, y = annual_demersal_landings,color = Harvest_Rate),linewidth = 0.9) +
  color_scale +
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

p_95_Recovery <- (biomass / landings_annual / landings_cumulative) +
  plot_layout(guides = "collect", axes = "collect_x") + plot_annotation(
  caption = "Simulated dynamic fisheries closures. Recovery to 95th percentile of B_MSY (ICES Threshold). All values measured in mmN/m².",
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
) +
  NULL
p_95_Recovery
# p_MSC_Recovery | p_95_Recovery + 
#   plot_layout(guides = "collect", axes = "collect")
ggsave("../Figures/Transient/Barents_Sea/NM/Draft 1/Figure 6/Figure 6 ICES.png",
       height = 1080,
       width = 1920,
       units = "px",
       dpi = 200,
       bg = "white",
       plot = p_95_Recovery)

## and if you're happy....
ggsave("./Figures/Figure 6 ICES.png",
       height = 1080,
       width = 1920,
       units = "px",
       dpi = 200,
       bg = "white",
       plot = p_95_Recovery)

ggsave("../Figures/Transient/Barents_Sea/NM/Draft 1/Figure 6/Figure 6 MSC.png",
       height = 1080,
       width = 1920,
       units = "px",
       dpi = 200,
       bg = "white",
       plot = p_MSC_Recovery)

## and if you're happy....
ggsave("./Figures/Figure 6 MSC.png",
       height = 1080,
       width = 1920,
       units = "px",
       dpi = 200,
       bg = "white",
       plot = p_MSC_Recovery)
