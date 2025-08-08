## Script to plot the recovery trajectories for each of the guilds, focal Demersal and Planktivorous fish
## Should validate our results for Figure 7 (Ecosystem Wide Recovery)

rm(list = ls()) # reset
library(MiMeMo.tools) # everything we need
color_scale <- scale_color_manual(
  values = c("Status Quo" = "#1b9e77", "MSY" = "#7570b3", "Overfishing" = "#d95f02"),
  name = "Harvest Rate"
) # for constistent plotting

biomass_Dfish <- readRDS("../Objects/Experiments/Maximum recovery time/Whole_ecosystem_Demersal_fish_Biomass.RDS") %>% 
  mutate(HR = case_when(
    HR == "2020s Baseline" ~ "Status Quo",
    HR == "2020s MSY" ~ "MSY",
    HR == "2020s 2x MSY" ~ "Overfishing"
  ), Guild = case_when(
    Guild == "Benthos_susp/dep_feeders_larvae" ~ "Benthos_susp_dep_feeders_larvae",
    Guild == "Benthos_carn/scav_feeders_larvae" ~ "Benthos_carn_scav_feeders_larvae",
    Guild == "Benthos_susp/dep_feeders" ~ "Benthos_susp_dep_feeders",
    Guild == "Benthos_carn/scav_feeders" ~ "Benthos_carn_scav_feeders",
    TRUE ~ Guild
  ))

biomass_Pfish <- readRDS("../Objects/Experiments/Maximum recovery time/Whole_ecosystem_Planktivorous_fish_Biomass.RDS") %>% 
  mutate(HR = case_when(
    HR == "2020s Baseline" ~ "Status Quo",
    HR == "2020s MSY" ~ "MSY",
    HR == "2020s 2x MSY" ~ "Overfishing"
    ), Guild = case_when(
      Guild == "Benthos_susp/dep_feeders_larvae" ~ "Benthos_susp_dep_feeders_larvae",
      Guild == "Benthos_carn/scav_feeders_larvae" ~ "Benthos_carn_scav_feeders_larvae",
      Guild == "Benthos_susp/dep_feeders" ~ "Benthos_susp_dep_feeders",
      Guild == "Benthos_carn/scav_feeders" ~ "Benthos_carn_scav_feeders",
      TRUE ~ Guild
    ))

guilds <- unique(biomass_Dfish$Guild)

for (i in guilds) {
  # i = guilds[[8]] # debug
  message(paste0(i,"\n"))
  tmp_Dfish <- filter(biomass_Dfish,Guild == i)
  tmp_Pfish <- filter(biomass_Pfish,Guild == i)
  
  dfish <- ggplot() +
    geom_line(data = tmp_Dfish,aes(x = year,y = Biomass,color = as.character(HR))) +
    geom_line(data = tmp_Dfish,aes(x = year,y = baseline_non_ss),alpha = 0.6,color = "black") +
    geom_ribbon(
      data = tmp_Dfish,
      aes(x = year, y = baseline_non_ss, ymin = baseline_non_ss - (baseline_non_ss * 0.2), ymax = baseline_non_ss), alpha = 0.1) +
    facet_wrap(~Crash_Year) +
    labs(title = paste0(i),x = "Year",y = "Biomass",color = "HR") +
    theme_bw() +
    theme(strip.text = element_text(face = "bold"),
          strip.background = element_rect(color = "black",fill = NA),
          legend.position = "top",
          legend.text = element_text(size = 12),
          axis.text.x = element_text(size = 8),
          panel.grid.minor = element_blank())  +
    scale_x_continuous(limits = c(2020,2099)) +
    color_scale +
    NULL
  
  pfish <- ggplot() +
    geom_line(data = tmp_Dfish,aes(x = year,y = Biomass,color = as.character(HR))) +
    geom_line(data = tmp_Dfish,aes(x = year,y = baseline_non_ss),alpha = 0.6,color = "black") +
    geom_ribbon(
      data = tmp_Dfish,
      aes(x = year, y = baseline_non_ss, ymin = baseline_non_ss - (baseline_non_ss * 0.2), ymax = baseline_non_ss), alpha = 0.1) +
    facet_wrap(~Crash_Year) +
    labs(title = paste0(i),x = "Year",y = "Biomass",color = "HR") +
    theme_bw() +
    theme(strip.text = element_text(face = "bold"),
          strip.background = element_rect(color = "black",fill = NA),
          legend.position = "top",
          legend.text = element_text(size = 12),
          axis.text.x = element_text(size = 8),
          panel.grid.minor = element_blank())  +
    scale_x_continuous(limits = c(2020,2099)) +
    color_scale +
    NULL
  
  ggsave(paste0("../Draft Figures/Supplementary/Demersal_fish Ecosystem Recovery Trajectory ",`i`,".png"),
         dpi = 1200,width = 35,height = 20,unit = "cm",bg = "white",plot = dfish)
  ggsave(paste0("../Draft Figures/Supplementary/Planktivorous_fish Ecosystem Recovery Trajectory ",`i`,".png"),
         dpi = 1200,width = 35,height = 20,unit = "cm",bg = "white",plot = pfish)
}