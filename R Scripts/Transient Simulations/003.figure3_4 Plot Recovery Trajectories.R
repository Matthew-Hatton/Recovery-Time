## Script to calculate Demersal stock recovery time using a rolling MSY value and different recovery conditions
## Recovery conditions are:
## Baseline (0 fishing) ran to a steady state each year - snapshot of ecosystem. Requires 5 consecutive years equal to or above baseline
## Baseline (0 fishing) ran in transient mode - effects from 2020. Requires intersection

rm(list = ls()) #reset

# load packages
library(purrr)
library(dplyr)
library(tibble)
library(ggplot2)
library(patchwork)
library(slider)
library(zoo)
library(progressr)
library(ggh4x)
library(tictoc)

tic()
progressr::handlers("cli") # progress bars are nice

# where are we calculating to?
transient_years <- seq(2020,2099)
interval <- seq(2020,2085,5)

##### SWITCH #####
# focal <- "Demersal_fish"
focal <- "Planktivorous_fish"

if (focal == "Demersal_fish") {
  all <- readRDS("../Objects/Experiments/Rolling Crash/Rolling_Crash_Static_MSY_DemersalV2.RDS")
} else{
  all <- readRDS("../Objects/Experiments/Rolling Crash/Rolling_Crash_and_MSY_Planktivorous.RDS")
}

# load
baseline <- readRDS("../Objects/Experiments/Baseline/Baseline_0_fishing_Demersal_fish.RDS")
baseline_non_ss <- readRDS("../Objects/Experiments/Baseline/Baseline_0_fishing_Demersal_fish_1year.RDS")


# extract
baseline_df <- data.frame(
  year = transient_years[1:length(baseline[["Biomasses"]])],
  baseline = map_dbl(baseline[["Biomasses"]], ~ filter(.x,Description == focal)$Model_annual_mean)) %>% #extract DF biomass
  mutate(MSC = baseline * 0.8)

baseline_non_ss_df <- data.frame(
  year = transient_years[1:length(baseline_non_ss[["Biomasses"]])],
  baseline = map_dbl(baseline_non_ss[["Biomasses"]], ~ filter(.x,Description  == focal)$Model_annual_mean)) %>% #extract DF biomass
  mutate(MSC = baseline * 0.8)

master <- data.frame(Baseline = numeric(0),
                     MSC = numeric(0),
                     Crash_Year = numeric(0),
                     Biomass = numeric(0),
                     year = numeric(0),
                     HR = character(0))

for (i in 1:length(all)) {
  ## DEBUG
  # i = 1
  current <- all[[i]][["Biomasses"]][[1]]

  for (k in 1:3) {
    ## DEBUG
    # k=1
    hrs <- current[[k]]
    df <- data.frame(
      year = (interval[i] + 1):max(transient_years),
      Biomass = map_dbl(hrs, ~ filter(.x,Description == focal)$Model_annual_mean)) %>% #extract DF biomass
      mutate(Crash_Year = interval[i],
             HR = case_when(
               k == 1 ~ "Baseline",
               k == 2 ~ "MSY",
               k == 3 ~ "2x MSY"
             ),
             HR_num = case_when(
               k == 1 ~ names(current)[k],
               k == 2 ~ names(current)[k],
               k == 3 ~ names(current)[k]
             ))
    base <- baseline_df %>% filter(year > interval[i])
    base_non_ss <- baseline_non_ss_df %>% filter(year > interval[i])
    df <- df %>% mutate(baseline = base$baseline,
                        baseline_non_ss = base_non_ss$baseline,
                        MSC = base$baseline * 0.8)
    master <- rbind(df,master)
  }
}

############ RECOVER TO SS
threshold_val <- 1     # e.g 0.8 if threshold is 80% of baseline
tolerance <- 0.01      # ±X%
n_consecutive <- 5     # Require at least N consecutive years within range

# Add threshold bands and check if values are within range OR above baseline
master_threshold <- master %>%
  group_by(HR, Crash_Year) %>%
  mutate(
    threshold = threshold_val * baseline,
    lower_bound = threshold * (1 - tolerance),
    upper_bound = threshold * (1 + tolerance),
    in_range = (Biomass >= lower_bound & Biomass <= upper_bound) | (Biomass >= baseline)
  )

# Use sliding window to detect first year with sufficient consecutive recovery
recovery_baseline <- master_threshold %>%
  group_by(HR, Crash_Year) %>%
  arrange(year) %>%
  mutate(
    recovered = slide_lgl(in_range, ~all(.x), .before = 0, .after = n_consecutive - 1)
  ) %>%
  filter(recovered) %>%
  slice_min(year, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(Recovery_Time_Baseline = year - Crash_Year) %>%
  dplyr::select(HR, Crash_Year, Recovery_Time_Baseline)


master$HR <- factor(master$HR, levels=c('Baseline', 'MSY', '2x MSY')) # reorder legend
master <- master %>% mutate(HR = case_when(
  HR == "Baseline" ~ "Status Quo",
  HR == "MSY" ~ "MSY",
  HR == "2x MSY" ~ "Overfishing"
))
############ RECOVER TO NON-SS
tolerance <- 0.2  # ±20% for MSC

# to fill in NA's
all_combos <- master %>%
  distinct(HR, Crash_Year)

# comp recovery time
recovery_results <- master %>%
  group_by(HR, Crash_Year) %>%
  mutate(
    lower_bound = baseline_non_ss * (1 - tolerance),
    upper_bound = baseline_non_ss * (1 + tolerance)
  ) %>%
  filter(year > Crash_Year) %>%
  filter(Biomass >= lower_bound & Biomass <= upper_bound) %>%
  slice_min(year, with_ties = FALSE) %>%
  mutate(Recovery_Time = year - Crash_Year) %>%
  ungroup() %>%
  dplyr::select(HR, Crash_Year, Recovery_Time)

# merge
recovery_baseline <- all_combos %>%
  left_join(recovery_results, by = c("HR", "Crash_Year"))

################################# SUPPLEMENTARY #################################


color_scale <- scale_color_manual(
  values = c("Status Quo" = "#1b9e77", "MSY" = "#7570b3", "Overfishing" = "#d95f02"),
  name = "Harvest Rate"
)
master$HR <- factor(master$HR, levels=c('Status Quo', 'MSY', 'Overfishing')) # reorder legend
non_ss_biomass <- ggplot() +
  geom_line(data = master, aes(x = year, y = Biomass, color = HR)) +
  geom_line(
    data = baseline_non_ss_df,
    aes(x = year, y = baseline),inherit.aes = FALSE,alpha = 0.6,color = "black"
  ) +
  geom_ribbon(
    data = baseline_non_ss_df,
    aes(x = year, y = baseline, ymin = baseline - (baseline * 0.2), ymax = baseline), alpha = 0.1) +
  facet_wrap(~ Crash_Year, ncol = 3, scales = "free_x", strip.position = "top") +
  labs(x = "Release Year", y = "Planktivorous Fish Biomass (N mmol⋅m¯³)", color = "Harvest Rate") +
  scale_x_continuous(limits = c(2020,2099)) +
  theme_bw() +
  theme(strip.text = element_text(face = "bold"),
        strip.background = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 12),
        axis.text.x = element_text(size = 8),
        panel.grid.minor = element_blank()) +
  color_scale
non_ss_biomass

recovery_baseline$HR <- factor(recovery_baseline$HR, levels=c('Status Quo', 'MSY', 'Overfishing')) # reorder legend
recovery_baseline$Recovery_Time <- recovery_baseline$Recovery_Time - 1

if (focal == "Demersal_fish") {
  recovery_baseline <- recovery_baseline %>% mutate(Guild = "Demersal fish")
} else{
  recovery_baseline <- recovery_baseline %>% mutate(Guild = "Planktivorous fish")
}

non_ss_recovery <- ggplot(recovery_baseline, aes(x = Crash_Year, y = Recovery_Time, color = (HR))) +
  geom_line(linewidth = 1, alpha = 1) +
  geom_point(size = 2, alpha = 1) +
  geom_hline(yintercept = 20, linetype = "dashed") +
  labs(x = "Release Year", y = "Recovery Time (Years)", color = "Harvest Rate") +
  facet_wrap(~Guild,strip.position = "top") +
  scale_y_continuous(limits = c(0, 60)) +
  scale_fill_discrete(breaks=c('Baseline', 'MSY','2x MSY')) +
  theme_bw() +
  theme(strip.text = element_text(face = "bold"),
        strip.background = element_rect(color = "black",fill = NA),
        legend.position = "top",
        legend.text = element_text(size = 12),
        axis.text.x = element_text(size = 8),
        panel.grid.minor = element_blank()) +
  color_scale

non_ss_biomass + non_ss_recovery + plot_layout(guides = "auto")

if (focal == "Demersal_fish") {
  saveRDS(non_ss_biomass,"../Objects/Figure Compilation/DFish Biomass.RDS")
  saveRDS(non_ss_recovery,"../Objects/Figure Compilation/DFish Recovery.RDS")
  
}else {
  saveRDS(non_ss_biomass,"../Objects/Figure Compilation/PFish Biomass.RDS")
  saveRDS(non_ss_recovery,"../Objects/Figure Compilation/PFish Recovery.RDS")
  
}

################################# FIGURE 4 #################################

master <- master %>% filter(Crash_Year %in% c(2020,2050,2080))

## Mark recovery Lines
if (focal == "Demersal_fish") {
  recovery_lines <- data.frame(Recovery_Year = c(NA,2027,2038,
                                                 NA,2059,2068,
                                                 2085,2095,NA),
                               biomass_at_recovery = c(NA,7.604748,7.135959,
                                                       NA,7.7590514,7.3678022,
                                                       5.6549002,5.2099363,NA),
                               Crash_Year = c(2020,2020,2020,
                                              2050,2050,2050,
                                              2080,2080,2080),
                               HR = c("Status Quo","MSY","Overfishing",
                                      "Status Quo","MSY","Overfishing",
                                      "Status Quo","MSY","Overfishing"))
  master <- master %>% mutate(Guild = "Demersal fish")
} else{
  recovery_lines <- data.frame(Recovery_Year = c(2027,2034,2052,
                                                 2074,2091,NA,
                                                 NA,NA,NA),
                               biomass_at_recovery = c(4.8510161,4.3567740,3.6175656,
                                                       2.25902898,1.13267874,NA,
                                                       NA,NA,NA),
                               Crash_Year = c(2020,2020,2020,
                                              2050,2050,2050,
                                              2080,2080,2080),
                               HR = c("Status Quo","MSY","Overfishing",
                                      "Status Quo","MSY","Overfishing",
                                      "Status Quo","MSY","Overfishing"))
  master <- master %>% mutate(Guild = "Planktivorous fish")
}

non_ss_biomass <- ggplot() +
  geom_line(data = master, aes(x = year, y = Biomass, color = HR)) +
  geom_line(
    data = baseline_non_ss_df,
    aes(x = year, y = baseline),inherit.aes = FALSE,alpha = 0.6,color = "black"
  ) +
  geom_ribbon(
    data = baseline_non_ss_df,
    aes(x = year, y = baseline, ymin = baseline - (baseline * 0.2), ymax = baseline), alpha = 0.1) +
  geom_segment(data = recovery_lines,aes(x = Recovery_Year,xend = Recovery_Year,y = 0,yend = biomass_at_recovery,color = HR),linetype = "dashed",show.legend = F) +
  ggh4x::facet_grid2(Crash_Year ~ Guild, scales = "free", independent = "all") +
  labs(x = "Release Year", y = paste0(master$Guild[1]," Biomass (N mmol⋅m⁻³)"), color = "Harvest Rate") +
  scale_x_continuous(limits = c(2020,2099)) +
  theme_bw() +
  theme(strip.text = element_text(face = "bold",size = 12),
        strip.background = element_rect(color = "black",fill = NA),
        legend.position = "top",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        panel.grid.minor = element_blank()) +
  color_scale
non_ss_biomass

if (focal == "Demersal_fish") {
  saveRDS(non_ss_biomass,"../Objects/Figure Compilation/DFish Biomass_main.RDS")
  ggsave("../Draft Figures/Transient/Barents_Sea/NM/Draft 2/Figure 3.png",
         dpi = 1200,width = 35,height = 20,unit = "cm",bg = "white") # will need cleaning up for publication
  ggsave("./Figures/Figure 3.png",
         dpi = 1200,width = 35,height = 20,unit = "cm",bg = "white") # will need cleaning up for publication
} else{
  saveRDS(non_ss_biomass,"../Objects/Figure Compilation/PFish Biomass_main.RDS")
  ggsave("../Draft Figures/Transient/Barents_Sea/NM/Draft 2/Figure 4.png",
         dpi = 1200,width = 35,height = 20,unit = "cm",bg = "white") # will need cleaning up for publication
  ggsave("./Figures/Figure 4.png",
         dpi = 1200,width = 35,height = 20,unit = "cm",bg = "white") # will need cleaning up for publication
}
toc()
