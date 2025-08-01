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
handlers(global = T)
progressr::handlers("cli") # progress bars are nice

# where are we calculating to?
transient_years <- seq(2020,2099)
interval <- seq(2020,2085,5)
all <- readRDS("../Objects/Experiments/Rolling Crash/Rolling_Crash_Static_MSY_DemersalV2.RDS")
# all <- readRDS("../Objects/Experiments/Rolling Crash/Rolling_Crash_and_MSY_Planktivorous.RDS")

baseline <- readRDS("../Objects/Experiments/Baseline/Baseline_0_fishing_Demersal_fish.RDS")
baseline_non_ss <- readRDS("../Objects/Experiments/Baseline/Baseline_0_fishing_Demersal_fish_1year.RDS")
# guilds <- c("Birds","Pinnipeds","Cetaceans","benths")
guilds <- filter(all[[1]][["Biomasses"]][["2020"]][["HR = 1"]][["2021"]],Description %in% c(
  # "Surface_layer_phytoplankton",
  # "Deep_layer_phytoplankton",
  "Omnivorous_zooplankton",
  "Carnivorous_zooplankton",
  "Benthos_susp/dep_feeders_larvae",
  "Benthos_susp/dep_feeders",
  "Benthos_carn/scav_feeders_larvae",
  "Benthos_carn/scav_feeders",
  # "Planktivorous_fish_larvae",
  # "Planktivorous_fish",
  "Migratory_fish",
  # "Demersal_fish_larvae",
  # "Demersal_fish",
  "Birds",
  "Pinnipeds",
  "Cetaceans",
  "Maritime_mammals"
))$Description

final_biomass <- data.frame(HR = numeric(0),
                             Crash_Year = numeric(0),
                             Biomass = numeric(0),
                             Guild = character(0),
                            baseline = numeric(0),
                            HR_num = character(0),
                            MSC = numeric(0),
                            baseline_non_ss = numeric(0),
                            year = numeric(0))

final_recovery <- data.frame(HR = numeric(0),
                     Crash_Year = numeric(0),
                     Recovery_Time = numeric(0),
                     Guild = character(0))
final_baseline <- data.frame(year = numeric(0),
                             baseline = numeric(0),
                             MSC = numeric(0),
                    Guild = character(0))
guild_extraction <- function(guilds){
  p <- progressr::progressor(along = transient_years)
for (guild in guilds) {
# load

focal <- guild

# extract
baseline_df <- data.frame(
  year = transient_years[1:length(baseline[["Biomasses"]])],
  baseline = map_dbl(baseline[["Biomasses"]], ~ filter(.x,Description == focal)$Model_annual_mean)) %>% #extract DF biomass
  mutate(MSC = baseline * 0.8)

baseline_non_ss_df <- data.frame(
  year = transient_years[1:length(baseline_non_ss[["Biomasses"]])],
  baseline = map_dbl(baseline_non_ss[["Biomasses"]], ~ filter(.x,Description  == focal)$Model_annual_mean)) %>% #extract DF biomass
  mutate(MSC = baseline * 0.8,
         Guild = guild)

final_baseline <- rbind(final_baseline,baseline_non_ss_df)

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

## all year biomasses
ss_biomass <- ggplot() +
  geom_line(data = master, aes(x = year, y = Biomass, color = as.character(HR))) +
  geom_line(
    data = baseline_df,
    aes(x = year, y = baseline), linetype = "solid", inherit.aes = FALSE,alpha = 0.6
  ) +
  geom_ribbon(
    data = baseline_df,
    aes(x = year, y = baseline,ymin = baseline - (baseline * 0.05),ymax = baseline + (baseline * 0.05)),alpha = 0.1) +
  facet_wrap(~ Crash_Year, ncol = 3, scales = "free_x",strip.position = "top") +
  labs(x = "Year", y = "Demersal Fish Biomass (mmN/m2)", color = "Harvest Rate",
       caption = "Recovery Time decreases here due to way in which recovery time is calculated.\n Large variation in baseline greatly affects recovery time."
  ) +
  scale_x_continuous(limits = c(2020,2099)) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"),
        legend.position = "top",
        legend.text = element_text(size = 12)) +
  NULL
# ss_biomass
# ggsave("../Figures/Transient/Barents_Sea/NM/Draft 1/Figure 2a Recovery Time Steady State Baseline.png",
#        dpi = 1200,width = 25,unit = "cm",bg = "white",plot = ss_biomass)



ss_recovery <- ggplot(recovery_baseline, aes(x = Crash_Year, y = Recovery_Time_Baseline, color = as.character(HR))) +
  geom_line(linewidth = 1,alpha = 1) +
  geom_point(size = 2,alpha = 1) +
  # geom_smooth(se = FALSE, linewidth = 0.75) +
  geom_hline(yintercept = 20, linetype = "dashed") +
  labs(x = "Release Year", y = "Recovery Time (Years)", color = "Harvest Rate",
       caption = "Recovery Time decreases here due to way in which recovery time is calculated.\n Large variation in baseline greatly affects recovery time."
  ) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +  # y-axis starts at 0
  theme_minimal(base_size = 14) +
  theme(legend.position = "top") +
  NULL
# ss_recovery
# ggsave("../Figures/Transient/Barents_Sea/NM/Draft 1/Figure 2b Recovery Time Steady State Baseline.png",
#        dpi = 1200,width = 25,unit = "cm",bg = "white",plot = ss_recovery)

# ss_biomass + ss_recovery
# ggsave("../Figures/Transient/Barents_Sea/NM/Draft 1/Figure 2 Steady State Baseline.png",
#        dpi = 1200,width = 35,unit = "cm",bg = "white") # will need cleaning up for publication
# 


master$HR <- factor(master$HR, levels=c('Baseline', 'MSY', '2x MSY')) # reorder legend
master <- master %>% mutate(HR = case_when(
  HR == "Baseline" ~ "2020s Baseline",
  HR == "MSY" ~ "2020s MSY",
  HR == "2x MSY" ~ "2020s 2x MSY"
),
  Guild = guild)
############ RECOVER TO NON-SS
tolerance <- 0.2  # ±20%

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



color_scale <- scale_color_manual(
  values = c("2020s Baseline" = "#1b9e77", "2020s MSY" = "#7570b3", "2020s 2x MSY" = "#d95f02"),
  name = "Harvest Rate"
)


recovery_baseline$HR <- factor(recovery_baseline$HR, levels=c('2020s Baseline', '2020s MSY', '2020s 2x MSY')) # reorder legend
recovery_baseline$Recovery_Time <- recovery_baseline$Recovery_Time - 1

recovery_baseline <- recovery_baseline %>% 
  mutate(Guild = guild)

final_recovery <- rbind(final_recovery,recovery_baseline)

final_biomass <- rbind(final_biomass,master)
p()
}
return(list(recovery = final_recovery,biomass = final_biomass))
}

final <- guild_extraction(guilds = guilds)
final_recovery <- final[["recovery"]]
final_biomass <- final[["biomass"]]

saveRDS(final_recovery,"../Objects/Experiments/Maximum recovery time/DF_Recovery_All_Upper.rds")
# ggsave("../Figures/Transient/Barents_Sea/NM/Draft 1/Figure 3/Figure 3 V2.png",
#        dpi = 1200,width = 35,height = 20,unit = "cm",bg = "white") # will need cleaning up for publication
# 
# ## and if you're happy
# ggsave("./Figures/Figure 3 V2.png",
#        dpi = 1200,width = 35,height = 20,unit = "cm",bg = "white")
