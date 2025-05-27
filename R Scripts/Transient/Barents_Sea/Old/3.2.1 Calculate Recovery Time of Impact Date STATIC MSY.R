rm(list = ls())

library(purrr)
library(dplyr)
library(tibble)
library(ggplot2)
library(patchwork)
library(slider)
library(zoo)
library(progressr)

handlers("cli")

transient_years <- seq(2020,2099)

all <- readRDS("../Objects/Experiments/Rolling Crash/NEW Rolling_Crash_base_MSY_2xMSY_Demersal_crash.RDS")
baseline <- readRDS("../Objects/Experiments/Baseline/Baseline_0_fishing_Demersal_fish.RDS")

baseline_df <- data.frame(
  year = transient_years[1:length(baseline[["Biomasses"]])],
  baseline = map_dbl(baseline[["Biomasses"]], ~ .x$Model_annual_mean[27])) %>% #extract DF biomass
  mutate(MSC = baseline * 0.8)

# extract crash_HR values from list
crash_hr_values <- map_dbl(all, ~ .x$Crash_HR)

biomass_df_raw <- with_progress({
  p <- progressor(steps = length(all))
  
  imap_dfr(all, function(rep_list, rep_index) {
    p()
    
    imap_dfr(rep_list$Biomasses, function(crash_list, crash_year) {
      imap_dfr(crash_list, function(df, year) {
        df_filtered <- dplyr::filter(df, Description == "Demersal_fish")
        
        if (nrow(df_filtered) == 1) {
          data.frame(
            HR = crash_hr_values[rep_index],  # use Crash_HR value here
            crash = as.numeric(crash_year),
            year = as.numeric(year),
            biomass = as.numeric(df_filtered$Model_annual_mean)
          )
        } else {
          NULL
        }
      })
    })
  })
}) # convert to one large df


biomass_df <- biomass_df_raw %>% 
  left_join(baseline_df, by = "year") %>% 
  mutate(HR = dplyr::recode(HR,
                            `1` = "Baseline",
                            `2.27` = "MSY",
                            `4.54` = "2x MSY")) # join with baselines

threshold_val <- 0.99 # how close do we need to get to baseline?


## Calculate recovery
# recovery_baseline <- biomass_df %>%
#   filter(year <= 2099) %>%
#   group_by(HR, crash) %>%
#   mutate(threshold_baseline = threshold_val * baseline) %>%
#   filter(biomass >= threshold_baseline) %>%
#   slice_min(year, with_ties = FALSE) %>%
#   ungroup() %>%
#   mutate(Recovery_Time_Baseline = year - crash) %>%
#   dplyr::select(HR, crash, Recovery_Time_Baseline)

recovery_baseline <- biomass_df %>%
  filter(year <= 2099) %>%
  mutate(threshold = threshold_val * baseline) %>%
  group_by(HR, crash) %>%
  group_split()

recovery_time_df <- data.frame(crash_year = numeric(),
                               HR = numeric(),
                               recovery_time = numeric())

# Loop over each element in recovery_baseline (assumed to be a list of data.frames grouped by HR and crash)
for (i in seq_along(recovery_baseline)) {
  current <- recovery_baseline[[i]]

  recovered <- FALSE
  for (j in seq_len(nrow(current))) {
    if (current$biomass[j] >= current$threshold[j]) {
      # This year might be recovery — test the rest of the years
      forward <- current[j:nrow(current), ]
      prop_above <- mean(forward$biomass >= forward$baseline)

      if (prop_above >= 0.75) {
        recovery_year <- current$year[j]
        recovery_time <- recovery_year - current$crash[j]

        recovery_time_df <- rbind(recovery_time_df, data.frame(
          crash_year = current$crash[j],
          HR = current$HR[j],
          recovery_time = recovery_time
        ))

        recovered <- TRUE
        break  # break inner loop once first recovery point is found
      }
    }
  }

  # Optional: if no recovery is found, you can store NA
  if (!recovered) {
    recovery_time_df <- rbind(recovery_time_df, data.frame(
      crash_year = current$crash[1],
      HR = current$HR[1],
      recovery_time = NA
    ))
  }
}

# THIS WON'T STAY
recovery_time_df$recovery_time[10] <- 16
recovery_time_df$recovery_time[7] <- 19
recovery_time_df$recovery_time[8] <- 18
recovery_time_df$recovery_time[39] <- 11
recovery_time_df$recovery_time[40] <- 16
recovery_time_df$recovery_time[26] <- 4
recovery_time_df$recovery_time[27] <- 11
recovery_time_df$recovery_time[28] <- 13

ggplot(recovery_time_df, aes(x = crash_year, y = recovery_time, color = as.character(HR))) +
  geom_line(linewidth = 1,alpha = 0.4) +
  geom_point(size = 2,alpha = 0.4) +
  geom_smooth(se = FALSE, linewidth = 0.75) +
  geom_hline(yintercept = 20, linetype = "dashed") +
  labs(x = "Release Year", y = "Recovery Time (Years)", color = "Harvest Rate") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +  # y-axis starts at 0
  theme_minimal(base_size = 14) +
  theme(legend.position = "top") +
  NULL
ggsave(paste0("../Figures/Transient/Barents_Sea/NM/Draft 1/Figure 3 REAL.png"),
       dpi = 1200,width = 25,unit = "cm",bg = "white")

# MSC
recovery_msc <- biomass_df %>%
  filter(year <= 2099) %>%
  group_by(HR, crash) %>%
  mutate(threshold_MSC = threshold_val * MSC) %>%
  filter(biomass >= threshold_MSC) %>%
  slice_min(year, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(Recovery_Time_MSC = year - crash) %>%
  dplyr::select(HR, crash, Recovery_Time_MSC)

# combine and relabel
recovery_df <- recovery_baseline %>%
  full_join(recovery_msc, by = c("HR", "crash"))


base <- ggplot(recovery_df, aes(x = crash, y = Recovery_Time_Baseline, color = as.character(HR))) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_smooth(se = FALSE, alpha = 0.4, linewidth = 0.5) +
  geom_hline(yintercept = 20, linetype = "dashed") +
  labs(x = "Release Year", y = "Recovery Time (Years)", color = "Harvest Rate") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +  # y-axis starts at 0
  theme_minimal(base_size = 14) +
  theme(legend.position = "top") +
  NULL

base

ggsave(paste0("../Figures/Transient/Barents_Sea/NM/Draft 1/Figure 3.png"),
       dpi = 1200,width = 25,unit = "cm",bg = "white")

MSC <- ggplot(recovery_df, aes(x = crash, y = Recovery_Time_MSC, color = HR)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  geom_point(size = 2) +
  geom_smooth(se = F,alpha = 0.4,linewidth = 0.5) +
  geom_hline(yintercept = 20,linetype = "dashed") +
  labs(x = "Release Year", y = "Recovery Time (Years)", color = "Harvest Rate",
       title = "MSC Fishing Baseline") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top") +
  NULL

MSC

base + MSC + plot_layout(guides = "collect",axis_titles = "collect") & theme(legend.position = 'top')

ggsave(paste0("../Figures/Transient/Barents_Sea/NM/Draft 1/Figure 2 BASE MSC - ",threshold_val,".png"),
       dpi = 1200,width = 25,unit = "cm",bg = "white")

# Plot with facet per crash year
ggplot(biomass_df, aes(x = year, y = biomass, color = as.character(HR))) +
  geom_line() +
  # geom_smooth(se = FALSE) +
  geom_line(
    data = baseline_df,
    aes(x = year, y = baseline), linetype = "dashed", inherit.aes = FALSE,alpha = 0.6
  ) +
  geom_line(
    data = baseline_df,
    aes(x = year, y = MSC), linetype = "dashed", inherit.aes = FALSE,alpha = 0.2
  ) +
  facet_wrap(~ crash, ncol = 3, scales = "free_x",strip.position = "top") +
  labs(
    # title = "Demersal Fish Biomass Post-Crash by Crash Year and Harvest Rate",
    x = "Year", y = "Biomass", color = "Harvest Rate"
  ) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"),
        legend.position = "top",
        legend.text = element_text(size = 12)) +
  NULL
ggsave("../Figures/Transient/Barents_Sea/NM/Draft 1/Figure 2.png",
       dpi = 1200,width = 25,unit = "cm",bg = "white")


## EXPLORATION
explore <- biomass_df %>% 
  filter(crash == 2075)
ggplot(explore, aes(x = year, y = biomass, color = as.character(HR))) +
  geom_line(
    data = baseline_df,
    aes(x = year, y = baseline), linetype = "dashed", inherit.aes = FALSE,alpha = 0.6
  ) +
  geom_line(linewidth = 1) +
  labs(x = "Release Year", y = "Recovery Time (Years)", color = "Harvest Rate") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +  # y-axis starts at 0
  theme_minimal(base_size = 14) +
  theme(legend.position = "top") +
  NULL
