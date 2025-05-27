## Script to calculate Demersal stock recovery time using a rolling MSY value

rm(list = ls())

library(purrr)
library(dplyr)
library(tibble)
library(ggplot2)
library(patchwork)
library(slider)
library(zoo)
library(progressr)

progressr::handlers("debug")


transient_years <- seq(2020,2099)
interval <- seq(2020,2085,5)

all <- readRDS("../Objects/Experiments/Rolling Crash/Rolling_Crash_and_MSY_Demersal.RDS")
baseline <- readRDS("../Objects/Experiments/Baseline/Baseline_0_fishing_Demersal_fish.RDS")

baseline_df <- data.frame(
  year = transient_years[1:length(baseline[["Biomasses"]])],
  baseline = map_dbl(baseline[["Biomasses"]], ~ filter(.x,Description == "Planktivorous_fish")$Model_annual_mean)) %>% #extract DF biomass
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
      Biomass = map_dbl(hrs, ~ filter(.x,Description == "Planktivorous_fish")$Model_annual_mean)) %>% #extract DF biomass
      mutate(Crash_Year = interval[i],
             HR = case_when(
               k == 1 ~ "Baseline",
               k == 2 ~ "MSY",
               k == 3 ~ "2x MSY"
             ))
    base <- baseline_df %>% filter(year > interval[i])
    df <- df %>% mutate(baseline = base$baseline,
                        MSC = base$baseline * 0.8)
    master <- rbind(df,master)
  }
}

# Define parameters
threshold_val <- 1     # E.g., 0.8 if threshold is 80% of baseline
tolerance <- 0.05      # ±5%
n_consecutive <- 5     # Require at least 3 consecutive years within range

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


ggplot(recovery_baseline, aes(x = Crash_Year, y = Recovery_Time_Baseline, color = as.character(HR))) +
  geom_line(linewidth = 1,alpha = 0.4) +
  geom_point(size = 2,alpha = 0.4) +
  # geom_smooth(se = FALSE, linewidth = 0.75) +
  geom_hline(yintercept = 20, linetype = "dashed") +
  labs(x = "Release Year", y = "Recovery Time (Years)", color = "Harvest Rate") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +  # y-axis starts at 0
  theme_minimal(base_size = 14) +
  theme(legend.position = "top") +
  NULL
# 
# ggsave("../Figures/Transient/Barents_Sea/NM/Draft 1/Figure 3 rolling MSY.png",
#        dpi = 1200,width = 25,unit = "cm",bg = "white")

ggplot() +
  geom_line(data = master, aes(x = year, y = Biomass, color = as.character(HR))) +
  geom_line(
    data = baseline_df,
    aes(x = year, y = baseline), linetype = "dashed", inherit.aes = FALSE,alpha = 1
  ) +
  # geom_smooth(se = FALSE) +
  geom_ribbon(
    data = baseline_df,
    aes(x = year, y = baseline,ymin = baseline - (baseline * 0.05),ymax = baseline + (baseline * 0.05)),alpha = 0.1) +
  facet_wrap(~ Crash_Year, ncol = 3, scales = "free_x",strip.position = "top") +
  labs(
    # title = "Demersal Fish Biomass Post-Crash by Crash Year and Harvest Rate",
    x = "Year", y = "Planktivorous Fish Biomass (mmN/m2)", color = "Harvest Rate"
  ) +
  scale_x_continuous(limits = c(2020,2099)) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"),
        legend.position = "top",
        legend.text = element_text(size = 12)) +
  NULL


ggplot(data = filter(master,Crash_Year == "2020")) +
  # geom_line(aes(x = year, y = Biomass, color = as.character(HR))) +
  geom_line(
    data = baseline_df,
    aes(x = year, y = baseline), linetype = "dashed", inherit.aes = FALSE,alpha = 1
  ) +
  # geom_smooth(se = FALSE) +
  # geom_ribbon(
  #   data = baseline_df,
  #   aes(x = year, y = baseline,ymin = baseline - (baseline * 0.05),ymax = baseline + (baseline * 0.05)),alpha = 0.1) +
  # facet_wrap(~ Crash_Year, ncol = 3, scales = "free_x",strip.position = "top") +
  labs(
    x = "Year", y = "Planktivorous Fish Biomass (mmN/m2)", color = "Harvest Rate"
  ) +
  scale_x_continuous(limits = c(2020,2099)) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"),
        legend.position = "top",
        legend.text = element_text(size = 12)) +
  NULL


# 
## what about if we take the yearly data?
baseline_non_ss <- readRDS("../Objects/Experiments/Baseline/Baseline_0_fishing_Demersal_fish_1year.RDS")
baseline_non_ss_df <- data.frame(
  year = transient_years[1:length(baseline_non_ss[["Biomasses"]])],
  baseline = map_dbl(baseline_non_ss[["Biomasses"]], ~ filter(.x,Description == "Planktivorous_fish")$Model_annual_mean))

# baseline_yearly <- readRDS("../Objects/Experiments/Baseline/YEARLY_Baseline_0_fishing_Demersal_fish.RDS")
# baseline_yearly_df <- data.frame(
#   year = transient_years[1:length(baseline_yearly[["Biomasses"]])],
#   baseline = map_dbl(baseline_yearly[["Biomasses"]], ~ filter(.x,Description == "Planktivorous_fish")$Model_annual_mean))

ggplot() +
  # geom_line(aes(x = year, y = Biomass, color = as.character(HR))) +
  geom_line(
    data = baseline_df,
    aes(x = year, y = baseline),inherit.aes = FALSE,alpha = 1,linetype = "dashed"
  ) +
  # geom_line(data = baseline_yearly_df,aes(x = year,y = baseline),alpha = 0.2) +
  geom_line(data = baseline_non_ss_df,aes(x = year,y = baseline),alpha = 0.4,linetype = "dashed") +
  labs(
    x = "Year", y = "Planktivorous Fish Biomass (mmN/m2)", color = "Harvest Rate", subtitle = "Steady State baseline and Transient Baseline"
  ) +

  scale_x_continuous(limits = c(2020,2099)) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"),
        legend.position = "top",
        legend.text = element_text(size = 12)) +
  NULL
ggsave("../Figures/Transient/Barents_Sea/NM/Draft 1/Figure 2 rolling MSY PFISH BASELINE.png",
       dpi = 1200,width = 25,unit = "cm",bg = "white")
### weird that the pfish have decreased
## let's take the row from pfish to everything else
## starting with baseline
handlers("cli")
`%notin%` <- Negate(`%in%`)

baseline_df <- data.frame(Year = numeric(0),
                          Flow_from = numeric(0),
                          Flow_to = character(0),
                          value = numeric(0))

with_progress({
  p <- progressor(along = baseline[["Flow_Matrices"]])
  for (i in seq_along(baseline[["Flow_Matrices"]])) {
    # i = 1
    current <- baseline[["Flow_Matrices"]][[i]]
    df <- current %>%
      as.data.frame() %>%
      rownames_to_column("Flow_from") %>%
      filter(Flow_from == "pfish") %>%
      pivot_longer(cols = -Flow_from, names_to = "Flow_to", values_to = "value") %>%
      filter(value > 0) %>%
      mutate(Year = transient_years[i])
    
    
    baseline_df <- rbind(df, baseline_df)
    p(message = sprintf("Finished interval %d", i))
  }
})

baseline_df <- baseline_df %>% filter(Flow_to %in% c("bird","ceta","seal"))

ggplot() +
  geom_line(data = baseline_df,aes(x = Year,y = value)) +
  facet_wrap(~ Flow_to) +
  labs(y = "Flow from Pfish",title = "As climate change occurs, pfish biomass drops to 0, so flows drop to 0")

## "As climate change occurs, pfish biomass drops to 0, so flows drop to 0"

## How is this affected when we fish?
master <- data.frame(Baseline = numeric(0),
                     MSC = numeric(0),
                     Crash_Year = numeric(0),
                     Biomass = numeric(0),
                     year = numeric(0),
                     HR = character(0))


with_progress({
  p <- progressor(along = all)
  for (i in seq_along(all)) {
    i = 1
    current <- all[[i]][["Flows"]][[1]]
    for (k in 1:3) {
      k = 1
      hrs <- current[[k]]
      for (j in seq_along(hrs)) {
        j = 1
        df <- hrs[[j]] %>%
          as.data.frame() %>%
          rownames_to_column("Flow_from") %>%
          filter(Flow_from == "pfish") %>%
          pivot_longer(cols = -Flow_from, names_to = "Flow_to", values_to = "value") %>%
          filter(value > 0) %>%
          mutate(Year = names(hrs)[j],
                 Crash_Year = interval[i],
                 HR = case_when(
                   k == 1 ~ "Baseline",
                   k == 2 ~ "MSY",
                   k == 3 ~ "2x MSY"
                 ),
                 HR_Prior = names(current)[k]) %>% 
          rownames_to_column("Flow_from")
        master <- rbind(df, master)
      }
    }
    p(message = sprintf("Finished interval %d", i))  # <-- Move this inside the outer loop
  }
})

pfish <- master %>% filter(Flow_from == "pfish")

