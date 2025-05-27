## Script to investigate the flows to and from birds across intervals
rm(list = ls())

library(purrr)
library(dplyr)
library(tibble)
library(ggplot2)
library(patchwork)
library(slider)
library(zoo)
library(progressr)

handlers("progress")
handlers(global = TRUE)  # Make sure a handler is set


transient_years <- seq(2020,2099)
interval <- seq(2020,2085,5)


all <- readRDS("../Objects/Experiments/Rolling Crash/Rolling_Crash_and_MSY_Demersal.RDS")
baseline <- readRDS("../Objects/Experiments/Baseline/Baseline_0_fishing_Demersal_fish.RDS")
example <- all[[1]][["Network_Indicators"]][["2020"]][["HR = 1"]][["2021"]] %>% 
  mutate(row_number = seq(1,nrow(.)))

baseline_df <- data.frame(bird = numeric(0),
                     Year = numeric(0),
                     Flow_from = character(0))


with_progress({
  p <- progressor(along = baseline[["Flow_Matrices"]])
  for (i in seq_along(baseline[["Flow_Matrices"]])) {
    # i = 1
    current <- baseline[["Flow_Matrices"]][[i]]
    df <- current %>% 
          subset(select = dfish) %>% 
          filter(dfish > 0) %>% 
          mutate(Year = transient_years[i]) %>% 
          rownames_to_column("Flow_from")
        baseline_df <- rbind(df, baseline_df)
        p(message = sprintf("Finished interval %d", i))
      }
    })



master <- data.frame(Baseline = numeric(0),
                     MSC = numeric(0),
                     Crash_Year = numeric(0),
                     Biomass = numeric(0),
                     year = numeric(0),
                     HR = character(0))


with_progress({
  p <- progressor(along = all)
  for (i in seq_along(all)) {
    current <- all[[i]][["Flows"]][[1]]
    for (k in 1:3) {
      hrs <- current[[k]]
      for (j in seq_along(hrs)) {
        df <- hrs[[j]] %>% 
          subset(select = dfish) %>% 
          filter(dfish > 0) %>% 
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

## ALL FLOWS
focal <- 27
baseline_df_bio <- data.frame(
  year = transient_years[1:length(baseline[["Biomasses"]])],
  baseline = map_dbl(baseline[["Biomasses"]], ~ .x$Model_annual_mean[focal])) %>% #extract DF biomass
  mutate(MSC = baseline * 0.8)

p1 <- ggplot() +
  geom_line(
    data = baseline_df_bio,
    aes(x = year, y = baseline), linetype = "solid", inherit.aes = FALSE,alpha = 1
  ) +
  # geom_line(
  #   data = baseline_non_ss_df,
  #   aes(x = year, y = baseline), linetype = "dashed", inherit.aes = FALSE,alpha = 1,color = "black"
  # ) +
  # # geom_smooth(se = FALSE) +
  # geom_ribbon(
  #   data = baseline_df,
  #   aes(x = year, y = baseline,ymin = baseline - (baseline * 0.05),ymax = baseline + (baseline * 0.05)),alpha = 0.1) +
  #facet_wrap(~ Crash_Year, ncol = 3, scales = "free_x",strip.position = "top") +
  labs(
    # title = "Demersal Fish Biomass Post-Crash by Crash Year and Harvest Rate",
    x = "Year", y = "Demersal Fish Biomass (mmN/m2)", color = "Harvest Rate"
  ) +
  scale_x_continuous(limits = c(2020,2099)) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"),
        legend.position = "top",
        legend.text = element_text(size = 12)) +
  NULL

p2 <- ggplot() +
  geom_line(data = baseline_df,aes(x = as.numeric(Year),y = dfish)) +
  facet_wrap(~Flow_from,scales = "free_y") +
  labs(y = "Flow into Demersal Fish")

p1 + p2
ggsave("../Figures/Transient/Barents_Sea/NM/Draft 1/Experiment/Meeting/DF biomass and flows.png",
       dpi = 1200,width = 35,unit = "cm",bg = "white")
