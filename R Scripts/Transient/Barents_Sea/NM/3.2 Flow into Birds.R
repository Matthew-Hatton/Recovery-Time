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
baseline <- readRDS("../Objects/Experiments/Baseline/Baseline_0_fishing_Demersal_fish_1year.RDS")
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
      subset(select = bird) %>% 
      filter(bird > 0) %>% 
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
          subset(select = bird) %>% 
          filter(bird > 0) %>% 
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
`%notin%` <- Negate(`%in%`)

flows <- master %>%
  group_by(Year, Crash_Year, HR) %>%
  filter(Flow_from %notin% c("ocean","discards","corpses")) %>% 
  ungroup() %>% 
  filter(Crash_Year == 2020) %>% 
  mutate(HR = factor(HR, levels = c("Baseline", "MSY", "2x MSY")))

baseline_df <- baseline_df %>% 
  group_by(Year) %>%
  filter(Flow_from %notin% c("ocean","discards","corpses"))

ggplot() +
  geom_line(data = flows,aes(x = as.numeric(Year),y = bird,color = as.character(HR))) +
  geom_line(data = baseline_df,aes(x = Year,y = bird)) +
  facet_wrap(~ Flow_from,scales = "free_y",ncol = 2) +
  labs(x = "Year",y = "Flow to Birds",color = "Harvest Rate",
       caption = "Flow into birds. Why is baseline higher than 2x MSY?") +
  theme(strip.text = element_text(face = "bold"),
        legend.position = "top",
        legend.text = element_text(size = 12)) +
  scale_x_continuous(limits = c(2020,2099)) +
  theme_minimal() +
  NULL
ggsave("../Figures/Transient/Barents_Sea/NM/Draft 1/Figure 4/Figure 4a Flow to birds.png",
       dpi = 1200,width = 35,unit = "cm",bg = "white") # will need cleaning up for publication