rm(list = ls())

library(tidyverse)
library(ggrepel)
library(patchwork)

## present
recovery_time <- readRDS("../Objects/Experiments/Crash/Paper/Recovery_Time_Aggregation_2020.RDS") %>% 
  mutate(Decade = 2020)
catch <- readRDS("../Objects/Experiments/Crash/Paper/Crash_Aggregation.RDS") %>% 
  mutate(Decade = 2020)
## 2050
recovery_time_mid <- readRDS("../Objects/Experiments/Crash/Paper/Recovery_Time_Aggregation_MID.RDS") %>% 
  mutate(Decade = 2050)
catch_mid <- readRDS("../Objects/Experiments/Crash/Paper/Crash_Aggregation_MID.RDS") %>% 
  mutate(Decade = 2050)
## 2070
recovery_time_2070 <- readRDS("../Objects/Experiments/Crash/Paper/Recovery_Time_Aggregation_2070.RDS") %>% 
  mutate(Decade = 2070)
catch_2070 <- readRDS("../Objects/Experiments/Crash/Paper/Crash_Aggregation_2070.RDS") %>% 
  mutate(Decade = 2070)

## combine data for plotting
merged_2020 <- cbind(recovery_time,catch) %>% 
  subset(select = c(catch,HR,
                    Recovery_Time_MSC,Decade))

merged_2050 <- cbind(recovery_time_mid,catch_mid) %>% 
  subset(select = c(catch,HR,
                    Recovery_Time_MSC,Decade))

merged_2070 <- cbind(recovery_time_2070,catch_2070) %>% 
  subset(select = c(catch,HR,
                    Recovery_Time_MSC,Decade))

merged <- rbind(merged_2020,merged_2050,merged_2070)


merged$highlight <- NA
merged$highlight[merged$HR == 1.0] <- "Baseline"
merged$highlight[merged$HR == as.character(2.8)] <- "MSY"
merged$highlight[merged$HR == 5.6] <- "2x MSY"

merged$highlight <- factor(merged$highlight, levels = c("Baseline", "MSY", "2x MSY"))


labelled_points <- merged[round(merged$HR, 1) %in% c(0, 1, 2.8, 5.6), ]


ggplot(data = merged, aes(x = catch, y = Recovery_Time_MSC, color = as.character(Decade))) +
  geom_hline(yintercept = 20, linetype = "dashed") +
  
  geom_path(
    arrow = arrow(type = "closed", length = unit(0.15, "inches")),
    lineend = "round"
  ) +
  
  geom_point(
    data = labelled_points,
    aes(x = catch, y = Recovery_Time_MSC),
    size = 2,
    show.legend = FALSE
  ) +
  
  geom_text_repel(
    data = labelled_points,
    aes(label = HR, color = as.character(Decade)),
    size = 5,
    max.overlaps = 10,
    segment.color = "grey50"
  ) +
  
  labs(color = "Decade",
       x = "Demersal Catch (mmN/m²)",
       y = "Recovery Time (Years)") +
  
  scale_color_manual(values = c("grey40", "#592DD2", "#D2592D")) +
  
  # Override legend: smaller line and no point
  guides(color = guide_legend(override.aes = list(size = 1.5, shape = NA))) +
  
  theme_minimal()
ggsave("./Figures/Figure 5 Catch vs. Recovery Time.png",
       dpi = 1200,width = 30,height = 25,unit = "cm",bg = "white")

