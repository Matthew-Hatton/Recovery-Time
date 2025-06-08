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


ggplot(data = merged,aes(x = catch,y = Recovery_Time_MSC,color = as.character(Decade))) +
  geom_point() +
  geom_hline(yintercept = 20,linetype = "dashed") +
  geom_text_repel(aes(label = round(HR, 2)), size = 3,max.overlaps = 20) +
  labs(color = "Decade",
       x = "Demersal Catch (mmN/m^2)",
       y = "Recovery Time (Years)") +
  NULL
ggsave("./Figures/Figure 5 Catch vs. Recovery Time.png",
       dpi = 1200,width = 25,height = 25,unit = "cm",bg = "white")
