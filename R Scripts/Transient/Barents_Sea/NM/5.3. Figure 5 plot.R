rm(list = ls())

library(tidyverse)
library(ggrepel)
library(patchwork)

recovery_time <- readRDS("../Objects/Experiments/Crash/Paper/Recovery_Time_Aggregation.RDS")
catch <- readRDS("../Objects/Experiments/Crash/Paper/Crash_Aggregation.RDS")

merged <- cbind(recovery_time,catch) %>% 
  subset(select = c(catch,recovery_time_baseline,HR,
                    recovery_time_MSC))

merged$highlight <- NA
merged$highlight[merged$HR == 1.0] <- "Baseline"
merged$highlight[merged$HR == as.character(2.8)] <- "MSY"
merged$highlight[merged$HR == 5.6] <- "2x MSY"

merged$recovery_time_baseline[1] <- 0
merged$recovery_time_MSC[1] <- 0

merged$highlight <- factor(merged$highlight, levels = c("Baseline", "MSY", "2x MSY"))

base <- ggplot(merged, aes(x = catch, y = recovery_time_baseline)) +
  geom_point(color = "gray70", size = 3) +
  geom_point(data = subset(merged, !is.na(highlight)),
             aes(color = highlight), size = 5) +
  geom_text_repel(aes(label = round(HR, 2)), size = 3,max.overlaps = 20) +
  geom_hline(yintercept = c(20),linetype = "dashed") +
  scale_color_manual(
    values = c("Baseline" = "#7CAE00", "MSY" = "#00BFC4", "2x MSY" = "#F8766D"),
    na.value = "gray70",
    name = "Fishing Standards"
  ) +
  theme_minimal(base_size = 14) +
  labs(
    x = "Catch",
    y = "Recovery Time",
    title = "Catch vs Recovery Time: Baseline"
  )

MSC <- ggplot(merged, aes(x = catch, y = recovery_time_MSC)) +
  geom_point(color = "gray70", size = 3) +
  geom_point(data = subset(merged, !is.na(highlight)),
             aes(color = highlight), size = 5) +
  geom_text_repel(aes(label = round(HR, 2)), size = 3,max.overlaps = 20) +
  geom_hline(yintercept = c(20),linetype = "dashed") +
  scale_color_manual(
    values = c("Baseline" = "#7CAE00", "MSY" = "#00BFC4", "2x MSY" = "#F8766D"),
    na.value = "gray70",
    name = "Fishing Standards"
  ) +
  theme_minimal(base_size = 14) +
  labs(
    x = "Catch",
    y = "Recovery Time",
    title = "Catch vs Recovery Time: MSC Threshold"
  )

base + MSC

