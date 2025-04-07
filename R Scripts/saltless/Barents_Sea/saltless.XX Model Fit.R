rm(list = ls())                                                                                # Reset

library(tidyverse)
library(mgcv)

water_quality <- read.csv("./Shared Data/Rivers/ArcticGRO Water Quality Data.csv")[-c(1:7),]   # get water quality data and clean
names(water_quality) <- water_quality[1,]
water_quality_ready <- water_quality[-c(1,2),]
names(water_quality_ready) <- make.unique(names(water_quality_ready))
water_quality_ready <- water_quality_ready %>%
  filter(Date != "")

wq <- water_quality_ready %>% 
  mutate(Season = case_when(
    month(Date) %in% c(12, 1, 2) ~ "Winter",
    month(Date) %in% c(3, 4, 5) ~ "Spring",
    month(Date) %in% c(6, 7, 8) ~ "Summer",
    month(Date) %in% c(9, 10, 11) ~ "Fall"
  )) %>% 
  subset(select = c(Discharge,NO3,NH4))

#convert to numeric and omit NA rows
wq$NO3 <- as.numeric(wq$NO3)
wq <- wq[!is.na(wq$NO3),]

wq$Discharge <- as.numeric(wq$Discharge)

wq$Discharge_scaled <- (wq$Discharge)/1000 #convert from litres to cubic meters

model.NO3 <- gam(NO3 ~ s(Discharge_scaled, bs = "cs"), data = wq)
discharge_seq <- seq(min(wq$Discharge_scaled), max(wq$Discharge_scaled), length.out = 100)
predictions <- predict(model.NO3, newdata = data.frame(Discharge_scaled = discharge_seq))

# Combine the prediction data for plotting
plot_data <- data.frame(Discharge_scaled = discharge_seq, Predicted_NO3 = predictions)

# Plot the original data and the fitted GAM line
ggplot(wq, aes(x = Discharge_scaled, y = NO3)) +
  geom_point(color = "blue", alpha = 0.6) +  # Original data points
  geom_line(data = plot_data, aes(x = Discharge_scaled, y = Predicted_NO3), color = "red", linewidth = 1) +  # Fitted GAM line
  labs(title = "GAM Fit of NO3 vs. Discharge",
       x = "Discharge (Scaled)",
       y = "NO3") +
  theme_minimal()