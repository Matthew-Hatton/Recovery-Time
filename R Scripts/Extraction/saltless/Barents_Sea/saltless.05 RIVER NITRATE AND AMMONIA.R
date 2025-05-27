rm(list = ls())                                                                                # Reset

library(tidyverse)

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

model.NO3 <- nls(NO3 ~ a * (1 - b)^Discharge_scaled,               #fitting exponential decay function
                 data = wq,
                 start = list(a = 150, b = 0.0001))                #provide estimates of start parameters, then model tries to converge

# library(mgcv)
# model.NO3 <- gam(NO3 ~ s(Discharge_scaled, bs = "cs"), data = wq)

### Found study on Greenlandic ice sheet input so may be better using that instead. let's create a time series to visualise this data
#read in data
mankoff <- read.csv("./Shared Data/Rivers/Mankoff Discharge/region_D.csv") %>% 
  mutate(Date = as.Date(Date))
ggplot() +
  geom_line(data = mankoff,aes(x = as.Date(Date),y = CW)) #there are seasonal cycles, so they SHOULD show up when we go to predict

mankoff_long <- mankoff %>%
  pivot_longer(cols = -Date, names_to = "Region", values_to = "Glacier_meltwater_flux") %>% #convert to long form
  filter(Region %in% c("CW","SW")) #filter to domain

# Create the plot
ggplot(mankoff_long, aes(x = Date, y = Glacier_meltwater_flux, color = Region)) +
  geom_line() +
  labs(x = "Date", y = "Water Discharge Gigatonnes per Year by Region",color = "Region") +
  #scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_minimal() +
  theme()

## Values need to be in cubic meters












## This plot would be better if we could convert from gate to an actualy m3 value.
gates <- read.csv("./Shared Data/Rivers/Mankoff Discharge/gate_meta.csv") %>%           # read in gate meta data
  filter(region %in% c("CW","SW")) %>% 
  mutate(gate_area = n_pixels*40000) %>%                                                       #pixels are 200m x 200m so convert - just surface layer, so height is 1 LIMITATION
  group_by(region) %>% 
  summarise(gate_area_m3 = mean(gate_area)) %>%                                                          #calculate mean gate area per region
  rename(Region = region)

## Values are measured in GT per year. we want m^3 per month
## 1GT == 1km^3

mankoff_joined <- mankoff_long %>%
  left_join(gates, by = "Region") %>% 
  mutate(Discharge_m3 = (Glacier_meltwater_flux*1e9) / gate_area_m3)  %>%   #convert to per m3                                             #convert to km3
  rename(Discharge_per_GT = Glacier_meltwater_flux)



ggplot(mankoff_joined, aes(x = Date, y = Discharge_m3, color = Region)) +
  geom_line() +
  labs(x = "Date", y = "Water Discharge per m3",color = "Region") +
  scale_y_continuous(limits = c(0, max(mankoff_joined$Discharge_m3, na.rm = TRUE))) +  # set y-axis limits
  #theme_minimal() +
  theme()


ggsave("./Figures/saltless/Mankoff Water Discharges.png",
       width = 12,height = 8)

## Now need to convert to a way in which the model will understand
## Model was created using meters cubed per second for Discharge
## Therefore, disacharge from Mankoff data needs to be the same
## Mankoff currently 1 data point for each month. Need to scale discharge as we did on creation of model


## regions don't actually matter anymore so let's aggregate

mankoff_agg <- mankoff_joined %>% 
  mutate(Discharge_scaled = Discharge_m3/1000,  # Consistent scaling
         Month = month(Date),
         Year = year(Date)) %>%
  filter(year(Date) %in% seq(2010,2019)) %>% 
  group_by(Month,Year) %>% 
  summarise(Discharge_scaled_m3 = sum(Discharge_scaled)) %>% 
  ungroup() %>% 
  group_by(Month) %>% 
  summarise(Discharge_m3 = mean(Discharge_scaled_m3))

ggplot() + geom_line(data = mankoff_agg,aes(x = Month,y = Discharge_m3))

## The error is something to do with these discharge scaled values. They really shouldn't all be the same.

NO3_WG <- predict(model.NO3,newdata = data.frame(Discharge_scaled = mankoff_agg$Discharge_m3))

NO3_WG_df <- data.frame(Month = seq(1,12),
                        NO3 = NO3_WG*1e11)

ggplot() + geom_line(data = NO3_WG_df,aes(x = Month,y = NO3),color = "darkgreen") +
  scale_x_continuous(labels = seq(1,12),breaks = seq(1,12)) + 
  labs(y = "Mean NO3 per meter cubed")

## Fix of this found in XX file. I give up. I have just taken the levels from AGRO and going to use those
## They represent the seasonal pattern you would expect.