foo <- subset(water_quality_ready,select = c(NO3,Date,Discharge)) %>% 
  mutate(Date = as.Date(Date),
         Month = month(Date),
         NO3 = as.numeric(NO3)/100) %>% 
  filter(year(Date) %in% seq(2010,2019))

ggplot() + 
  geom_line(data = foo,aes(x = Date,y = NO3))

ggplot(data = foo,aes(x = Date,y = as.numeric(Discharge))) +
  geom_line() +
  geom_point()


no3_monthly <- foo %>% 
  group_by(Month) %>% 
  summarise(monthly_no3 = mean(NO3,na.rm = T))

p1 <- ggplot() +
  geom_line(data = no3_monthly,aes(x = Month,y = monthly_no3),color = "darkgreen") +
  scale_x_continuous(labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                     breaks = seq(1,12)) + 
  labs(x = "Month",y = "NO3 (mMN/m^2)") +
  theme_minimal() +
  NULL
#################
bar <- subset(water_quality_ready,select = c(NH4,Date,Discharge)) %>% 
  mutate(Date = as.Date(Date),
         Month = month(Date),
         NH4 = as.numeric(NH4)/1000) %>% 
  filter(year(Date) %in% seq(2010,2019))

ggplot() + 
  geom_line(data = bar,aes(x = Date,y = NH4))

ggplot() +
  geom_line(data = bar,aes(x = Date,y = as.numeric(Discharge)))


NH4_monthly <- bar %>% 
  group_by(Month) %>% 
  summarise(monthly_NH4 = mean(NH4,na.rm = T))

ggplot() +
  geom_line(data = NH4_monthly,aes(x = Month,y = monthly_NH4)) +
  labs(title = "Monthly NH4 from AGRO")


saveRDS(no3_monthly,"./Objects/NO3 River Concentrations.RDS")
saveRDS(NH4_monthly,"./Objects/NH4 River Concentrations.RDS")


library(patchwork)
p1 <- ggplot() +
  geom_line(data = no3_monthly,aes(x = Month,y = monthly_no3),color = "darkgreen",linewidth = 1.5) +
  geom_point(data = no3_monthly,aes(x = Month,y = monthly_no3),color = "darkgreen",size = 3) +
  scale_x_continuous(labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                     breaks = seq(1,12)) + 
  scale_y_continuous(limits = c(0,4)) +
  labs(y = "NO3 (mMN/m^2)") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 14)) + 
  NULL
p2 <- ggplot() +
  geom_line(data = NH4_monthly,aes(x = Month,y = monthly_NH4),color = "darkgreen", linewidth = 1.5) +
  geom_point(data = NH4_monthly,aes(x = Month,y = monthly_NH4),color = "darkgreen",size = 3) +
  scale_x_continuous(labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                     breaks = seq(1,12)) + 
  labs(x = "Month",y = "NH3 (mMN/m^2)") +
  theme_minimal() +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 14)) +
  NULL
p2
p1/p2

ggsave("./Figures/saltless/AGRO NH4_NO3 levels.png",plot = last_plot(),width = 26,unit = "cm",height = 18)
