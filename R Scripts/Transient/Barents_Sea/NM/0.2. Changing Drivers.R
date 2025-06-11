## Script to plot the model domain - potential Figure 1

#### Setup ####
rm(list=ls()) # reset
Packages <- c("MiMeMo.tools", "exactextractr", "raster", "lubridate",
              "sf","rnaturalearth","tictoc","progressr")    # List packages
lapply(Packages, library, character.only = TRUE)   
source("../@_Region_file_BS.R")

My_volumes <- readRDS("../Objects/Barents_Sea/NM/TS.rds")

My_H_Flows <- readRDS("../Objects/Barents_Sea/NM/H-Flows.rds")

avg_data <- My_volumes %>%
  group_by(date) %>%
  summarise(across(where(is.numeric) & !c(Year, Month), mean, na.rm = TRUE)) %>%
  ungroup()


long_data <- avg_data %>%
  pivot_longer(cols = -c(date), names_to = "Variable", values_to = "Value") %>% 
  filter(! Variable %in% c("Zonal_avg","Meridional_avg","Salinity_avg","Phytoplankton_avg","Detritus_avg","Ice_pres","Snow_Thickness_avg"))

long_data$year <- format(as.Date(long_data$date, format="%d/%m/%Y"),"%Y")

long_data <- long_data %>% 
  mutate(
    Value = ifelse(is.na(Value), 0, Value),
    Variable = case_when(
      Variable == "DIN_avg" ~ "Dissolved Inorganic Nutrient (mmN/m^2)",
      Variable == "Ice_conc_avg" ~ "Ice Concentration (Proportion)",
      Variable == "Ice_pres" ~ "Ice Presence (Proportion)",
      Variable == "Ice_Thickness_avg" ~ "Ice Thickness (Meters)",
      Variable == "Snow_Thickness_avg" ~ "Snow Thickness (Metres)",
      Variable == "Temperature_avg" ~ "Seawater Temperature (Degrees C)",
      TRUE ~ Variable  # keep original if no match
    )
  )

long_data$Variable <- factor(long_data$Variable, levels = c(
  "Ice Concentration (Proportion)",        # top-left
  "Seawater Temperature (Degrees C)",      # top-right
  "Ice Thickness (Meters)",                 # bottom-left
  "Dissolved Inorganic Nutrient (mmN/m^2)" # bottom-right
))


ggplot(filter(long_data,year >= 2020), aes(x = date, y = Value)) +

  geom_line(color = "grey60") +
  geom_smooth(se = F,alpha = 0.3,color = "#4A8DB5") +
  facet_wrap(~ Variable, scales = "free_y", ncol = 2,strip.position = "right") +
  theme_linedraw() +
  labs(x = "Date",
       y = "Monthly Domain Average") +
  theme(axis.title.x = element_blank(),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(color = "black",size = 12),
        panel.grid.major.y = element_blank(),  # remove major y grid lines
        panel.grid.minor.y = element_blank()   # remove minor y grid lines (if any)
  )
ggsave("../Figures/Transient/Barents_Sea/NM/Draft 1/Figure 1/Figure 1 Drivers.png",
       dpi = 1200,
       bg = "white",width = 25) #save out
