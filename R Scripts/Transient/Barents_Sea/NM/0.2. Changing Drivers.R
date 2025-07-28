## Script to plot the model domain - potential Figure 1

#### Setup ####
rm(list=ls()) # reset
Packages <- c("MiMeMo.tools", "exactextractr", "raster", "lubridate",
              "sf","rnaturalearth","tictoc","progressr","zoo")    # List packages
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
  filter(! Variable %in% c("Zonal_avg","Meridional_avg","Salinity_avg","Phytoplankton_avg","Detritus_avg","Ice_pres","Snow_Thickness_avg","Ice_Thickness_avg"))

long_data$year <- format(as.Date(long_data$date, format="%d/%m/%Y"),"%Y")

long_data <- long_data %>% 
  mutate(
    Value = ifelse(is.na(Value), 0, Value),
    Variable = case_when(
      Variable == "DIN_avg" ~ "DIN (N mmol⋅m⁻³)",
      Variable == "Ice_conc_avg" ~ "Ice Concentration (Proportion)",
      Variable == "Ice_pres" ~ "Ice Presence (Proportion)",
      # Variable == "Ice_Thickness_avg" ~ "Ice Thickness (Meters)",
      Variable == "Snow_Thickness_avg" ~ "Snow Thickness (Metres)",
      Variable == "Temperature_avg" ~ "Seawater Temperature (Degrees C)",
      TRUE ~ Variable  # keep original if no match
    )
  )

long_data$Variable <- factor(long_data$Variable, levels = c(
  "Ice Concentration (Proportion)",        # top
  # "Ice Thickness (Meters)",                 # second
  "Seawater Temperature (Degrees C)",      # third
  "DIN (N mmol⋅m¯³)" #bottom
))

plt_data <- long_data %>% filter(year >= 2020) %>%
  mutate(date = as.Date(date))

# Define a helper function to apply the 10-year trailing window
smooth_10yr <- function(df) {
  df <- df %>%
    arrange(date) %>%
    mutate(Smoothed = rollapply(Value, 
                                width = 120,  # 10 years * 12 months = 120
                                FUN = mean, 
                                align = "right", 
                                fill = NA))
  return(df)
}

# Apply smoothing per variable
smoothed_data <- plt_data %>%
  group_by(Variable) %>%
  group_modify(~ smooth_10yr(.x)) %>%
  ungroup()

ggplot(smoothed_data, aes(x = date, y = Value)) +
  geom_line(color = "grey60", linewidth = 0.3) +
  geom_smooth(se = F,alpha = 0.3,color = "#4A8DB5") +
  facet_wrap(~ Variable, scales = "free_y", ncol = 1, strip.position = "right") +
  theme_linedraw() +
  labs(x = "Date",
       y = "Monthly Domain Average") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 8),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(color = "black", size = 8),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())


ggsave("../Figures/Transient/Barents_Sea/NM/Draft 1/Figure 1/Figure 1 Drivers.png",
       bg = "white",width = 1400,height = 1754,units = "px") #save out
ggsave("./Figures/Figure 1.png",
       bg = "white",width = 1400,height = 1754,units = "px") #save out
