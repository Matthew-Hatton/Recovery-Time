library(tidyverse)
library(lubridate)
## weird jumps up in recovery time (and down in biomass)

My_boundary_data <- readRDS("../Objects/Barents_Sea/NM/Boundary measurements.rds") %>%   
  group_by(Month, Compartment, Variable,Year) %>%                                                 # Average across years
  summarise(Measured = mean(Measured, na.rm = T)) %>% 
  ungroup() %>% 
  arrange(Month) %>%       
  # Order months ascending
  mutate(Compartment = factor(Compartment, levels = c("Inshore S", "Offshore S", "Offshore D"),
                              labels = c("Inshore S" = "SI", "Offshore S" = "SO", "Offshore D" = "D")),
         #Measured = ifelse(Variable == "Chlorophyll", 
         #  Redundant      Measured * (20 / 12) * (16/106), # weight C : weight Chla, convert to moles of C 
         #                 Measured)  # weight C : weight Chla, convert to moles of C, Redfield ratio atomic N to C 
  ) %>%
  pivot_wider(names_from = c(Compartment, Variable), names_sep = "_", values_from = Measured)  %>% 
  mutate(date = make_date(Year, Month, 1))

My_boundary_long <- My_boundary_data %>%
  pivot_longer(
    cols = -c(Month, Year, date),  # keep these columns
    names_to = "Variable",
    values_to = "Value"
  )

# Plot with one facet per variable
ggplot(My_boundary_long, aes(x = date, y = Value)) +
  geom_line() +
  geom_vline(xintercept = as.Date("2050-01-01"), linetype = "dashed", color = "red") +
  facet_wrap(~ Variable, scales = "free_y") +
  labs(x = "Date", y = "Value", title = "Boundary Conditions Over Time") +
  theme_minimal()


My_atmosphere <- readRDS(stringr::str_glue("../Objects/Barents_Sea/NM/Atmospheric N deposition.rds")) %>%
  group_by(Month, Oxidation_state, Shore,  Year) %>%
  summarise(Measured = sum(Measured, na.rm = T)) %>%                                              # Sum across deposition states
  summarise(Measured = mean(Measured, na.rm = T)) %>%                                             # Average over years
  ungroup() %>%
  pivot_wider(names_from = c(Shore, Oxidation_state), values_from = Measured) %>%                     # Spread to match template
  arrange(Month)

#### Physics ####
My_light <- readRDS("../Objects/Barents_Sea/NM/Air temp and light.rds") %>% 
  filter(Shore == "Combined") %>%               # Limit to reference period and variable - light only goes to 2019, so if past that, hold it at 2019 values
  group_by(Month) %>%  # Average across months
  summarise(Measured = mean(Measured, na.rm = T)) %>% 
  ungroup() %>% 
  arrange(Month)                                                            # Order to match template

My_air_temp <- readRDS("../Objects/Barents_Sea/NM/Air temp and light.rds") %>% 
  filter(Shore %in% c("Inshore","Offshore")) %>% 
  group_by(Month,Shore) %>% 
  summarise(Measured = mean(Measured)) %>% 
  ungroup() %>% 
  arrange(Month)

My_H_Flows <- readRDS("../Objects/Barents_Sea/NM/H-Flows.rds") %>%                               # Limit to reference period
  group_by(across(-c(Year, Flow))) %>%                                      # Group over everything except year and variable of interest
  summarise(Flow = mean(Flow, na.rm = T)) %>%                               # Average flows by month over years
  ungroup() %>% 
  left_join(My_scale,by = join_by(Shore,slab_layer)) %>%                                                   # Attach compartment volumes
  mutate(Flow = Flow/Volume) %>%                                            # Scale flows by compartment volume
  mutate(Flow = abs(Flow * 86400)) %>%                                      # Multiply for total daily from per second, and correct sign for "out" flows
  arrange(Month)                                                           # Order by month to match template


My_V_Flows <- readRDS("../Objects/Barents_Sea/NM/vertical diffusivity.rds") %>%                                 # Limit to reference period
  group_by(Month) %>% 
  summarise(V_diff = mean(Vertical_diffusivity, na.rm = T)) %>% 
  ungroup() %>% 
  arrange(Month)                                                            # Order by month to match template

My_volumes <- readRDS("../Objects/Barents_Sea/NM/TS.rds") %>%                                  # Limit to reference period
  group_by(Compartment, Month,Year) %>%                                          # By compartment and month
  summarise(across(c(DIN_avg,Phytoplankton_avg,Detritus_avg,Temperature_avg), mean, na.rm = T)) %>%         # Average across years for multiple columns
  ungroup() %>% 
  arrange(Month)  %>% 
  mutate(date = make_date(Year, Month, 1)) %>% 
  pivot_longer(cols = c(DIN_avg,Phytoplankton_avg,Detritus_avg,Temperature_avg),
               names_to = "Variable",
               values_to = "Value")  

ggplot(My_volumes, aes(x = date, y = Value, color = Compartment)) +
  geom_line() +
  facet_wrap(~ Variable, scales = "free_y") +
  geom_vline(xintercept = as.Date("2050-01-01"), linetype = "dashed", color = "red") +
  #labs(x = "Date", y = "Value", title = "Sea Ice Variables Over Time") +
  theme_minimal()
# Order by month to match template

My_ice <- readRDS("../Objects/Barents_Sea/NM/TS.rds") %>% 
  filter(Shore %in% c("Inshore","Offshore") & slab_layer == "S") %>%  # Remove Buffer Zone
  group_by(Month,Shore,Year) %>% 
  summarise(Ice_Pres = mean(Ice_pres,na.rm = T),
            Snow_Thickness = mean(Snow_Thickness_avg,na.rm = T),
            Ice_Thickness = mean(Ice_Thickness_avg,na.rm = T),
            Ice_Conc = mean(Ice_conc_avg,na.rm = T)) %>%
  mutate(across(everything(), ~ ifelse(is.nan(.), 0, .))) %>% 
  mutate(date = make_date(Year, Month, 1)) %>% 
  pivot_longer(cols = c(Ice_Pres, Snow_Thickness, Ice_Thickness, Ice_Conc),
               names_to = "Variable",
               values_to = "Value")

# Pivot longer to reshape the dataset for faceting


# Plot with one facet per variable
ggplot(My_ice, aes(x = date, y = Value, color = Shore)) +
  geom_line() +
  facet_wrap(~ Variable, scales = "free_y") +
  geom_vline(xintercept = as.Date("2050-01-01"), linetype = "dashed", color = "red") +
  labs(x = "Date", y = "Value", title = "Sea Ice Variables Over Time") +
  theme_minimal()

