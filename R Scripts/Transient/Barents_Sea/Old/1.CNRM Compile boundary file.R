## Overwrite example boundary data for transient dynamics
## Forcing: CNRM
## SSP: SSP126

#### Setup ####

rm(list=ls())                                                               # Wipe the brain
Packages <- c("MiMeMo.tools", "exactextractr", "raster", "lubridate")       # List packages
lapply(Packages, library, character.only = TRUE)   
source("./@_Region_file_BS.R")

Boundary_template <- read.csv("C:/Users/psb22188/AppData/Local/R/win-library/4.2/StrathE2EPolar/extdata/Models/Barents_Sea/2011-2019/Driving/chemistry_BS_2011-2019.csv")  # Read in example boundary drivers
NH4_boundary <- readRDS("./Objects/NH4 River Concentrations.RDS")
NO3_boundary <- readRDS("./Objects/NO3 River Concentrations.RDS")

## Iterate over different time periods ##

map2(c(2011),
     c(2019), ~{
       
       #### Last minute data manipulation ####
       
       My_boundary_data <- readRDS("./Objects/Boundary measurements.rds") %>% # Import data
         pivot_longer(
           cols = starts_with("D_") | starts_with("SO_") | starts_with("SI_"), # Pivot relevant columns
           names_to = "Temp", # Temporary column to hold original names
           values_to = "Measured" # Column for values
         ) %>%
         mutate(
           Compartment = case_when(
             grepl("^SI_", Temp) ~ "Inshore S",
             grepl("^SO_", Temp) ~ "Offshore S",
             grepl("^D_", Temp) ~ "Offshore D"
           ),
           Variable = sub("^(SI_|SO_|D_)", "", Temp) # Extract part after underscore
         ) %>%
         dplyr::select(-Temp) %>% # Remove temporary column
         filter(between(Year, .x, .y)) %>% # Limit to reference period
         group_by(Month, Compartment, Variable) %>% # Average across years
         summarise(Measured = mean(Measured, na.rm = T), .groups = "drop") %>%
         arrange(Month) %>% # Order months ascending
         mutate(
           Compartment = factor(
             Compartment,
             levels = c("Inshore S", "Offshore S", "Offshore D"),
             labels = c("SI", "SO", "D")
           )
         ) %>%
         pivot_wider(names_from = c(Compartment, Variable), names_sep = "_", values_from = Measured) # Spread columns to match template
       
       My_river_N <- readRDS("./Objects/NE River input.rds") %>%
         filter(between(as.numeric(Year), 2011, 2019)) %>%                                          # Limit to reference period
         mutate(Month = lubridate::month(Date)) %>%
         group_by(Month) %>%                                                                        # Average across years
         summarise(NO3 = mean(NO3, na.rm = T),
                   NH4 = mean(NH4, na.rm = T)) %>%
         ungroup() %>%
         arrange(Month)                                                                             # Order months ascending
       # 
       My_atmosphere <- readRDS(stringr::str_glue("./Objects/Atmospheric N deposition.rds")) %>%
         filter(between(Year, 2010, 2019)) %>%     
         filter(SSP == ssp) %>% # Limit to reference period
         group_by(Month, Oxidation_state, Shore,  Year) %>%
         summarise(Measured = sum(Measured, na.rm = T)) %>%                                         # Sum across deposition states
         summarise(Measured = mean(Measured, na.rm = T)) %>%                                        # Average over years
         ungroup() %>%
         pivot_wider(names_from = c(Shore, Oxidation_state), values_from = Measured) %>%            # Spread to match template
         arrange(Month)                                                                             # Order months ascending
       
       #### Create new file ####
       
       Boundary_new <- Boundary_template %>% 
         mutate(SO_nitrate = My_boundary_data$SO_NO3,
                SO_ammonia = My_boundary_data$SO_NH4,
                SO_phyt = My_boundary_data$SO_Diatoms + My_boundary_data$SO_Other_phytoplankton,
                SO_detritus = My_boundary_data$SO_Detritus,
                D_nitrate = My_boundary_data$D_NO3, 
                D_ammonia = My_boundary_data$D_NH4, 
                D_phyt = My_boundary_data$D_Diatoms + My_boundary_data$D_Other_phytoplankton,
                D_detritus = My_boundary_data$D_Detritus,
                SI_nitrate = My_boundary_data$SI_NO3,
                SI_ammonia = My_boundary_data$SI_NH4,
                SI_phyt = My_boundary_data$SI_Diatoms + My_boundary_data$SI_Other_phytoplankton, 
                SI_detritus = My_boundary_data$SI_Detritus,
                ## Rivers
                RIV_nitrate = NO3_boundary$monthly_no3,
                RIV_ammonia = NH4_boundary$monthly_NH4,
                RIV_detritus = 0,
                ## Atmosphere, daily deposition as monthly averages
                SO_ATM_nitrate_flux = My_atmosphere$Offshore_O,
                SO_ATM_ammonia_flux = My_atmosphere$Offshore_R,
                SI_ATM_nitrate_flux = My_atmosphere$Inshore_O,
                SI_ATM_ammonia_flux = My_atmosphere$Inshore_R,
                SI_other_nitrate_flux = 0,   # Can be used for scenarios
                SI_other_ammonia_flux = 0,
                SO_other_nitrate_flux = 0,   # Can be used for scenarios
                SO_other_ammonia_flux = 0,
                # ## Overhang
                # DO_nitrate = My_overhang$NO3,
                # DO_ammonia	= My_overhang$NH4,
                # DO_detritus = My_overhang$Detritus
         ) 
       
       write.csv(Boundary_new, file = "C:/Users/psb22188/AppData/Local/Programs/R/R-4.3.1/library/StrathE2EPolar/extdata/Models/West_Greenland/2011-2019/Driving/chemistry_WG_2011-2019.csv", row.names = F)
       
     })
fn <- "C:/Users/psb22188/AppData/Local/Programs/R/R-4.3.1/library/StrathE2EPolar/extdata/Models/West_Greenland/2011-2019/Driving/chemistry_BS_2011-2019.csv"
#Check its existence
if (file.exists(fn)) {
  #Delete file if it exists
  file.remove(fn)
}
