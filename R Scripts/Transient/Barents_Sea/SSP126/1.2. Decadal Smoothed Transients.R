## Overwrite example boundary data and run transient dynamics

#### Setup ####
rm(list=ls())                                                                                              # Wipe the brain
Packages <- c("MiMeMo.tools", "exactextractr", "raster", "lubridate","StrathE2EPolar","furrr","tictoc")                     # List packages
lapply(Packages, library, character.only = TRUE)   
source("../Objects/@_Region_file_BS.R")

plan(multisession)

tic()
master <- list(All_Results = list(),
               Flow_Matrices = list(),
               Biomasses = list(),
               Network_Indicators = list(),
               Initial_Conditions = list()) #How are we going to save all of this?
Force <- "CNRM" # What forcing are we using? GFDL or CNRM
ssp <- "ssp126" # What SSP are we using? ssp126 or ssp370                                                                      
transient_years <- seq(2010,2061) # How far do we want to compute?

#### LOAD MODEL AND EXAMPLE FILES ####
model <- e2ep_read(model.name = "Barents_Sea",
                   model.variant = "2011-2019")
Boundary_template <- model[["data"]][["chemistry.drivers"]]                                    

My_scale <- readRDS("./Objects/Domains_BS.rds") %>%                          # Calculate the volume of the three zones
  sf::st_drop_geometry() %>% 
  mutate(S = c(T, T),
         D = c(F, T)) %>% 
  gather(key = "slab_layer", value = "Exists", S, D) %>% 
  filter(Exists == T) %>%
  mutate(Elevation = c(Elevation[1], -60, Elevation[3] + 60)) %>% 
  mutate(Volume = area * abs(Elevation)) %>% 
  dplyr::select(Shore, slab_layer, Volume)

My_Waves <- readRDS("../Objects/Significant wave height BS.rds") %>%  #*2000 - 2010   
  arrange(month) %>% 
  group_by(month) %>% 
  summarise(mean_height = mean(mean_height))# Arrange to match template

NH4_boundary <- readRDS("../Objects/NH4 River Concentrations BS.RDS")                                          # Read in NH4
NO3_boundary <- readRDS("../Objects/NO3 River Concentrations BS.RDS")                                          # Read in NO3

#### Crashing the system ####


e2ep_transient <- function(x) {
  options(dplyr.summarise.inform = FALSE) # Turn off dplyr warnings
  model <- e2ep_read(model.name = "Barents_Sea",
                     model.variant = "2011-2019") # Read in new baseline model
  model[["data"]][["fleet.model"]][["HRscale_vector_multiplier"]][1:10] <- 10 # Set a high HR for DF
  print("Running Crashed System...")
  results <- e2ep_run(model,nyears = 50) # Run model again
  model[["data"]][["initial.state"]][1:length(e2ep_extract_start(model = model,results = results,
                                                                 csv.output = F)[,1])] <- e2ep_extract_start(model = model,results = results,
                                                                                                             csv.output = F)[,1]
  B_X <- results[["final.year.outputs"]][["mass_results_wholedomain"]]$Model_annual_mean[27] # Get Crashed biomass
  model[["data"]][["fleet.model"]][["HRscale_vector_multiplier"]][1:2] <- x # reset fishing to value specified
  #pb <- txtProgressBar(min = 0, max = length(transient_years), style = 3)
  #### Iterate over different time periods ####
  for (i in 1:length(transient_years[1:41])) {
    #### Chemistry ####
    My_boundary_data <- readRDS("../Objects/Boundary measurements.rds") %>%                                     # Import data
      pivot_longer(
        cols = starts_with("D_") | starts_with("SO_") | starts_with("SI_"),       # Pivot relevant columns
        names_to = "Temp",                                                            # Temporary column to hold original names
        values_to  = "Measured"                                                         # Column for values
      ) %>%
      mutate(
        Compartment  = case_when(
          grepl("^SI_", Temp) ~ "Inshore S",
          grepl("^SO_", Temp) ~ "Offshore S",
          grepl("^D_", Temp) ~ "Offshore D"
        ),
        Variable = sub("^(SI_|SO_|D_)", "", Temp)                                     # Extract part after underscore
      ) %>%
      dplyr::select(-Temp) %>%                                                                          # Remove temporary column
      filter(Year %in% (transient_years[seq(i,i+10)])) %>%                                                            # Limit to reference period
      group_by(Month, Compartment, Variable) %>%                                                        # Average across years
      summarise(Measured = mean(Measured, na.rm = T), .groups = "drop") %>%
      arrange(Month) %>%                                                                                # Order months ascending
      mutate(
        Compartment  = factor(
          Compartment,
          levels = c("Inshore S", "Offshore S", "Offshore D"),
          labels = c("SI", "SO", "D")
        )
      ) %>%
      pivot_wider(names_from = c(Compartment, Variable), names_sep = "_", values_from = Measured) # Spread columns to match template
    
    My_atmosphere <- readRDS(stringr::str_glue("../Objects/Atmospheric N deposition.rds")) %>%
      filter(Year %in% (transient_years[seq(i,i+10)])) %>%     
      group_by(Month, Oxidation_state, Shore,  Year) %>%
      summarise(Measured = sum(Measured, na.rm = T)) %>%                                              # Sum across deposition states
      summarise(Measured = mean(Measured, na.rm = T)) %>%                                             # Average over years
      ungroup() %>%
      pivot_wider(names_from = c(Shore, Oxidation_state), values_from = Measured) %>%                     # Spread to match template
      arrange(Month)
    # Create list to map
    replacement_values <- list(
      so_nitrate   = My_boundary_data$SO_NO3,
      so_ammonia   = My_boundary_data$SO_NH4,
      so_phyt      = My_boundary_data$SO_Diatoms + My_boundary_data$SO_Other_phytoplankton,
      so_detritus  = My_boundary_data$SO_Detritus,
      d_nitrate    = My_boundary_data$D_NO3,
      d_ammonia    = My_boundary_data$D_NH4,
      d_phyt       = My_boundary_data$D_Diatoms + My_boundary_data$D_Other_phytoplankton,
      d_detritus   = My_boundary_data$D_Detritus,
      si_nitrate   = My_boundary_data$SI_NO3,
      si_ammonia   = My_boundary_data$SI_NH4,
      si_phyt      = My_boundary_data$SI_Diatoms + My_boundary_data$SI_Other_phytoplankton,
      si_detritus  = My_boundary_data$SI_Detritus,
      rivnitrate   = NO3_boundary$monthly_no3,
      rivammonia   = NH4_boundary$monthly_NH4,
      rivdetritus  = 0,
      so_atmnitrate = My_atmosphere$Offshore_O,
      so_atmammonia = My_atmosphere$Offshore_R,
      si_atmnitrate = My_atmosphere$Inshore_O,
      si_atmammonia = My_atmosphere$Inshore_R,
      si_othernitrate = 0,
      si_otherammonia = 0
    )
    # Replace values
    Boundary_template %>%
      mutate(across(all_of(names(replacement_values)), ~ replacement_values[[cur_column()]])) -> model[["data"]][["chemistry.drivers"]]
    
    
    #### Physics ####
    My_light <- readRDS("../Objects/light.rds") %>% 
      filter(Forcing == Force & Year == ifelse(transient_years[i] <= 2019,transient_years[i],2019)) %>%               # Limit to reference period and variable - light only goes to 2019, so if past that, hold it at 2019 values
      filter(SSP %in% c("hist","ssp370")) %>% 
      group_by(Month,SSP,Forcing) %>%  # Average across months
      summarise(Measured = mean(Light, na.rm = T)) %>% 
      ungroup() %>% 
      arrange(Month)                                                            # Order to match template
    
    My_H_Flows <- readRDS("../Objects/H-Flows.rds") %>% 
      filter(Year %in% transient_years[seq(i,i+10)]) %>%                                     # Limit to reference period
      filter(SSP %in% c(ssp)) %>% 
      group_by(across(-c(Year, Flow))) %>%                                      # Group over everything except year and variable of interest
      summarise(Flow = mean(Flow, na.rm = T)) %>%                               # Average flows by month over years
      ungroup() %>% 
      left_join(My_scale,by = join_by(Shore,slab_layer)) %>%                                                   # Attach compartment volumes
      mutate(Flow = Flow/Volume) %>%                                            # Scale flows by compartment volume
      mutate(Flow = abs(Flow * 86400)) %>%                                      # Multiply for total daily from per second, and correct sign for "out" flows
      arrange(Month) %>%                                                             # Order by month to match template
      filter(Forcing == Force)
    
    My_V_Flows <- readRDS("../Objects/vertical diffusivity.rds") %>%
      filter(Year %in% transient_years[seq(i,i+10)]) %>%                                     # Limit to reference period
      filter(SSP %in% c(ssp)) %>% 
      group_by(Month) %>% 
      summarise(V_diff = mean(Vertical_diffusivity, na.rm = T)) %>% 
      ungroup() %>% 
      arrange(Month)                                                            # Order by month to match template
    
    My_volumes <- readRDS("../Objects/TS.rds") %>% 
      filter(Year %in% transient_years[seq(i,i+10)]) %>%                                     # Limit to reference period
      group_by(Compartment, Month) %>%                                          # By compartment and month
      summarise(across(c(NO3_avg,NH4_avg,Diatoms_avg,Other_phytoplankton_avg,Detritus_avg,Temperature_avg), mean, na.rm = T)) %>%         # Average across years for multiple columns
      ungroup() %>% 
      arrange(Month)                                                            # Order by month to match template
    
    My_ice <- readRDS("../Objects/Ice_Summary_TEST.rds") %>% 
      filter(Shore %in% c("Inshore","Offshore")) %>%  # Remove Buffer Zone
      filter(Model == Force) %>%  # Access just one scenario
      filter(Year %in% transient_years[seq(i,i+10)]) %>%  # Filter down to just the target year
      group_by(Month,Shore) %>% 
      summarise(Ice_Pres = mean(Ice_pres),
                Snow_Thickness = mean(Snow_Thickness),
                Ice_Thickness = mean(Ice_Thickness),
                Ice_Conc = mean(Ice_conc))
    
    Physics_template <- model[["data"]][["physics.drivers"]]
    
    #not behaving, manually replace
    Physics_template$sslight <-  My_light$Measured
    # Physics_template$so_logespm <- Physics_template$SO_LogeSPM
    # Physics_template$si_logespm <- Physics_template$SI_LogeSPM
    Physics_template$so_temp <- filter(My_volumes, Compartment == "Offshore S")$Temperature_avg
    Physics_template$d_temp <- filter(My_volumes, Compartment == "Offshore D")$Temperature_avg
    Physics_template$si_temp <- filter(My_volumes, Compartment == "Inshore S")$Temperature_avg
    #Physics_template$rivervol <- Physics_template$Rivervol_SI
    Physics_template$logkvert <- log10(My_V_Flows$V_diff)
    #Physics_template$mixlscale <- Physics_template$mixLscale
    Physics_template$upwelling <- 0
    Physics_template$so_inflow <- filter(My_H_Flows, slab_layer == "S", Shore == "Offshore", Neighbour == "Ocean", Direction == "In")$Flow
    Physics_template$d_inflow <- filter(My_H_Flows, slab_layer == "D", Shore == "Offshore", Neighbour == "Ocean", Direction == "In")$Flow
    Physics_template$si_inflow <- filter(My_H_Flows, slab_layer == "S", Shore == "Inshore", Neighbour == "Ocean", Direction == "In")$Flow
    Physics_template$si_outflow <- filter(My_H_Flows, slab_layer == "S", Shore == "Inshore", Neighbour == "Ocean", Direction == "Out")$Flow
    Physics_template$so_si_flow <- filter(My_H_Flows, slab_layer == "S", Shore == "Offshore", Neighbour == "Inshore", Direction == "Out")$Flow
    # Physics_template$s1_pdist <- Physics_template$habS1_pdist
    # Physics_template$s2_pdist <- Physics_template$habS2_pdist
    # Physics_template$s3_pdist <- Physics_template$habS3_pdist
    # Physics_template$d1_pdist <- Physics_template$habD1_pdist
    # Physics_template$d2_pdist <- Physics_template$habD2_pdist
    # Physics_template$d3_pdist <- Physics_template$habD3_pdist
    Physics_template$Inshore_waveheight <- My_Waves$mean_height
    Physics_template$so_icefree <- 1 - filter(My_ice, Shore == "Offshore")$Ice_Pres
    Physics_template$si_icefree <- 1 - filter(My_ice, Shore == "Inshore")$Ice_Pres
    Physics_template$so_icecov <- filter(My_ice, Shore == "Offshore")$Ice_Conc
    Physics_template$si_icecov <- filter(My_ice, Shore == "Inshore")$Ice_Conc
    Physics_template$so_icethick <- filter(My_ice, Shore == "Offshore")$Ice_Thickness
    Physics_template$si_icethick <- filter(My_ice, Shore == "Inshore")$Ice_Thickness
    Physics_template$so_snowthick <- filter(My_ice, Shore == "Offshore")$Snow_Thickness
    Physics_template$si_snowthick <- filter(My_ice, Shore == "Inshore")$Snow_Thickness
    # Physics_template$so_airtemp <- Physics_template$SO_AirTemp
    # Physics_template$si_airtemp <- Physics_template$SI_AirTemp
    
    # ## Replace with new drivers
    model[["data"]][["physics.drivers"]] <- Physics_template #lsoda error here
    
    results <- e2ep_run(model = model,
                        nyears = 1)
    
    #Pull everything we need
    master[["All_Results"]][[paste0(transient_years[i])]] <- results
    master[["Flow_Matrices"]][[paste0(transient_years[i])]] <- results[["final.year.outputs"]][["flow_matrix_all_fluxes"]]
    master[["Biomasses"]][[paste0(transient_years[i])]] <- results[["final.year.outputs"]][["mass_results_wholedomain"]]
    master[["Network_Indicators"]][[paste0(transient_years[i])]] <- results[["final.year.outputs"]][["NetworkIndexResults"]]
    
    
    #Extract I.C
    init_con <- e2ep_extract_start(model = model,results = results,
                                   csv.output = F)
    #Store I.C
    master[["Initial_Conditions"]][[paste0(transient_years[i])]] <- init_con
    
    #Reinsert I.C
    model[["data"]][["initial.state"]][1:nrow(init_con)] <- e2ep_extract_start(model = model,results = results,
                                                                               csv.output = F)[,1]
  }
  return(master)
}

top_level <- future_map(c(0,1,2,3), e2ep_transient,.progress = F)
toc()

saveRDS(top_level,"../Objects/biomass_transients_smoothed_climate.RDS")
