## Run transient dynamics, impact the system at 5 year intervals and time recovery.
## This script will pull the biomass of the impacts at various dates
## Also calculates a rolling MSY for each interval

#### Setup ####
rm(list=ls())                                                                                              # Wipe the brain
Packages <- c("MiMeMo.tools", "exactextractr", "raster", "lubridate","StrathE2EPolar","furrr","tictoc","progressr")                     # List packages
lapply(Packages, library, character.only = TRUE)   
source("../@_Region_file_BS.R")
handlers("cli")
handlers(global = TRUE)

plan(multisession,workers = availableCores()-1) # parallel processing is good

tic()
master <- list(All_Results = list(),
               Flow_Matrices = list(),
               Biomasses = list(),
               Network_Indicators = list(),
               Initial_Conditions = list(),
               Crash_year = list(),
               Crash_HR = list(),
               Flows = list()) #How are we going to save all of this?
transient_years <- seq(2020,2099) # How far do we want to compute? +1 to account for previous 10 year crashes (with 5 being the interval size)


#### LOAD MODEL AND EXAMPLE FILES ####
model <- e2ep_read("Barents_Sea",
                   "2011-2019") # read model
Boundary_template <- model[["data"]][["chemistry.drivers"]] # pull template                                   

## Unchanging
My_scale <- readRDS("../Objects/Domain_BS.rds") %>%                          # Calculate the volume of the three zones
  sf::st_drop_geometry() %>% 
  mutate(S = c(T, T),
         D = c(F, T)) %>% 
  gather(key = "slab_layer", value = "Exists", S, D) %>% 
  filter(Exists == T) %>%
  mutate(Elevation = c(Elevation[1], -60, Elevation[3] + 60)) %>% 
  mutate(Volume = area * abs(Elevation)) %>% 
  dplyr::select(Shore, slab_layer, Volume)

My_Waves <- readRDS("../Objects/Barents_Sea/NM/Significant wave height.rds") %>%  #*2000 - 2010   
  arrange(Month) %>% 
  group_by(Month) %>% 
  summarise(mean_height = mean(Waves))# Arrange to match template

NH4_boundary <- readRDS("../Objects/Barents_Sea/NM/River nitrate and ammonia.rds") %>% subset(select = c(Month,Ammonia)) # Read in NH4
NO3_boundary <- readRDS("../Objects/Barents_Sea/NM/River nitrate and ammonia.rds") %>% subset(select = c(Month,Nitrate)) # Read in NO3

My_atmosphere <- readRDS(stringr::str_glue("../Objects/Barents_Sea/NM/Atmospheric N deposition.rds")) %>%
  filter(Year %in% seq(2010,max(.$Year))) %>%
  group_by(Month, Oxidation_state, Shore,  Year) %>%
  summarise(Measured = sum(Measured, na.rm = T)) %>%                                              # Sum across deposition states
  summarise(Measured = mean(Measured, na.rm = T)) %>%                                             # Average over years
  ungroup() %>%
  pivot_wider(names_from = c(Shore, Oxidation_state), values_from = Measured) %>%                     # Spread to match template
  arrange(Month)

My_DIN_fix <- readRDS("../Objects/Barents_Sea/NM/Ammonia to DIN.rds")

My_SPM <- readRDS("../Objects/Barents_Sea/NM/Suspended particulate matter.rds") %>%
  filter(between(Year, 2011, 2019)) %>%                                     # Limit to reference period
  group_by(Shore, Month) %>%
  summarise(SPM = mean(SPM, na.rm = T)) %>%                                 # Average by month across years
  ungroup() %>%
  arrange(Month)                                                            # Order by month to match template

My_Rivers <- readRDS("../Objects/Barents_Sea/NM/River volume input.rds") %>%
  filter(between(Year, 2011, 2019)) %>%                                     # Limit to reference period
  group_by(Month) %>%
  summarise(Runoff = mean(Runoff, na.rm = T)) %>%                           # Average by month across years
  ungroup() %>%
  arrange(as.numeric(Month))                                                # Order by month to match template

My_Stress <- readRDS("../Objects/Barents_Sea/NM/Habitat disturbance.rds") %>%
  mutate(Month = factor(Month, levels = month.name)) %>%                    # Set month as a factor for non-alphabetical ordering
  arrange(Month)                                                            # Arrange to match template

#### Crashing the system ####
e2ep_transient_interval <- function(relax,guilds_to_crash,interval,nyears,progress = NULL) { # Guilds will take a vector of names of guilds to crash
  options(dplyr.summarise.inform = FALSE) # Turn off dplyr warnings
  
  interval <- seq(2020,2020,5)
  transient_years <- seq(2020,2030)
  relax_values <- 0
  guilds_to_crash <- "Demersal_fish"
  nyears <- 50
  
  model <- e2ep_read(model.name = "Barents_Sea",
                     model.variant = "2011-2019") # Read in new baseline model
  
  guilds <- c("Planktivorous_fish","Demersal_fish","Migratory_fish",
              "Benthos_susp-dep","Benthos_carn-scav","Zooplankton_carn",
              "Birds","Pinnipeds","Cetaceans","Macrophytes")
  
  positions <- match(guilds_to_crash,guilds) # match guild to crash
  
  # INTERVAL CRASH

  
  model[["data"]][["physical.parameters"]][["xinshorewellmixedness"]] <- 1.8 # Reset Wellmixed coefficient
  My_boundary_data <- readRDS("../Objects/Barents_Sea/NM/Boundary measurements.rds") %>%
    filter(Year %in% seq(interval-10,interval)) %>%    # Import data
    group_by(Month, Compartment, Variable) %>%                                                 # Average across years
    summarise(Measured = mean(Measured, na.rm = T)) %>%
    ungroup() %>%
    arrange(Month) %>%
    mutate(Compartment = factor(Compartment, levels = c("Inshore S", "Offshore S", "Offshore D"),
                                labels = c("Inshore S" = "SI", "Offshore S" = "SO", "Offshore D" = "D"))) %>%
    pivot_wider(names_from = c(Compartment, Variable), names_sep = "_", values_from = Measured) # Spread columns to match template
  
  Boundary_template <- model[["data"]][["chemistry.drivers"]]
  Boundary_new <- mutate(Boundary_template,
                         so_nitrate = My_boundary_data$SO_DIN * (1-filter(My_DIN_fix, Depth_layer == "Shallow")$Proportion),
                         so_ammonia = My_boundary_data$SO_DIN * filter(My_DIN_fix, Depth_layer == "Shallow")$Proportion,
                         so_phyt = My_boundary_data$SO_Phytoplankton,
                         so_detritus = My_boundary_data$SO_Detritus,
                         d_nitrate =  My_boundary_data$D_DIN * (1-filter(My_DIN_fix, Depth_layer == "Deep")$Proportion),
                         d_ammonia = My_boundary_data$D_DIN * filter(My_DIN_fix, Depth_layer == "Deep")$Proportion,
                         d_phyt = My_boundary_data$D_Phytoplankton,
                         d_detritus = My_boundary_data$D_Detritus,
                         si_nitrate = My_boundary_data$SI_DIN * (1-filter(My_DIN_fix, Depth_layer == "Shallow")$Proportion),
                         si_ammonia = My_boundary_data$SI_DIN * filter(My_DIN_fix, Depth_layer == "Shallow")$Proportion,
                         si_phyt = My_boundary_data$SI_Phytoplankton,
                         si_detritus = My_boundary_data$SI_Detritus,
                         rivnitrate = NO3_boundary$Nitrate,
                         rivammonia = NH4_boundary$Ammonia,
                         rivdetritus = 0,
                         so_atmnitrate = My_atmosphere$Offshore_O,
                         so_atmammonia = My_atmosphere$Offshore_R,
                         si_atmnitrate = My_atmosphere$Inshore_O,
                         si_atmammonia = My_atmosphere$Inshore_R,
                         si_othernitrate = 0,
                         si_otherammonia = 0)
  model[["data"]][["chemistry.drivers"]] <- Boundary_new
  #### Physics ####
  My_light <- readRDS("../Objects/Barents_Sea/NM/Air temp and light.rds") %>% 
    filter(Shore == "Combined" & Year %in% seq(interval-10,interval)) %>%               # Limit to reference period and variable - light only goes to 2019, so if past that, hold it at 2019 values
    group_by(Month) %>%  # Average across months
    summarise(Measured = mean(Measured, na.rm = T)) %>% 
    ungroup() %>% 
    arrange(Month)                                                            # Order to match template
  
  My_air_temp <- readRDS("../Objects/Barents_Sea/NM/Air temp and light.rds") %>% 
    filter(Shore %in% c("Inshore","Offshore") & Year %in% seq(interval-10,interval)) %>% 
    group_by(Month,Shore) %>% 
    summarise(Measured = mean(Measured)) %>% 
    ungroup() %>% 
    arrange(Month)
  
  My_H_Flows <- readRDS("../Objects/Barents_Sea/NM/H-Flows.rds") %>% 
    filter(Year %in% seq(interval-10,interval)) %>%    # Import data%>%                                     # Limit to reference period
    group_by(across(-c(Year, Flow))) %>%                                      # Group over everything except year and variable of interest
    summarise(Flow = mean(Flow, na.rm = T)) %>%                               # Average flows by month over years
    ungroup() %>% 
    left_join(My_scale,by = join_by(Shore,slab_layer)) %>%                                                   # Attach compartment volumes
    mutate(Flow = Flow/Volume) %>%                                            # Scale flows by compartment volume
    mutate(Flow = abs(Flow * 86400)) %>%                                      # Multiply for total daily from per second, and correct sign for "out" flows
    arrange(Month)                                                           # Order by month to match template
  
  
  My_V_Flows <- readRDS("../Objects/Barents_Sea/NM/vertical diffusivity.rds") %>%
    filter(Year %in% seq(interval-10,interval)) %>%    # Import data%>%                                     # Limit to reference period
    group_by(Month) %>% 
    summarise(V_diff = mean(Vertical_diffusivity, na.rm = T)) %>% 
    ungroup() %>% 
    arrange(Month)                                                            # Order by month to match template
  
  My_volumes <- readRDS("../Objects/Barents_Sea/NM/TS.rds") %>% 
    filter(Year %in% seq(interval-10,interval)) %>%    # Import data%>%                                     # Limit to reference period
    group_by(Compartment, Month) %>%                                          # By compartment and month
    summarise(across(c(DIN_avg,Phytoplankton_avg,Detritus_avg,Temperature_avg), mean, na.rm = T)) %>%         # Average across years for multiple columns
    ungroup() %>% 
    arrange(Month)                                                            # Order by month to match template
  
  My_ice <- readRDS("../Objects/Barents_Sea/NM/TS.rds") %>% 
    filter(Shore %in% c("Inshore","Offshore") & slab_layer == "S") %>%  # Remove Buffer Zone
    filter(Year %in% seq(interval-10,interval)) %>%    # Import data%>%  # Filter down to just the target year
    group_by(Month,Shore) %>% 
    summarise(Ice_Pres = mean(Ice_pres,na.rm = T),
              Snow_Thickness = mean(Snow_Thickness_avg,na.rm = T),
              Ice_Thickness = mean(Ice_Thickness_avg,na.rm = T),
              Ice_Conc = mean(Ice_conc_avg,na.rm = T))%>% 
    mutate(across(everything(), ~replace(., is.nan(.), 0)))
  
  
  Physics_template <- model[["data"]][["physics.drivers"]]
  
  Physics_template$sslight <-  My_light$Measured
  Physics_template$so_logespm <- log(filter(My_SPM,Shore == "Offshore")$SPM)
  Physics_template$si_logespm <- log(filter(My_SPM,Shore == "Inshore")$SPM)
  Physics_template$so_temp <- filter(My_volumes, Compartment == "Offshore S")$Temperature_avg
  Physics_template$d_temp <- filter(My_volumes, Compartment == "Offshore D")$Temperature_avg
  Physics_template$si_temp <- filter(My_volumes, Compartment == "Inshore S")$Temperature_avg
  Physics_template$rivervol <- My_Rivers$Runoff / filter(My_scale, Shore == "Inshore")$Volume
  Physics_template$logkvert <- log10(My_V_Flows$V_diff)
  Physics_template$mixlscale <- Physics_template$mixlscale
  Physics_template$upwelling <- 0
  Physics_template$so_inflow <- filter(My_H_Flows, slab_layer == "S", Shore == "Offshore", Neighbour == "Ocean", Direction == "In")$Flow
  Physics_template$d_inflow <- filter(My_H_Flows, slab_layer == "D", Shore == "Offshore", Neighbour == "Ocean", Direction == "In")$Flow
  Physics_template$si_inflow <- filter(My_H_Flows, slab_layer == "S", Shore == "Inshore", Neighbour == "Ocean", Direction == "In")$Flow
  Physics_template$si_outflow <- filter(My_H_Flows, slab_layer == "S", Shore == "Inshore", Neighbour == "Ocean", Direction == "Out")$Flow
  Physics_template$so_si_flow <- filter(My_H_Flows, slab_layer == "S", Shore == "Offshore", Neighbour == "Inshore", Direction == "Out")$Flow
  Physics_template$s1_pdist = filter(My_Stress, Shore == "Inshore", Habitat == "Silt")$Disturbance
  Physics_template$s2_pdist = filter(My_Stress, Shore == "Inshore", Habitat == "Sand")$Disturbance
  Physics_template$s3_pdist = filter(My_Stress, Shore == "Inshore", Habitat == "Gravel")$Disturbance
  Physics_template$d1_pdist = filter(My_Stress, Shore == "Offshore", Habitat == "Silt")$Disturbance
  Physics_template$d2_pdist = filter(My_Stress, Shore == "Offshore", Habitat == "Sand")$Disturbance
  Physics_template$d3_pdist = filter(My_Stress, Shore == "Offshore", Habitat == "Gravel")$Disturbance
  Physics_template$Inshore_waveheight <- My_Waves$mean_height
  Physics_template$so_icefree <- 1 - filter(My_ice, Shore == "Offshore")$Ice_Pres
  Physics_template$si_icefree <- 1 - filter(My_ice, Shore == "Inshore")$Ice_Pres
  Physics_template$so_icecov <- filter(My_ice, Shore == "Offshore")$Ice_Conc
  Physics_template$si_icecov <- filter(My_ice, Shore == "Inshore")$Ice_Conc
  Physics_template$so_icethick <- filter(My_ice, Shore == "Offshore")$Ice_Thickness
  Physics_template$si_icethick <- filter(My_ice, Shore == "Inshore")$Ice_Thickness
  Physics_template$so_snowthick <- filter(My_ice, Shore == "Offshore")$Snow_Thickness
  Physics_template$si_snowthick <- filter(My_ice, Shore == "Inshore")$Snow_Thickness
  Physics_template$so_airtemp <- filter(My_air_temp,Shore == "Offshore")$Measured
  Physics_template$si_airtemp <- filter(My_air_temp,Shore == "Inshore")$Measured
  
  # Replace with new drivers
  model[["data"]][["physics.drivers"]] <- Physics_template
  
  ## calculate MSY for current interval
  ycurve_res <- e2ep_run_ycurve(model,nyears = nyears,HRvector = seq(1,5,0.2),selection = "DEMERSAL")
  
  ## one of these per interval
  ## extract MSY - highest point on curve for Demersal fish
  
  focal <- ycurve_res %>% slice(which.max(.$DemFishland)) %>% 
    dplyr::select(c(DemFishland,DemFishHRmult))
  
  ## above gives us the MSY we should use in the next step
  MSY <- focal$DemFishHRmult
  MSY_2x <- 2 * MSY
  
  ## then plug in
  fishing <- c(1, MSY,MSY_2x)
  # fishing <- c(1)
  
  for (f in 1:length(fishing)) {
    ## DEBUG
    f = 1
    ## Crash the system
    model[["data"]][["fleet.model"]][["HRscale_vector_multiplier"]] <- rep(0,length(model[["data"]][["fleet.model"]][["HRscale_vector_multiplier"]])) #turn off fishing
    model[["data"]][["fleet.model"]][["HRscale_vector_multiplier"]][positions] <- fishing[f] # Set a HR for focal guild
    results <- e2ep_run(model,nyears = nyears) # Run model to s.s
    
    model[["data"]][["initial.state"]][1:length(e2ep_extract_start(model = model,results = results,
                                                                   csv.output = F)[,1])] <- e2ep_extract_start(model = model,results = results,
                                                                                                               csv.output = F)[,1] #plug in I.C to model
    
    ## Reset fishing within the system
    model[["data"]][["fleet.model"]][["HRscale_vector_multiplier"]] <- rep(0,length(model[["data"]][["fleet.model"]][["HRscale_vector_multiplier"]])) # Reset fishing
    model[["data"]][["fleet.model"]][["HRscale_vector_multiplier"]][positions] <- relax # reset matched fishing to specific value
    
    ## DEBUG
    # interval <- 2020
    # i <- 1
    # Def seq of years to compute
    years <- seq(interval + 1,max(transient_years))
    
    #### Run system from point of crash
    for (i in 1:length(years)) {
      #### Chemistry ####
      model[["data"]][["physical.parameters"]][["xinshorewellmixedness"]] <- 1.8 # Reset Wellmixed coefficient
      My_boundary_data <- readRDS("../Objects/Barents_Sea/NM/Boundary measurements.rds") %>%
        # filter(Year %in% years) %>%    # Import data
        filter(Year %in% seq(min(years)-11 + i,min(years) + i-1)) %>% 
        group_by(Month, Compartment, Variable) %>%                                                 # Average across years
        summarise(Measured = mean(Measured, na.rm = T)) %>%
        ungroup() %>%
        arrange(Month) %>%
        mutate(Compartment = factor(Compartment, levels = c("Inshore S", "Offshore S", "Offshore D"),
                                    labels = c("Inshore S" = "SI", "Offshore S" = "SO", "Offshore D" = "D"))) %>%
        pivot_wider(names_from = c(Compartment, Variable), names_sep = "_", values_from = Measured) # Spread columns to match template
      
      Boundary_template <- model[["data"]][["chemistry.drivers"]]
      Boundary_new <- mutate(Boundary_template,
                             so_nitrate = My_boundary_data$SO_DIN * (1-filter(My_DIN_fix, Depth_layer == "Shallow")$Proportion),
                             so_ammonia = My_boundary_data$SO_DIN * filter(My_DIN_fix, Depth_layer == "Shallow")$Proportion,
                             so_phyt = My_boundary_data$SO_Phytoplankton,
                             so_detritus = My_boundary_data$SO_Detritus,
                             d_nitrate =  My_boundary_data$D_DIN * (1-filter(My_DIN_fix, Depth_layer == "Deep")$Proportion),
                             d_ammonia = My_boundary_data$D_DIN * filter(My_DIN_fix, Depth_layer == "Deep")$Proportion,
                             d_phyt = My_boundary_data$D_Phytoplankton,
                             d_detritus = My_boundary_data$D_Detritus,
                             si_nitrate = My_boundary_data$SI_DIN * (1-filter(My_DIN_fix, Depth_layer == "Shallow")$Proportion),
                             si_ammonia = My_boundary_data$SI_DIN * filter(My_DIN_fix, Depth_layer == "Shallow")$Proportion,
                             si_phyt = My_boundary_data$SI_Phytoplankton,
                             si_detritus = My_boundary_data$SI_Detritus,
                             rivnitrate = NO3_boundary$Nitrate,
                             rivammonia = NH4_boundary$Ammonia,
                             rivdetritus = 0,
                             so_atmnitrate = My_atmosphere$Offshore_O,
                             so_atmammonia = My_atmosphere$Offshore_R,
                             si_atmnitrate = My_atmosphere$Inshore_O,
                             si_atmammonia = My_atmosphere$Inshore_R,
                             si_othernitrate = 0,
                             si_otherammonia = 0)
      model[["data"]][["chemistry.drivers"]] <- Boundary_new
      #### Physics ####
      My_light <- readRDS("../Objects/Barents_Sea/NM/Air temp and light.rds") %>% 
        filter(Shore == "Combined" & Year %in% years) %>%               # Limit to reference period and variable - light only goes to 2019, so if past that, hold it at 2019 values
        filter(Year %in% seq(min(years)-11 + i,min(years) + i-1)) %>% #compute for 10 years prior
        group_by(Month) %>%  # Average across months
        summarise(Measured = mean(Measured, na.rm = T)) %>% 
        ungroup() %>% 
        arrange(Month)                                                            # Order to match template
      
      My_air_temp <- readRDS("../Objects/Barents_Sea/NM/Air temp and light.rds") %>% 
        filter(Shore %in% c("Inshore","Offshore")) %>% 
        filter(Year %in% seq(min(years)-11 + i,min(years) + i-1)) %>% 
        group_by(Month,Shore) %>% 
        summarise(Measured = mean(Measured)) %>% 
        ungroup() %>% 
        arrange(Month)
      
      My_H_Flows <- readRDS("../Objects/Barents_Sea/NM/H-Flows.rds") %>% 
        # filter(Year %in% years) %>%                                     # Limit to reference period
        filter(Year %in% seq(min(years)-11 + i,min(years) + i-1)) %>% 
        group_by(across(-c(Year, Flow))) %>%                                      # Group over everything except year and variable of interest
        summarise(Flow = mean(Flow, na.rm = T)) %>%                               # Average flows by month over years
        ungroup() %>% 
        left_join(My_scale,by = join_by(Shore,slab_layer)) %>%                                                   # Attach compartment volumes
        mutate(Flow = Flow/Volume) %>%                                            # Scale flows by compartment volume
        mutate(Flow = abs(Flow * 86400)) %>%                                      # Multiply for total daily from per second, and correct sign for "out" flows
        arrange(Month)                                                           # Order by month to match template
      
      
      My_V_Flows <- readRDS("../Objects/Barents_Sea/NM/vertical diffusivity.rds") %>%
        # filter(Year %in% years) %>%                                     # Limit to reference period
        filter(Year %in% seq(min(years)-11 + i,min(years) + i-1)) %>% 
        group_by(Month) %>% 
        summarise(V_diff = mean(Vertical_diffusivity, na.rm = T)) %>% 
        ungroup() %>% 
        arrange(Month)                                                            # Order by month to match template
      
      My_volumes <- readRDS("../Objects/Barents_Sea/NM/TS.rds") %>% 
        # filter(Year %in% years) %>%                                     # Limit to reference period
        filter(Year %in% seq(min(years)-11 + i,min(years) + i-1)) %>% 
        group_by(Compartment, Month) %>%                                          # By compartment and month
        summarise(across(c(DIN_avg,Phytoplankton_avg,Detritus_avg,Temperature_avg), mean, na.rm = T)) %>%         # Average across years for multiple columns
        ungroup() %>% 
        arrange(Month)                                                            # Order by month to match template
      
      My_ice <- readRDS("../Objects/Barents_Sea/NM/TS.rds") %>% 
        filter(Shore %in% c("Inshore","Offshore") & slab_layer == "S") %>%  # Remove Buffer Zone
        # filter(Year %in% years) %>%  # Filter down to just the target year
        filter(Year %in% seq(min(years)-11 + i,min(years) + i-1)) %>% 
        group_by(Month,Shore) %>% 
        summarise(Ice_Pres = mean(Ice_pres,na.rm = T),
                  Snow_Thickness = mean(Snow_Thickness_avg,na.rm = T),
                  Ice_Thickness = mean(Ice_Thickness_avg,na.rm = T),
                  Ice_Conc = mean(Ice_conc_avg,na.rm = T))%>% 
        mutate(across(everything(), ~replace(., is.nan(.), 0)))
      
      
      Physics_template <- model[["data"]][["physics.drivers"]]
      
      #not behaving, manually replace
      Physics_template$sslight <-  My_light$Measured
      Physics_template$so_logespm <- log(filter(My_SPM,Shore == "Offshore")$SPM)
      Physics_template$si_logespm <- log(filter(My_SPM,Shore == "Inshore")$SPM)
      Physics_template$so_temp <- filter(My_volumes, Compartment == "Offshore S")$Temperature_avg
      Physics_template$d_temp <- filter(My_volumes, Compartment == "Offshore D")$Temperature_avg
      Physics_template$si_temp <- filter(My_volumes, Compartment == "Inshore S")$Temperature_avg
      Physics_template$rivervol <- My_Rivers$Runoff / filter(My_scale, Shore == "Inshore")$Volume
      Physics_template$logkvert <- log10(My_V_Flows$V_diff)
      Physics_template$mixlscale <- Physics_template$mixlscale
      Physics_template$upwelling <- 0
      Physics_template$so_inflow <- filter(My_H_Flows, slab_layer == "S", Shore == "Offshore", Neighbour == "Ocean", Direction == "In")$Flow
      Physics_template$d_inflow <- filter(My_H_Flows, slab_layer == "D", Shore == "Offshore", Neighbour == "Ocean", Direction == "In")$Flow
      Physics_template$si_inflow <- filter(My_H_Flows, slab_layer == "S", Shore == "Inshore", Neighbour == "Ocean", Direction == "In")$Flow
      Physics_template$si_outflow <- filter(My_H_Flows, slab_layer == "S", Shore == "Inshore", Neighbour == "Ocean", Direction == "Out")$Flow
      Physics_template$so_si_flow <- filter(My_H_Flows, slab_layer == "S", Shore == "Offshore", Neighbour == "Inshore", Direction == "Out")$Flow
      Physics_template$s1_pdist = filter(My_Stress, Shore == "Inshore", Habitat == "Silt")$Disturbance
      Physics_template$s2_pdist = filter(My_Stress, Shore == "Inshore", Habitat == "Sand")$Disturbance
      Physics_template$s3_pdist = filter(My_Stress, Shore == "Inshore", Habitat == "Gravel")$Disturbance
      Physics_template$d1_pdist = filter(My_Stress, Shore == "Offshore", Habitat == "Silt")$Disturbance
      Physics_template$d2_pdist = filter(My_Stress, Shore == "Offshore", Habitat == "Sand")$Disturbance
      Physics_template$d3_pdist = filter(My_Stress, Shore == "Offshore", Habitat == "Gravel")$Disturbance
      Physics_template$Inshore_waveheight <- My_Waves$mean_height
      Physics_template$so_icefree <- 1 - filter(My_ice, Shore == "Offshore")$Ice_Pres
      Physics_template$si_icefree <- 1 - filter(My_ice, Shore == "Inshore")$Ice_Pres
      Physics_template$so_icecov <- filter(My_ice, Shore == "Offshore")$Ice_Conc
      Physics_template$si_icecov <- filter(My_ice, Shore == "Inshore")$Ice_Conc
      Physics_template$so_icethick <- filter(My_ice, Shore == "Offshore")$Ice_Thickness
      Physics_template$si_icethick <- filter(My_ice, Shore == "Inshore")$Ice_Thickness
      Physics_template$so_snowthick <- filter(My_ice, Shore == "Offshore")$Snow_Thickness
      Physics_template$si_snowthick <- filter(My_ice, Shore == "Inshore")$Snow_Thickness
      Physics_template$so_airtemp <- filter(My_air_temp,Shore == "Offshore")$Measured
      Physics_template$si_airtemp <- filter(My_air_temp,Shore == "Inshore")$Measured
      
      # Replace with new drivers
      model[["data"]][["physics.drivers"]] <- Physics_template
      
      results <- tryCatch({
        e2ep_run(model = model, nyears = 1)
      }, error = function(e) {
        message("\n An error occurred during e2ep_run: ", e$message,"\n Error occured at i = ",i,". Year = ",transient_years[i])
        #saveRDS(master,paste0("../Objects/Experiments/Crash/FAILED_AT_",transient_years[i],"Demersal_crash_",crash,"_relax_",relax,".RDS"))
        return(master)
      })
      
      # Pull everything we need
      master[["Biomasses"]][[paste0(interval)]][[paste0("HR = ",fishing[f])]][[paste0(interval + i)]] <- results[["final.year.outputs"]][["mass_results_wholedomain"]]
      master[["Network_Indicators"]][[paste0(interval)]][[paste0("HR = ",fishing[f])]][[paste0(interval + i)]] <- results[["final.year.outputs"]][["NetworkIndexResults"]]
      master[["Crash_year"]] <- interval
      master[["Flows"]][[paste0(interval)]][[paste0("HR = ",fishing[f])]][[paste0(interval + i)]] <- results[["final.year.outputs"]][["flow_matrix_all_fluxes"]]

      
      #Extract I.C
      init_con <- e2ep_extract_start(model = model,results = results,
                                     csv.output = F)
      
      #Reinsert I.C
      model[["data"]][["initial.state"]][1:nrow(init_con)] <- e2ep_extract_start(model = model,results = results,
                                                                                 csv.output = F)[,1]
      # master[["Biomasses"]][[paste0(interval)]][["HR"]] <- fishing[f]
      
    }

  }
  return(master)

  
}

## TEST
# interval <- seq(2020,2025,5)
# transient_years <- seq(2020,2030)
# relax_values <- 0
# guilds_to_crash <- "Demersal_fish"
# nyears <- 1

interval <- seq(2020,2085,5)
transient_years <- seq(2020,2099)
relax_values <- 0
guilds_to_crash <- "Demersal_fish"
nyears <- 50

res <- future_map(.x = interval,
              ~ {
                
                e2ep_transient_interval(relax = relax_values,
                                        guilds_to_crash = guilds_to_crash,
                                        interval = .x,
                                        nyears = nyears)
              },
              .options = furrr_options(seed = TRUE),
              .progress = T)





saveRDS(res,paste0("../Objects/Experiments/Rolling Crash/Rolling_Crash_and_MSY_Demersal.RDS"))
toc()



