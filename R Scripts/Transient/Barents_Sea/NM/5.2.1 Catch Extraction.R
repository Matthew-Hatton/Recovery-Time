## Run model for 2020 extracting catch of a large range of HR's
## Data will be used to make Figure 5 of recovery time manuscript

#### Setup ####
rm(list=ls())                                                                                              # Wipe the brain
Packages <- c("MiMeMo.tools", "exactextractr", "raster", "lubridate","StrathE2EPolar","furrr","tictoc")                     # List packages
lapply(Packages, library, character.only = TRUE)   
source("../@_Region_file_BS.R")

plan(multisession,workers = availableCores()-1) # parallel processing is good

tic()
master <- list(Biomasses = list(),
               inshore_catch_mat = list(),
               offshore_catch_mat = list(),
               inshore_land_mat = list(),
               offshore_land_mat = list(),
               inshore_disc_mat = list(),
               offshore_disc_mat = list()) #How are we going to save all of this?

#### LOAD MODEL AND EXAMPLE FILES ####
model <- e2ep_read("Barents_Sea",
                   "2011-2019") # read model
Boundary_template <- model[["data"]][["chemistry.drivers"]] # pull template                                   

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

#### Crashing the system ####
e2ep_transient <- function(relax,guilds_to_crash,crash) { # Guilds will take a vector of names of guilds to crash
  options(dplyr.summarise.inform = FALSE) # Turn off dplyr warnings
  p <- progressr::progressor(along = transient_years)
  
  ## DEBUG
  # guilds_to_crash <- "Demersal_fish"
  # relax <- 0
  # i = 1
  # crash <- 10
  
  model <- e2ep_read(model.name = "Barents_Sea",
                     model.variant = "2011-2019") # Read in new baseline model
  guilds <- c("Planktivorous_fish","Demersal_fish","Migratory_fish",
              "Benthos_susp-dep","Benthos_carn-scav","Zooplankton_carn",
              "Birds","Pinnipeds","Cetaceans","Macrophytes")
  positions <- match(guilds_to_crash,guilds)
  model[["data"]][["fleet.model"]][["HRscale_vector_multiplier"]] <- rep(0,length(model[["data"]][["fleet.model"]][["HRscale_vector_multiplier"]])) #turn off fishing
  model[["data"]][["fleet.model"]][["HRscale_vector_multiplier"]][positions] <- crash # Set a high HR for focal guild
  #### Chemistry ####
  model[["data"]][["physical.parameters"]][["xinshorewellmixedness"]] <- 1.8 # Reset Wellmixed coefficient - issue is potentially something to do with this. let's turn it off for now
  My_boundary_data <- readRDS("../Objects/Barents_Sea/NM/Boundary measurements.rds") %>%
    filter(Year %in% seq(2060,2070)) %>%    # Import data
    group_by(Month, Compartment, Variable) %>%                                                 # Average across years
    summarise(Measured = mean(Measured, na.rm = T)) %>%
    ungroup() %>%
    arrange(Month) %>%
    mutate(Compartment = factor(Compartment, levels = c("Inshore S", "Offshore S", "Offshore D"),
                                labels = c("Inshore S" = "SI", "Offshore S" = "SO", "Offshore D" = "D"))) %>%
    pivot_wider(names_from = c(Compartment, Variable), names_sep = "_", values_from = Measured) # Spread columns to match template                                                                      # Remove temporary column
  
  My_atmosphere <- readRDS(stringr::str_glue("../Objects/Barents_Sea/NM/Atmospheric N deposition.rds")) %>%
    filter(Year %in% seq(2010,max(.$Year))) %>%
    group_by(Month, Oxidation_state, Shore,  Year) %>%
    summarise(Measured = sum(Measured, na.rm = T)) %>%                                              # Sum across deposition states
    summarise(Measured = mean(Measured, na.rm = T)) %>%                                             # Average over years
    ungroup() %>%
    pivot_wider(names_from = c(Shore, Oxidation_state), values_from = Measured) %>%                     # Spread to match template
    arrange(Month)
  
  My_DIN_fix <- readRDS("../Objects/Barents_Sea/NM/Ammonia to DIN.rds")
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
    filter(Shore == "Combined" & Year %in% seq(2060,2070)) %>%               # Limit to reference period and variable - light only goes to 2019, so if past that, hold it at 2019 values
    group_by(Month) %>%  # Average across months
    summarise(Measured = mean(Measured, na.rm = T)) %>% 
    ungroup() %>% 
    arrange(Month)                                                            # Order to match template
  
  My_air_temp <- readRDS("../Objects/Barents_Sea/NM/Air temp and light.rds") %>% 
    filter(Shore %in% c("Inshore","Offshore") & Year %in% seq(2060,2070)) %>% 
    group_by(Month,Shore) %>% 
    summarise(Measured = mean(Measured)) %>% 
    ungroup() %>% 
    arrange(Month)
  
  My_H_Flows <- readRDS("../Objects/Barents_Sea/NM/H-Flows.rds") %>% 
    filter(Year %in% seq(2060,2070)) %>%                                     # Limit to reference period
    group_by(across(-c(Year, Flow))) %>%                                      # Group over everything except year and variable of interest
    summarise(Flow = mean(Flow, na.rm = T)) %>%                               # Average flows by month over years
    ungroup() %>% 
    left_join(My_scale,by = join_by(Shore,slab_layer)) %>%                                                   # Attach compartment volumes
    mutate(Flow = Flow/Volume) %>%                                            # Scale flows by compartment volume
    mutate(Flow = abs(Flow * 86400)) %>%                                      # Multiply for total daily from per second, and correct sign for "out" flows
    arrange(Month)                                                           # Order by month to match template
  
  
  My_V_Flows <- readRDS("../Objects/Barents_Sea/NM/vertical diffusivity.rds") %>%
    filter(Year %in% seq(2060,2070)) %>%                                     # Limit to reference period
    group_by(Month) %>% 
    summarise(V_diff = mean(Vertical_diffusivity, na.rm = T)) %>% 
    ungroup() %>% 
    arrange(Month)                                                            # Order by month to match template
  
  My_volumes <- readRDS("../Objects/Barents_Sea/NM/TS.rds") %>% 
    filter(Year %in% seq(2060,2070)) %>%                                     # Limit to reference period
    group_by(Compartment, Month) %>%                                          # By compartment and month
    summarise(across(c(DIN_avg,Phytoplankton_avg,Detritus_avg,Temperature_avg), mean, na.rm = T)) %>%         # Average across years for multiple columns
    ungroup() %>% 
    arrange(Month)                                                            # Order by month to match template
  
  My_ice <- readRDS("../Objects/Barents_Sea/NM/TS.rds") %>% 
    filter(Shore %in% c("Inshore","Offshore") & slab_layer == "S") %>%  # Remove Buffer Zone
    filter(Year %in% seq(2060,2070)) %>%  # Filter down to just the target year
    group_by(Month,Shore) %>% 
    summarise(Ice_Pres = mean(Ice_pres,na.rm = T),
              Snow_Thickness = mean(Snow_Thickness_avg,na.rm = T),
              Ice_Thickness = mean(Ice_Thickness_avg,na.rm = T),
              Ice_Conc = mean(Ice_conc_avg,na.rm = T))%>% 
    mutate(across(everything(), ~replace(., is.nan(.), 0)))
  
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
  
  results <- e2ep_run(model,nyears = 50) # Run model to s.s
  
    
    
    # Pull everything we need
  master[["Biomasses"]] <- results[["final.year.outputs"]][["mass_results_wholedomain"]]
    
  master[["inshore_catch_mat"]] <- results[["final.year.outputs"]][["inshore_catchmat"]]
  master[["offshore_catch_mat"]] <- results[["final.year.outputs"]][["offshore_catchmat"]]
    
  master[["inshore_land_mat"]] <- results[["final.year.outputs"]][["inshore_landmat"]]
  master[["offshore_land_mat"]] <- results[["final.year.outputs"]][["offshore_landmat"]]
    
  master[["inshore_disc_mat"]] <- results[["final.year.outputs"]][["inshore_discmat"]]
  master[["offshore_disc_mat"]] <- results[["final.year.outputs"]][["offshore_discmat"]]
    
  
  return(master)
  p()
}

transient_years <- seq(2050,2099) # How far do we want to compute?
relax_values <- 0
crash <- seq(0,5.6,0.2) # 5.6 given as 2x MSY from experiment 2.1
guilds_to_crash <- "Demersal_fish"

results_list <- future_map(crash,
                           ~ e2ep_transient(relax = relax_values,
                                            guilds_to_crash = guilds_to_crash,
                                            crash = .x),
                           .options = furrr_options(seed = TRUE),
                           .progress = F)

saveRDS(results_list,paste0("../Objects/Experiments/Crash/Paper/Catch_Road_To_Recovery_2070.RDS"))
toc()

# transient_years <- seq(2040,2050) # How far do we want to compute?
# relax_values <- 0
# crash <- seq(0,0.2,0.2) # baseline, MSY, 2x MSY
# guilds_to_crash <- "Demersal_fish"

# results_list <- future_map(crash, 
#                            ~ e2ep_transient(relax = relax_values, 
#                                             guilds_to_crash = guilds_to_crash, 
#                                             crash = .x),
#                            .options = furrr_options(seed = TRUE),
#                            .progress = F)


