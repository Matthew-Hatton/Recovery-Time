## Script to calculate the initial biomass of each guild given climate driven effects (with a baseline level of fishing for the 2010s)
## Will compute a decadal average per time step (1 year) and input that per year
## Basically smooths the input data

#### Setup ####
rm(list=ls())                                                                                              # Wipe the brain
Packages <- c("MiMeMo.tools", "exactextractr", "raster", "lubridate",
              "StrathE2EPolarV2","StrathE2EPolar","furrr","tictoc")    # List packages
lapply(Packages, library, character.only = TRUE)   
source("../@_Region_file_BS.R")

plan(multisession)

tic()

Force <- "CNRM" # What forcing are we using? GFDL or CNRM
ssp <- "ssp126" # What SSP are we using? ssp126 or ssp370                                                                      
transient_years <- seq(2010,2069) # How far do we want to compute?

e2ep_transient_baseline <- function(hr_scale,guilds_to_crash){
  options(dplyr.summarise.inform = FALSE) # Turn off dplyr warnings
  pb <- txtProgressBar(min = 0, max = 100, style = 3)
  
  # Debugging
  guilds_to_crash <- c("Demersal_fish")
  i <- 13
  hr_scale <- 0

  
  master <- list(All_Results = list(),
                 Flow_Matrices = list(),
                 Biomasses = list(),
                 Network_Indicators = list(),
                 Initial_Conditions = list()) #How are we going to save all of this?
  
  
  #### LOAD MODEL AND EXAMPLE FILES ####
  model <- e2ep_read(model.name = "Barents_Sea",
                     model.variant = "2011-2019") # Read in basic model
  guilds <- c("Planktivorous_fish","Demersal_fish","Migratory_fish",
              "Benthos_susp-dep","Benthos_carn-scav","Zooplankton_carn",
              "Birds","Pinnipeds","Cetaceans","Macrophytes") # Define the guilds we can fish
  positions <- match(guilds_to_crash,guilds) # Find position of interested guild
  
  # Set fishing to 0
  model[["data"]][["fleet.model"]][["HRscale_vector_multiplier"]] <- rep(0,length(model[["data"]][["fleet.model"]][["HRscale_vector_multiplier"]]))
  
  # Change target guild
  model[["data"]][["fleet.model"]][["HRscale_vector_multiplier"]][positions] <- rep(hr_scale,length(positions))

  Boundary_template <- model[["data"]][["chemistry.drivers"]]                                    
  
  My_scale <- readRDS("../Objects/Domain_BS.rds") %>%                            # Calculate the volume of the three zones
    sf::st_drop_geometry() %>% 
    mutate(S = c(T, T),
           D = c(F, T)) %>% 
    gather(key = "slab_layer", value = "Exists", S, D) %>% 
    filter(Exists == T) %>%
    mutate(Elevation = c(Elevation[1], -60, Elevation[3] + 60)) %>% 
    mutate(Volume = area * abs(Elevation)) %>% 
    dplyr::select(Shore, slab_layer, Volume)
  
  My_Waves <- readRDS("../Objects/Barents_Sea/Objects/Significant wave height.rds") %>%  #*2000 - 2010   
    arrange(Month) %>% 
    group_by(Month) %>% 
    summarise(mean_height = mean(Waves))# Arrange to match template
  
  My_atmosphere <- readRDS(stringr::str_glue("../Objects/Barents_Sea/Objects/Atmospheric N deposition.rds")) %>%
    # filter(Year %in% (transient_years[seq(i,i+10)])) %>%
    group_by(Month, Oxidation_state, Shore,  Year) %>%
    summarise(Measured = sum(Measured, na.rm = T)) %>%                                              # Sum across deposition states
    summarise(Measured = mean(Measured, na.rm = T)) %>%                                             # Average over years
    ungroup() %>%
    pivot_wider(names_from = c(Shore, Oxidation_state), values_from = Measured) %>%                     # Spread to match template
    arrange(Month)
  
  My_H_Flows <- readRDS("../Objects/Barents_Sea/Objects/H-Flows.rds") %>% 
    #filter(Year %in% transient_years[seq(i,i+10)]) %>%                                     # Limit to reference period
    #filter(SSP %in% c(ssp)) %>% 
    group_by(across(-c(Year, Flow))) %>%                                      # Group over everything except year and variable of interest
    summarise(Flow = mean(Flow, na.rm = T)) %>%                               # Average flows by month over years
    ungroup() %>% 
    left_join(My_scale,by = join_by(Shore,slab_layer)) %>%                                                   # Attach compartment volumes
    mutate(Flow = Flow/Volume) %>%                                            # Scale flows by compartment volume
    mutate(Flow = abs(Flow * 86400)) %>%                                      # Multiply for total daily from per second, and correct sign for "out" flows
    arrange(Month)         
  
  NH4_boundary <- readRDS("../Objects/NH4 River Concentrations BS.RDS")                                          # Read in NH4
  NO3_boundary <- readRDS("../Objects/NO3 River Concentrations BS.RDS")                                          # Read in NO3
  
  for (i in 1:length(transient_years[1:(length(transient_years)-10)])) { # We need to make sure the loop cuts at 2050. To compute that decadal average we don't want to run out of data
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
      si_nitrate   = My_boundary_data$SI_NO3, # - could be culprit
      si_ammonia   = My_boundary_data$SI_NH4, # - could be culprit
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
    My_light <- readRDS("../Objects/Barents_Sea/Objects/Air temp and light.rds") %>% 
      filter(Shore == "Combined" & Year %in% transient_years[seq(i,i+10)]) %>%               # Limit to reference period and variable - light only goes to 2019, so if past that, hold it at 2019 values
      #filter(SSP %in% c("hist","ssp370")) %>% 
      # group_by(Month,SSP,Forcing) %>%  # Average across months
      group_by(Month) %>%  # Average across months
      summarise(Measured = mean(Measured, na.rm = T)) %>% 
      ungroup() %>% 
      arrange(Month)                                                            # Order to match template
    
    My_air_temp <- readRDS("../Objects/Barents_Sea/Objects/Air temp and light.rds") %>% 
      filter(Shore %in% c("Inshore","Offshore") & Year %in% transient_years[seq(i,i+10)]) %>% 
      group_by(Month,Shore) %>% 
      summarise(Measured = mean(Measured)) %>% 
      ungroup() %>% 
      arrange(Month)
    
                                                  # Order by month to match template
    
    
    My_V_Flows <- readRDS("../Objects/Barents_Sea/Objects/vertical diffusivity.rds") %>%
      filter(Year %in% transient_years[seq(i,i+10)]) %>%                                     # Limit to reference period
      #filter(SSP %in% c(ssp)) %>% 
      group_by(Month) %>% 
      summarise(V_diff = mean(Vertical_diffusivity, na.rm = T)) %>% 
      ungroup() %>% 
      arrange(Month)                                                            # Order by month to match template
    
    My_volumes <- readRDS("../Objects/Barents_Sea/Objects/TS.rds") %>% 
      filter(Year %in% transient_years[seq(i,i+10)]) %>%                                     # Limit to reference period
      group_by(Compartment, Month) %>%                                          # By compartment and month
      summarise(across(c(DIN_avg,Phytoplankton_avg,Detritus_avg,Temperature_avg), mean, na.rm = T)) %>%         # Average across years for multiple columns
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
    
    My_SPM <- readRDS("../Objects/Barents_Sea/Objects/Suspended particulate matter.rds") %>%
      filter(between(Year, 2011, 2019)) %>%                                     # Limit to reference period
      group_by(Shore, Month) %>%
      summarise(SPM = mean(SPM, na.rm = T)) %>%                                 # Average by month across years
      ungroup() %>%
      arrange(Month)                                                            # Order by month to match template
    
    My_Rivers <- readRDS("../Objects/Barents_Sea/Objects/River volume input.rds") %>%
      filter(between(Year, 2011, 2019)) %>%                                     # Limit to reference period
      group_by(Month) %>%
      summarise(Runoff = mean(Runoff, na.rm = T)) %>%                           # Average by month across years
      ungroup() %>%
      arrange(as.numeric(Month))                                                # Order by month to match template
    
    My_Stress <- readRDS("../Objects/Barents_Sea/Objects/Habitat disturbance.rds") %>%
      mutate(Month = factor(Month, levels = month.name)) %>%                    # Set month as a factor for non-alphabetical ordering
      arrange(Month)                                                            # Arrange to match template
    
    
    Physics_template <- model[["data"]][["physics.drivers"]]
    
    #not behaving, manually replace
    Physics_template$sslight <-  My_light$Measured
    Physics_template$so_logespm <- filter(My_SPM,Shore == "Offshore")$SPM
    Physics_template$si_logespm <- filter(My_SPM,Shore == "Inshore")$SPM
    Physics_template$so_temp <- filter(My_volumes, Compartment == "Offshore S")$Temperature_avg
    Physics_template$d_temp <- filter(My_volumes, Compartment == "Offshore D")$Temperature_avg
    Physics_template$si_temp <- filter(My_volumes, Compartment == "Inshore S")$Temperature_avg
    Physics_template$rivervol <- My_Rivers$Runoff
    Physics_template$logkvert <- log10(My_V_Flows$V_diff)
    #Physics_template$mixlscale <- Physics_template$mixLscale
    Physics_template$upwelling <- 0
    Physics_template$so_inflow <- filter(My_H_Flows, slab_layer == "S", Shore == "Offshore", Neighbour == "Ocean", Direction == "In")$Flow
    Physics_template$d_inflow <- filter(My_H_Flows, slab_layer == "D", Shore == "Offshore", Neighbour == "Ocean", Direction == "In")$Flow
    Physics_template$si_inflow <- filter(My_H_Flows, slab_layer == "S", Shore == "Inshore", Neighbour == "Ocean", Direction == "In")$Flow
    Physics_template$si_outflow <- filter(My_H_Flows, slab_layer == "S", Shore == "Inshore", Neighbour == "Ocean", Direction == "Out")$Flow
    Physics_template$so_si_flow <- filter(My_H_Flows, slab_layer == "S", Shore == "Offshore", Neighbour == "Inshore", Direction == "Out")$Flow
    Physics_template$habS1_pdist = filter(My_Stress, Shore == "Inshore", Habitat == "Silt")$Disturbance
    Physics_template$habS2_pdist = filter(My_Stress, Shore == "Inshore", Habitat == "Sand")$Disturbance
    Physics_template$habS3_pdist = filter(My_Stress, Shore == "Inshore", Habitat == "Gravel")$Disturbance
    Physics_template$habD1_pdist = filter(My_Stress, Shore == "Offshore", Habitat == "Silt")$Disturbance
    Physics_template$habD2_pdist = filter(My_Stress, Shore == "Offshore", Habitat == "Sand")$Disturbance
    Physics_template$habD3_pdist = filter(My_Stress, Shore == "Offshore", Habitat == "Gravel")$Disturbance
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

    # ## Replace with new drivers
    model[["data"]][["physics.drivers"]] <- Physics_template 
    
    # results <- StrathE2EPolarV2::e2ep_run(model = model,
    #                     nyears = 6)
    results <- StrathE2EPolar::e2ep_run(model = model,
                                          nyears = 6)
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
    #setTxtProgressBar(pb,i)
    cat("\rFinished", i, "of", length(transient_years),"\n")
  }
  return(master)
  #saveRDS(master,paste0("../Objects/Shifting_Baseline_Decadal_",hr_scale,"_fishing_",guild_to_crash,".RDS"))
}

baselines <- e2ep_transient_baseline(hr_scale = 0,guilds_to_crash = "Demersal_fish")
#saveRDS(baselines,paste0("../Objects/Shifting_Baseline_Decadal_",hr_scale,"_fishing_",guild_to_crash,".RDS"))
        
# future_map(.x = c(0),.f = e2ep_transient_baseline,c("Demersal_fish"),.progress = T)
toc()
