get_spatial_1D <- function (file, depthvar = NULL, depthdim = NULL) 
{
  nc_raw <- ncdf4::nc_open(file)
  nc_lat <- ncdf4::ncvar_get(nc_raw, "nav_lat")
  nc_lon <- ncdf4::ncvar_get(nc_raw, "nav_lon")
  ncdf4::nc_close(nc_raw)
  all <- list(nc_lat = nc_lat, nc_lon = nc_lon)
  return(all)
}

scheme_reframe_ice <- function (slabr_scheme) 
{
  scheme <- mutate(scheme, x = (x + 1) - min(x), y = (y + 
                                                        1) - min(y))
}

categorise_files <- function (dir, recursive = TRUE, full.names = TRUE,ice = FALSE) 
{
  ifelse (ice == F,
      ersem_files <- list.files(dir, recursive = recursive, full.names = full.names, 
    pattern = ".nc") %>% as.data.frame() %>% tidyr::separate(".", 
    into = c("Path", "File"), sep = "\\d{4}/", extra = "merge") %>% 
    tidyr::separate(File, into = c("Forcing", "SSP", NA, 
      "date"), sep = "_", remove = FALSE) %>% dplyr::mutate(Month = stringr::str_sub(date, 
    start = 5, end = 6), Year = stringr::str_sub(date, start = 1, 
    end = 4)) %>% dplyr::mutate(Path = paste0(Path, Year, 
    "/")) %>% dplyr::mutate(String = dplyr::case_when(stringr::str_detect(File, 
    "thetao_con") ~ "thetao_con-Temperature", stringr::str_detect(File, 
    "so_abs") ~ "so_abs-Salinity", stringr::str_detect(File, 
    "uo") ~ "uo-Zonal currents", stringr::str_detect(File, 
    "vo") ~ "vo-Meridional currents", stringr::str_detect(File, 
    "wo") ~ "wo-Vertical velocity", stringr::str_detect(File, 
    "difvho") ~ "difvho-Vertical diffusivity", stringr::str_detect(File, 
    "R1") ~ "R1-Dissolved organic nitrogen", stringr::str_detect(File, 
    "O2") ~ "O2-Oxygen", stringr::str_detect(File, "N4") ~ 
    "N4-Ammonium", stringr::str_detect(File, "N3") ~ "N3-Nitrate", 
    stringr::str_detect(File, "RP") ~ "RP-Detritus (Particulate organic nitrogen)", 
    stringr::str_detect(File, "B1") ~ "B1-Bacterial nitrogen", 
    stringr::str_detect(File, "P1") ~ "P1-Diatom nitrogen", 
    stringr::str_detect(File, "P234_n") ~ "P234_n-Other phytoplankton nitrogen")) %>% 
    tidyr::separate("String", into = c("Type", "Name"), 
      sep = "-") %>% dplyr::select(Path, File, date, Forcing, 
    SSP, Year, Month, Type, Name),#if
    
    ersem_files <- list.files(dir, recursive = recursive, full.names = TRUE, pattern = "\\.nc$") %>%
      data.frame(PathFile = ., stringsAsFactors = FALSE) %>%
      # Filter paths that contain "ice" and exclude "check" and "hindcast"
      filter(grepl("/ice/", PathFile) & !grepl("check|Hindcast", PathFile)) %>%
      # Separate directory path and file name based on the "ice" folder
      separate(PathFile, into = c("Path", "File"), sep = "(?<=/ice/)", extra = "merge")%>% 
    tidyr::separate(File, into = c("Forcing", "SSP", NA, 
      "date",NA,NA,NA), sep = "_", remove = FALSE) %>% dplyr::mutate(Month = stringr::str_sub(date, 
    start = 5, end = 6), Year = stringr::str_sub(date, start = 1, 
    end = 4),Type = "Ice") %>% 
    dplyr::select(Path, File, Forcing, 
      SSP, Year, Month, Type)
    #else do a thing
  )
  return(ersem_files)
}

get_icemod <- function(path, file, scheme_result, start = c(1,1,1,1), count = c(-1,-1,-1,-1), ice_scheme,
                       year,month,forcing,SSP,plt_save = FALSE,out.dir) {
  nc_raw <- ncdf4::nc_open(paste0(path, file))           # Open up a netcdf file to see it's raw contents (var names)
  nc_Ice <- ncdf4::ncvar_get(nc_raw, "siconc", start[-3], count[-3])# Extract a matrix of ice presence
  nc_Ithick <- ncdf4::ncvar_get(nc_raw, "sithic", start[-3], count[-3]) # Extract ice thicknesses
  nc_Sthick <- ncdf4::ncvar_get(nc_raw, "snvolu", start[-3], count[-3]) # Extract snow thicknesses
  nc_simsk <- ncdf4::ncvar_get(nc_raw,"simsk15",start[-3],count[-3])
  ncdf4::nc_close(nc_raw)                                # You must close an open netcdf file when finished to avoid data loss
  
  all <- cbind(                                                     # Bind as a matrix
    Ice_conc = c(nc_Ice[ice_scheme$n]),
    Ice_Thickness = c(nc_Ithick[ice_scheme$n]), 
    Snow_Thickness = c(nc_Sthick[ice_scheme$n]),
    Ice_pres = c(nc_simsk[ice_scheme$n])) %>% 
    as.data.frame() %>% 
    cbind(.,scheme_result)
  if (plt_save == TRUE) {
    # Make the rough plots for export
    ggplot() +
      geom_raster(data = all,aes(x = x,y = y,fill = Ice_Thickness)) +
      scale_fill_gradient(limits = c(0, 2)) +
      labs(title = paste0("Forcing: ",forcing," SSP: ",SSP))
    ggsave(paste0("./Figures/NEMO-ERSEM/ice grids/Ice Thickness/",year,".",month,".png"),
           plot = last_plot()) #will need to make a date
    
    ggplot() +
      geom_raster(data = all,aes(x = x,y = y,fill = Snow_Thickness)) +
      scale_fill_gradient(limits = c(0, 1)) +
      labs(title = paste0("Forcing: ",forcing," SSP: ",SSP))
    ggsave(paste0("./Figures/NEMO-ERSEM/ice grids/Snow Thickness/",year,".",month,".png"),
           plot = last_plot()) #will need to make a date
    
    ggplot() +
      geom_raster(data = all,aes(x = x,y = y,fill = Ice_conc)) +
      scale_fill_gradient(limits = c(0, 1)) +
      labs(title = paste0("Forcing: ",forcing," SSP: ",SSP))
    ggsave(paste0("./Figures/NEMO-ERSEM/ice grids/Ice Concentration/",year,".",month,".png"),
           plot = last_plot()) #will need to make a date
    ggplot() +
      geom_raster(data = all,aes(x = x,y = y,fill = Ice_pres)) +
      scale_fill_gradient(limits = c(0, 1)) +
      labs(title = paste0("Forcing: ",forcing," SSP: ",SSP))
    ggsave(paste0("./Figures/NEMO-ERSEM/ice grids/Ice Presence/",year,".",month,".png"),
           plot = last_plot()) #will need to make a date
  }

  # Put into a saveable format for export
  saveRDS(all,paste0(out.dir,"/NE.ICE.",forcing,".",SSP,".",year,".",month,".rds"))
}
