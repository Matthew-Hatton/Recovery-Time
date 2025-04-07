#script to extract ice variables from NEMO-ERSEM
rm(list = ls())

packages <- c("tidyverse", "nemoRsem", "furrr", "ncdf4","tictoc")                                 # List packages
lapply(packages, library, character.only = TRUE)                            # Load packages
source("./@_Region_file_EG.R") 
source("./R Scripts/Functions/Ice Extraction.R")
sf_use_s2(F)

plan(multisession)


domains <- readRDS("./Objects/Domain_EG.rds") %>%                             # Load SF polygons of the MiMeMo model domains
  select(-c(Elevation, area))                                               # Drop unneeded data which would get included in new NM files

crop <- readRDS("./Objects/Domain_EG.rds") %>%  # Load SF polygons of the MiMeMo model domains
  st_transform(crs = 3035) %>% #convert to 3035 for buffer
  st_buffer(dist = 50000) %>%                                               # It needs to be a bit bigger for sampling flows at the domain boundary
  st_transform(crs = 4326) %>% #convert back incase it needs to be in 4326 for some
  summarise() %>%                                                           # Combine polygons to avoid double sampling
  mutate(Shore = "Buffer")

Bathymetry <- readRDS("./Objects/NE_grid.rds") %>%                          # Import NEMO-ERSEM bathymetry
  st_drop_geometry() %>%                                                    # Drop sf geometry column 
  select(-c("x", "y"), latitude = Latitude, longitude = Longitude)          # Clean column so the bathymetry is joined by lat/lon

#load example file
space <- get_spatial_1D("I:/Science/MS-Marine/MA/CNRM_ssp370/ice/CNRM_ssp370_1m_20701201_20701231_icemod_207012-207012.nc",depthdim = "deptht")

grid <- reshape2::melt(space$nc_lon) %>% rename(x = "Var1", 
                                                y = "Var2", longitude = "value") %>%
  cbind(latitude = reshape2::melt(space$nc_lat)$value)

points <- st_as_sf(grid, coords = c("longitude", "latitude"), 
                   crs = 4326, remove = F)

scheme <- st_join(points, crop) %>% st_drop_geometry() %>% 
  drop_na() %>% 
  group_by(x,y) %>% 
  mutate(group = cur_group_id()) %>% 
  ungroup() %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = F) %>% # Convert to sf object
  st_join(st_transform(domains, crs = 4326)) %>%                            # Attach model zone information
  st_drop_geometry() %>% 
  dplyr::select(x,y,longitude,latitude,group,Shore.y) %>% 
  dplyr::rename(Shore = Shore.y)##region is cropped here - only actually need to do this once

start <- scheme_to_start()                                                  # Get netcdf vectors which define the minimum
count <- scheme_to_count()                                                  # amount of data to import

ice_scheme <- scheme_reframe_ice(scheme) %>% 
  arrange(group) %>%
  transmute(n = xyindex_to_nindex(x, y, count[1]))

scheme_result <- arrange(scheme, group) # Create results to bind ice values to later

all_files <- categorise_files("I:/Science/MS-Marine/MA/",
                              recursive = TRUE,ice = T)
tic()
split_files <- all_files %>%
  split(.,f = seq(nrow(.)))

split_files %>% 
  #.[1:12] %>% 
  future_map(.,~{
    # Extracting the path and file from the current split
    path <- unique(.x$Path)  # Assuming Path is the same for each group
    file <- .x$File
    forcing <- .x$Forcing
    SSP <- .x$SSP
    year <- .x$Year
    month <- .x$Month
    # Call your get_icemod function
    get_icemod(path, file, scheme_result, start = start, count = count, ice_scheme = ice_scheme,
               year = year, month = month, forcing = forcing,SSP = SSP,out.dir = "./Objects/NE_Months_Ice/East_Greenland")
  },.progress = T)
toc()