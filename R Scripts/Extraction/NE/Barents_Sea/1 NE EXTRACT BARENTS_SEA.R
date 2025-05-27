
# Pull the contents of netcdf files:
# saveRDS("./Objects/NE_Months/.")  # Marker so network script can see where the data is being saved too, it's buried in a function

#### Set up ####

rm(list=ls())                                                               # Wipe the brain

packages <- c("tidyverse", "nemoRsem", "furrr", "ncdf4","sf")                                 # List packages
lapply(packages, library, character.only = TRUE)                            # Load packages
source("./@_Region_file_BS.R")   # Define project region 
sf_use_s2(F) #switch off spherical geometry

plan(multisession)                                                          # Choose the method to parallelise by with furrr

all_files <- rbind(categorise_files("I:/Science/MS-Marine/MA/CNRM_ssp370", recursive = TRUE),      # For projection runs
                   categorise_files("I:/Science/MS-Marine/MA/CNRM_ssp126", recursive = TRUE),      # From multiple SSPs
                   categorise_files("I:/Science/MS-Marine/MA/GFDL_ssp370", recursive = TRUE),     
                   categorise_files("I:/Science/MS-Marine/MA/GFDL_ssp126", recursive = TRUE),     
                   categorise_files("I:/Science/MS-Marine/MA/CNRM_hist", recursive = TRUE),        # and forcings from historical runs too
                   categorise_files("I:/Science/MS-Marine/MA/GFDL_hist", recursive = TRUE)) %>%   # Build metadata for each file
  drop_na() %>% 
  dplyr::select(-Name) #%>% 
#filter(Year > 2009)

domains <- readRDS("./Objects/Domain_BS.rds") %>%                             # Load SF polygons of the MiMeMo model domains
  dplyr::select(-c(Elevation, area))                                               # Drop unneeded data which would get included in new NM files

crop <- readRDS("./Objects/Domain_BS.rds") %>%  # Load SF polygons of the MiMeMo model domains
  st_transform(crs = 3035) %>% #convert to 3035 for buffer
  st_buffer(dist = 50000) %>%                                               # It needs to be a bit bigger for sampling flows at the domain boundary
  st_transform(crs = 4326) %>% #convert back incase it needs to be in 4326 for some
  summarise() %>%                                                           # Combine polygons to avoid double sampling
  mutate(Shore = "Buffer")

Bathymetry <- readRDS("./Objects/NE_grid.rds") %>%                          # Import NEMO-ERSEM bathymetry
  st_drop_geometry() %>%                                                    # Drop sf geometry column 
  dplyr::select(-c("x", "y"), latitude = Latitude, longitude = Longitude)          # Clean column so the bathymetry is joined by lat/lon

#### Build summary scheme ####

example <- filter(all_files, Type == "N3") %>% 
  slice(1) 

scheme <- scheme_strathE2E(get_spatial(paste0(example$Path, example$File),depthdim = "deptht"),
                           Bathymetry, SDepth, DDepth, crop) %>% 
  dplyr::select(x, y, layer, group, weight, slab_layer, longitude, latitude, Bathymetry) %>%   # Get a scheme to summarise for StrathE2E
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = F) %>% # Convert to sf object
  st_join(st_transform(domains, crs = 4326)) %>%                            # Attach model zone information
  st_drop_geometry()                                                        # Drop sf formatting

start <- scheme_to_start()                                                  # Get netcdf vectors which define the minimum
count <- scheme_to_count()                                                  # amount of data to import
scheme <- scheme_reframe(scheme) 

scheme_result <- arrange(scheme, group) %>%                                 # Create a meta-data object to attach to the summaries
  dplyr::select(x, y, slab_layer, longitude, latitude, Shore, Bathymetry) %>% 
  distinct() %>% 
  mutate(slab_layer = if_else(slab_layer == 1, "S", "D"),
         weights = case_when(slab_layer == "S" & Bathymetry >= SDepth ~ SDepth,     # Weights for zonal averages by thickness of water column
                             slab_layer == "S" & Bathymetry < SDepth ~ Bathymetry,
                             slab_layer == "D" & Bathymetry >= DDepth ~ (DDepth - SDepth),
                             slab_layer == "D" & Bathymetry < DDepth ~ (Bathymetry - SDepth)))

look <- filter(scheme_result, slab_layer == "S") 

ggplot(look) +
  geom_raster(aes(x = x, y = y, fill = Shore))

#### extract ####

skip <- list.files("./Objects/NE_Months/Barents_Sea/")                                      # Already processed files

tictoc::tic()
all_files %>%
  filter(Type == "thetao_con") %>%                                             # The only monthly volumes the compiler actually needs are temperatures. Others are boundary (5-daily) extractions
  #skip?
  #filter(str_detect(paste0("NE.",Forcing,".",SSP,".",Month,".",Year,".rds"), paste(skip, collapse = "|"), negate = TRUE)) %>% # Remove files that already have summaries in the cache
  split(., f = list(paste(.$Month, .$Year, .$Forcing, .$SSP))) %>%                                   # Specify the timestep to average files over.
  #.[1:12] %>%
  future_map(NEMO_ERSEM, analysis = "slabR", summary = scheme_result,
             scheme = scheme, start = start, count = count,
             out_dir = "./Objects/NE_Months/Barents_Sea", 
             collapse_days = TRUE, .progress = T)                # Perform the extraction and save an object for each month (in parallel)
tictoc::toc() # 42 minutes to extract for all files.

#### check ####
# NE.01.2017 <- readRDS("./Objects/NE_Months/Barents_Sea/NE.01.2017.rds")
# 
# ggplot(NE.01.2016) +
#   geom_raster(aes(x = x, y = y, fill = Temperature)) +
#   facet_grid(rows = vars(slab_layer))
