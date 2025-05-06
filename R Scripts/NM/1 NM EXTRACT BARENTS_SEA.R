#### Set up ####
rm(list=ls())                                                               # Wipe the brain

packages <- c("MiMeMo.tools", "furrr", "ncdf4")                             # List packages
lapply(packages, library, character.only = TRUE)                            # Load packages
source("./@_Region_file_BS.R")

plan(multisession)

all_files <- list.files("I:\\Science\\MS\\Shared\\CAO\\nemo\\ALLARC",
                        recursive = TRUE, full.names = TRUE) %>%
  as.data.frame() %>%
  mutate(Path  = substr(.,1,42),
         File = substr(.,43,60),  # Extract the file name including "grid_" and the character after the underscore
         Year = substr(., 50, 53),  # Extract the year as a two-digit value from the file name
         date = substr(., 56, 57),  # Extract the date as a two-digit value from the file name
         Month = substr(., 54, 55),
         Type = substr(.,43,49)) %>%  # Extract the month as a two-digit value from the file name
  filter(!File %in% c("ptrc_T_20000625.nc", "ptrc_T_20470130.nc")) %>%      # Drop corrupted files
  filter(Type != "grid_W_") %>%
  filter(Type != "_meter.") %>% 
  filter(Type != "drg.nc") %>%
  filter(Type != "inates.") %>% # Drop the vertical water movement files
  dplyr::select(Path,File,date,Year,Month,Type) #drops unnecessary

all_files$Path <- gsub("/", "\\", all_files$Path, fixed = TRUE)

domains <- readRDS("../Objects/Domain_BS.rds") %>% 
  subset(select = -c(Elevation,area))

ggplot() +
  geom_sf(data = domains,aes(fill = Shore))

sf_use_s2(FALSE) #switch off spherical geometry (makes next bit work)

#takes hot minute to run
crop <- readRDS("../Objects/Domain_BS.rds") %>%
  st_transform(crs = 3035) %>% #loads domain rds, takes every point and draw circle 5000m away
  st_buffer(dist = 50000) %>% #boundary needs to be bigger to sample flows at domain boundary so add a bit on
  summarise() %>%  #combine polgyons to avoid double sampling
  mutate(Shore = "Buffer")

ggplot() +
  geom_sf(data = crop,aes(fill = Shore))

Bathymetry <- readRDS("../Objects/NE_grid.rds") %>%                          # Import NEMO-MEDUSA bathymetry
  st_drop_geometry() %>%                                                    # Drop sf geometry column 
  dplyr::select(-c("x", "y"), latitude = Latitude, longitude = Longitude)          # Clean column so the bathymetry is joined by lat/lon

#### Build summary scheme ####

scheme <- scheme_strathE2E(get_spatial(paste0(all_files$Path[1], all_files$File[1]), grid_W = F),
                           Bathymetry, 40, 600, crop) %>% 
  dplyr::select(x, y, layer, group, weight, slab_layer, longitude, latitude, Bathymetry) %>%   # Get a scheme to summarise for StrathE2E
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = F) %>% # Convert to sf object
  st_join(st_transform(domains, crs = 4326)) %>%                            # Attach model zone information
  st_drop_geometry()                                                        # Drop sf formatting

start <- scheme_to_start()                                                  # Get netcdf vectors which define the minimum
count <- scheme_to_count()                                                  # amount of data to import
scheme <- scheme_reframe(scheme)

ice_scheme <- filter(scheme, layer == 1) %>%                                # Ice data is stored as a matrix, so needs a new scheme
  arrange(group) %>% 
  transmute(n = xyindex_to_nindex(x, y, count[1]))

scheme_result <- arrange(scheme, group) %>%                                 # Create a meta-data object to attach to the summaries
  subset(select = c(x, y, slab_layer, longitude, latitude, Shore, Bathymetry)) %>% 
  distinct() %>% 
  mutate(slab_layer = if_else(slab_layer == 1, "S", "D"),
         weights = case_when(slab_layer == "S" & Bathymetry >= 40 ~ 40,     # Weights for zonal averages by thickness of water column
                             slab_layer == "S" & Bathymetry < 40 ~ Bathymetry,
                             slab_layer == "D" & Bathymetry >= 600 ~ 560,
                             slab_layer == "D" & Bathymetry < 600 ~ (Bathymetry - 40)))

tictoc::tic()
all_files %>% 
  split(., f = list(.$Month, .$Year)) %>%                                   # Specify the timestep to average files over.
  #.[1:12] %>% 
  future_map(NEMO_MEDUSA, analysis = "slabR", summary = scheme_result,
             scheme = scheme, ice_scheme = ice_scheme$n, start = start,  
             count = count, out_dir = "../Objects/NM_Months", .progress = T)    # Perform the extraction and save an object for each month (in parallel)

tictoc::toc()