# Interpolate the exchange at the vertical boundary from NEMO-MEDUSA model output
#### Setup ####

rm(list=ls())                                                               # Wipe the brain

library(MiMeMo.tools)
library(furrr)                                                              # List packages
plan(multisession)

domain <- readRDS("../Objects/Domain_BS.rds") %>%                              # Get the horizontal area to extract over 
  dplyr::select(Shore) %>% 
  filter(Shore =="Offshore")

target_depth <- 40                                                          # Set the depth to interpolate to

example <- list.files("I:\\Science\\MS\\Shared\\CAO\\nemo\\ALLARC", # File to pull dimensions from 
                      recursive = T, full.names = TRUE, pattern = "grid_W")[1]

#### Create summary scheme to interpolate a depth layer over a given area #####

Bathymetry <- readRDS("../Objects/NE_grid.rds") %>% #Import NM bathymetry data
  st_drop_geometry() %>%  #drop sf geometry column
  dplyr::select(-c("x","y"),latitude = Latitude,longitude = Longitude) #clean column s.t bathymetry joined by lat/lon

scheme <- scheme_interp_slice(get_spatial(example, grid_W = T), target_depth, domain) #get a scheme for linear interpolation between 2 depth layers

start <- scheme_to_start() #get ncdf vectors which def the min
count <- scheme_to_count() #amount of data to import

scheme <- scheme_reframe(scheme) %>%                                        # Adjust scheme indices so they match the array subset
  left_join(Bathymetry) %>%                                                 # Attach bathymetry to summary scheme
  filter(depth < Bathymetry & target_depth < Bathymetry) %>%                # Drop points where the target depth or next deeper layer are below the sea floor
  group_by(y, x) %>%                                                        # Redefine the group column as removing points can disrupt this
  mutate(group = cur_group_id()) %>%                                        # Create a new grouping column for the summary scheme
  ungroup()

summary <- filter(scheme, layer == 1) %>% #create metadata to attach to summaries
  arrange(group) %>% #summaries returned in group order, make sure these match
  mutate(depth = target_depth) %>% #return the depth we interpolated to
  dplyr::select(x,y,longitude,latitude,depth)#as well as horizontal info

#### Extract ####

W_files <- list.files("I:\\Science\\MS\\Shared\\CAO\\nemo\\ALLARC",
                      recursive = TRUE, full.names = TRUE) %>%
  as.data.frame() %>%
  mutate(Path  = substr(.,1,42),
         File = substr(.,43,60),  # Extract the file name including "grid_" and the character after the underscore
         Year = substr(., 52, 53),  # Extract the year as a two-digit value from the file name
         date = substr(., 56, 57),  # Extract the date as a two-digit value from the file name
         Month = substr(., 54, 55),
         Type = substr(.,43,49)) %>%  # Extract the month as a two-digit value from the file name
  filter(!File %in% c("ptrc_T_20000625.nc", "ptrc_T_20470130.nc")) %>%      # Drop corrupted files
  filter(Type == "grid_W_") %>%
  dplyr::select(Path,File,date,Year,Month,Type) %>%
  split(.,f = list(.$Month,.$Year)) #get df file names for each time step to summarise to

ice_scheme <- filter(scheme, layer == 1) %>%                                # Ice data is stored as a matrix, so needs a new scheme
  arrange(group) %>% 
  transmute(n = xyindex_to_nindex(x, y, count[1]))

tic()
future_map(W_files, NEMO_MEDUSA, analysis = "slabR",                        # Interpolate grid_W files in parallel
           out_dir = "../Objects/NM vertical boundary BS", scheme_w = scheme,
           start_w = start, count_w = count, summary = summary, .progress = T)
toc()