
#### Set up ####

rm(list=ls())                                                               # Wipe the brain

packages <- c("MiMeMo.tools", "furrr")                                      # List packages
lapply(packages, library, character.only = TRUE)                            # Load packages
source("./@_Region_file_BS.R")                                       # Define project region 
sf_use_s2(F)

plan(multisession)                                                          # Choose the method to parallelise by with furrr

all_files <- list.files("./Objects/Shared data/Light and air temp", recursive = TRUE, full.names = TRUE, pattern = ".nc") %>%
  as.data.frame() %>%                                                           # Turn the vector into a dataframe/tibble
  rename(value = 1) %>% 
  separate(value, into = c(NA, "Year", NA), 
           remove = FALSE, sep = "_y") %>%                                  # Extract the year from the file name
  mutate(Year = str_sub(Year, end = -4)) %>%                                # Drop file extension to get number
  separate(value, into = c(NA, NA, NA, "Type", NA, NA), 
         remove = FALSE, sep = "[/_]") %>%                                  # Extract the data type and period from the file name
  rename(File = "value")

examples <- group_by(all_files, Type) %>% slice(1) %>% ungroup()            # Get one example for each file type

domains <- readRDS("./Objects/Domain_BS.rds") %>%                             # Load SF polygons of the MiMeMo model domains
  st_transform(crs = 4326)

Space <- Window(examples[1,]$File, shift = TRUE, sf = st_buffer(domains, 100000)) # Get values to crop a netcdf file spatially at import. Both data types share a grid

sf_use_s2(F)
domains_mask <- expand.grid(Longitude = Space$Lons, Latitude = Space$Lats) %>% # Get the data grid
  mutate(Longitude = ifelse(Longitude > 180, Longitude-360, Longitude)) %>%    # adjust for 0:360 longitudes during voronoi gridding
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = F) %>%    # Convert to SF
  mutate(Longitude = ifelse(Longitude < 0, Longitude+360, Longitude)) %>%      # Undo adjustment for joins in get_air function 
  voronoi_grid(st_make_valid(domains)) %>%                                                    # Weight points within the model domain
  select(-c(Elevation, area, geometry))  
sf_use_s2(T)

ggplot() + geom_sf(data = domains_mask, aes(fill = Cell_area)) 

domains_mask <- st_drop_geometry(domains_mask)

#### Extract Air temperature and light ####

Light_months <- data.frame(Time_step = seq(1,360, 1), Month = rep(1:12, each = 30))     # Add month, 30 days in a model with a 360 day year
Airtemp_months <- data.frame(Time_step = seq(1,1440, 1), Month = rep(1:12, each = 120)) # Add month, to 6 hour time steps for 30 days in a model with a 360 day year

tic()
Air <- future_pmap_dfr(all_files, get_air, .progress = TRUE) %>%                        # Data extraction with parameters stored rowise in dataframe, executed in par
  ungroup() %>%
  mutate(Date = as.Date(paste("15", Month, Year, sep = "/"), format = "%d/%m/%Y"),      # Get date column for plotting
         Measured = ifelse(Type == "T150", Measured - 273.15,                           # Convert Kelvin to celsius for Temp data
                           shortwave_to_einstein(Measured)),                            # Convert Watts to Einsteins for Light data.
         Type = factor(Type, levels = c("SWF", "T150"),                                 # Give units for variables when facetting
                 labels = c(SWF = expression("Light (Em"^{-2}*"d"^{-1}*" )"),
                            T150 = expression("Air temperature ( "*degree*"C )"))),
         Shore = replace_na(Shore, "Combined"))                                         # Only temperature is grouped by shore, replace NA with combined label
toc()
saveRDS(Air, "./Objects/BARENTS_SEA Air temp and light.rds")

#### Plot ####

ggplot(data = Air) +
  geom_line(aes(x = Date, y = Measured, colour = Shore), size = 0.25) +
  theme_minimal() +
  facet_grid(rows = vars(Type), scales = "free_y", labeller = label_parsed) +
  labs(y = NULL, caption = "NEMO-MEDUSA driving data") +
  theme(legend.position = "top") +
  NULL

ggsave("./Figures/saltless/Barents_Sea/Air temp and Light.png", last_plot(), dpi = 500, width = 18, height = 10 , units = "cm")
