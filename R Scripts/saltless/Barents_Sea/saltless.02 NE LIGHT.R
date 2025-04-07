
# Processing irradiance data used to force the West Greenland Implementation NEMO-ERSEM runs

#### Set up ####

rm(list=ls())                                                                   # Wipe the brain

packages <- c("tidyverse", "terra", "exactextractr", "tictoc", "ncdf4", "sf","AlignAssign","purrr")         # List packages
lapply(packages, library, character.only = TRUE)                                # Load packages
sf_use_s2(FALSE) #switch off spherical geometry
plan(multisession)

# A weakness of terra is that you can't use furrr to parallelise operations, something about the C code fights with the threading. 

all_files <- c(list.files("I:/Science/MS-Marine/MA/CNRM_hist/irradiance/", recursive = TRUE, full.names = TRUE, pattern = ".nc"),
              list.files("I:/Science/MS-Marine/MA/GFDL_hist/irradiance/", recursive = TRUE, full.names = TRUE, pattern = ".nc"),
              list.files("I:/Science/MS-Marine/MA/CNRM_ssp126/irradiance/", recursive = TRUE, full.names = TRUE, pattern = ".nc"),
              list.files("I:/Science/MS-Marine/MA/CNRM_ssp370/irradiance/", recursive = TRUE, full.names = TRUE, pattern = ".nc"),
              list.files("I:/Science/MS-Marine/MA/GFDL_ssp126/irradiance/", recursive = TRUE, full.names = TRUE, pattern = ".nc"),
              list.files("I:/Science/MS-Marine/MA/GFDL_ssp370/irradiance/", recursive = TRUE, full.names = TRUE, pattern = ".nc")) %>%
  as.data.frame() %>%                                                           # Turn the vector into a dataframe/tibble
  rename(value = 1) %>% 
  separate(value, into = c(NA, "Year"), 
           remove = FALSE, sep = "_y") %>%                                      # Extract the year from the file name
  rename(File = "value") %>% 
  mutate(Year = str_sub(Year, end = -4),                                        # Drop file extension to get number
         Forcing = case_when(str_detect(File, "CNRM") ~ "CNRM",
                             str_detect(File, "GFDL") ~ "GFDL",
                             T ~ NA),
         SSP = case_when(str_detect(File, "hist") ~ "hist",
                         str_detect(File, "ssp126") ~ "ssp126",
                         str_detect(File, "ssp370") ~ "ssp370",
                         T ~ NA)) %>% 
  filter(Year %in% c(2010:2019))                                                  # Filter down so that we save memory

domains <- readRDS("./Objects/Domains.rds") %>%                                   # Load SF polygons of the model domain
  st_transform(crs = 4326) %>% 
  summarise(area = sum(area))                                                     # Quick and dirty polygon union

#### Extraction ####
                                                                                  # Need to deal with memory management here as we're running out of space. I have decreased number of files down to just what we need
tic()
unlink(tempdir(), recursive = TRUE)                                               # Wipe temporary directory to make space


extract <- pmap(all_files, function(File, Year, Forcing, SSP){

Watts <- rast(File) %>%                                                           # Load the data for a year
  rotate() %>%                                                                    # change 0:360 to -180:180
  exact_extract(domains, "mean", progress = TRUE) %>%                             # Get the average value across the model domain
  as.matrix() %>% 
  as.vector                                                                       # Convert from wide data frame to a vector

if(length(Watts)%%365 == 0) Days <- 365                                           # Catch leap years
if(length(Watts)%%366 == 0) Days <- 366
 
light <- data.frame(Watts = Watts, Day = rep(1:Days, each = 8), Year = Year, SSP = SSP, Forcing = Forcing) %>% # 3 hour time step means 8 steps per day
  group_by(Day, Year, SSP, Forcing) %>% 
    summarise(Watts = mean(Watts, na.rm = TRUE), .groups="drop") %>%            # Average power(Watts) per day 
    mutate(Light = nemoRsem::shortwave_to_einstein(Watts),                      # Convert to Einstens for Strath E2E
           Date = as.Date(Day-1, origin = str_glue("{Year}-01-01")),               
           Month = lubridate::month(Date)) %>% 
  group_by(Month, Year, SSP, Forcing) %>% 
  summarise(Light = mean(Light, na.rm = TRUE),                                  # average per month
            Date = mean(Date), .groups="drop") 

}, .progress = TRUE) %>% 
  data.table::rbindlist()

toc() # ~ 11 hours to run

saveRDS(extract, "./Objects/light.rds")

#### Plot ####

ggplot(data = extract) +
  geom_line(aes(x = Date, y = Light, colour = SSP), linewidth = 0.25) +
  theme_minimal() +
  labs(y = NULL, caption = "NEMO-ERSEM driving data", color = "SSP") +
  theme(legend.position = "top",
        axis.title.x = element_blank()) +
  ylab(expression("Light (Em"^{-2}*"d"^{-1}*" )")) +
  facet_grid(rows = vars(Forcing)) +
  NULL

ggsave("./Figures/saltless/Light.png", last_plot(), dpi = 500, width = 18, height = 18 , units = "cm", bg = "white")
