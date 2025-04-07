
# Extract monthly significant wave height

#### Setup ####

rm(list=ls())

Packages <- c("MiMeMo.tools", "exactextractr", "raster", "lubridate")       # List packages
lapply(Packages, library, character.only = TRUE)                            # Load packages
source("./@_Region_file_BS.R")

domains <- readRDS("./Objects/Domain_BS.rds") %>%                             # Import inshore polygon
  st_transform(crs = 4326)

Reduced <- brick("./Objects/Shared data/ISIMIP Atmosphere/ndep-nhx_histsoc_monthly_1901_2021.nc") # import reduced nitrogen deposition
Oxidised <- brick("./Objects/Shared data/ISIMIP Atmosphere/ndep-noy_histsoc_monthly_1901_2021.nc")# import oxidised nitrogen deposition

ReducedSSP370<-brick("./Objects/Shared data/ISIMIP Atmosphere/ndep-nhx_ssp370soc_monthly_2015_2100.nc")
OxidisedSSP370<-brick("./Objects/Shared data/ISIMIP Atmosphere/ndep-noy_ssp370soc_monthly_2015_2100.nc")

ReducedSSP126<-brick("./Objects/Shared data/ISIMIP Atmosphere/ndep-nhx_ssp126soc_monthly_2015_2100.nc")
OxidisedSSP126<-brick("./Objects/Shared data/ISIMIP Atmosphere/ndep-noy_ssp126soc_monthly_2015_2100.nc")
  
#### Calculate mean disturbance per habitat area, weighting by cell coverage ####

Deposition_hist <- map(list(Reduced, Oxidised), ~{
  
exact_extract(.x, domains, "mean") %>%                                      # Extract deposition    
  mutate(Shore = domains$Shore) %>%                                         # Add metadata
  pivot_longer(-Shore, names_to = "Date", values_to = "Measured")           # Move time steps into a single column
  
}) %>%
  map2_dfr(c("R", "O"), ~{ mutate(.x, Oxidation_state = .y)}) %>%               # Add label for each variable
  mutate(Date = str_remove(Date, "mean.X")) %>%                                 # Fix date string
  mutate(Date = ymd("1850-01-01") %m+% months(as.numeric(Date))) %>%            # Format as date
  mutate(Month = month(Date), 
         Year = year(Date),
         Measured = full_to_milli((Measured/14)/days_in_month(Date)),           # Convert to daily rate, and from grams to mmol
         SSP = "hist") %>% 
  filter(Year<2015)                                                             # Trim to seam with SSPs

Deposition_ssp<- map(list(ReducedSSP370, ReducedSSP126, OxidisedSSP370, OxidisedSSP126), ~{
  
  exact_extract(.x, domains, "mean") %>%                                      # Extract deposition    
    mutate(Shore = domains$Shore) %>%                                         # Add metadata
    pivot_longer(-Shore, names_to = "Date", values_to = "Measured")           # Move time steps into a single column
  
}) %>%
  map2(c("R", "R", "O", "O"), ~{ mutate(.x, Oxidation_state = .y)}) %>%         # Add label for each variable
  map2_dfr(c("ssp370", "ssp126", "ssp370", "ssp126"), ~{ mutate(.x, SSP = .y)}) %>%   # Add label for each SSP
  mutate(Date = str_remove(Date, "mean.X")) %>%                               # Fix date string
  mutate(Date = ymd("2015-01-01") %m+% months(as.numeric(Date)-4248)) %>%     # Format as date
  mutate(Month = month(Date), 
         Year = year(Date),
         Measured = full_to_milli((Measured/14)/days_in_month(Date))) %>%       # Convert to daily rate, and from grams to mmol
  rbind(Deposition_hist)

#### Plot ####

Deposition_lab <- mutate(Deposition_ssp, Oxidation_state = factor(Oxidation_state, levels = c("O", "R"),
                                                          labels = c(expression("Oxidised Nitrogen (NO"["y"]*")"), expression("Reduced Nitrogen (NH"["x"]*")"))))



ggplot(data = filter(Deposition_lab, Year > 2000)) + 
  geom_line(aes(x = Date, y = Measured, colour = Shore), size = 0.15) +
  theme_minimal() +
  facet_grid(rows = vars(Oxidation_state), cols = vars(SSP), scales = "free_y", labeller = label_parsed) +
  labs(y = expression("mmols N m"^{-2}*"Day"^{-1}), caption = "ISIMIP Atmospheric Nitrogen deposition") +
  theme(legend.position = "top",
        strip.text = element_text(size = 6),
        axis.text.x = element_text(size = 6,angle = 45),
        axis.title.x = element_blank()) +
  NULL

ggsave("./Figures/saltless/Barents_Sea/Atmospheric N Deposition.png", last_plot(), dpi = 500, width = 18, height = 10 , units = "cm", bg = "white")

#### Save ####
Deposition_ssp %>% 
  dplyr::select(Month, Oxidation_state, Shore,  Year, SSP, Measured) %>%  
  saveRDS("./Objects/BARENTS_SEA Atmospheric N deposition.rds")

#### Split ####
#we need varying SSP's/Forcings saved 
