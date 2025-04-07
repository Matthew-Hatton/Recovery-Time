rm(list = ls()) #reset

#average files based on zone and forcing/ssp data
packages <- c("tidyverse", "nemoRsem", "furrr", "ncdf4","tictoc")                                 # List packages
lapply(packages, library, character.only = TRUE)   
plan(multisession)

#load in a temporary file
temp <- readRDS("./Objects/NE_Months_Ice/Barents_Sea/NE.ICE.CNRM.hist.1976.01.rds") %>% 
  group_by(Shore) %>% 
  summarise(Ice_presence = mean(Ice_pres,na.rm = T),
            Ice_Thickness = mean(Ice_Thickness,na.rm = T),
            Snow_Thickness = mean(Snow_Thickness,na.rm = T),
            Ice_concentration = mean(Ice_conc))
#will also need a line of code which adds the ssp and forcing from the name + month and year for the time series

all_files <- list.files("./Objects/NE_Months_Ice/Barents_Sea/", pattern = ".rds", full.names = TRUE) %>%
  as.data.frame(stringsAsFactors = FALSE) %>%
  setNames("FullPath") %>%
  mutate(
    Path = paste0(dirname(FullPath), "/"),
    File = basename(FullPath)
  ) %>%
  separate(File, into = c("Region", "Variable", "Model", "Scenario", "Year", "Month", "Ext"), sep = "\\.", remove = FALSE) %>%
  select(Path, File, Model, Scenario, Year, Month) %>%
  mutate(
    Year = as.integer(Year),
    Month = as.integer(Month)
  )

# Read each file in parallel and summarize by Year, Shore, Month, Model, and Scenario
summary_data <- all_files %>%
  mutate(data = future_map(paste0(Path,File), ~ readRDS(.x), .progress = TRUE)) %>%
  unnest(data) %>%
  group_by(Year, Shore, Month, Model, Scenario) %>%
  summarize(across(everything(), mean, na.rm = TRUE))

saveRDS(summary_data,"./Objects/Ice_Summary.rds")

# time_series_dat <- summary_data %>% 
#   select(Year,Shore,Month,Model,Scenario,Ice_pres,Ice_Thickness,Snow_Thickness,Ice_conc) %>% 
#   drop_na(Shore) %>% 
#   mutate(Date = make_date(Year, Month, 1))
# 
# ggplot(data = time_series_dat,aes(x = Date,y = Ice_pres,color = Scenario,linetype = Model)) +
#   geom_line() +
#   facet_wrap(~Shore,ncol = 1) +
#   labs(color = "SSP",
#        linetype = "Forcing",
#        y = "Ice Presence") +
#   theme(axis.title.x = element_blank()) +
#   theme_bw()
# ggsave("./Figures/NEMO-ERSEM/Ice/ICE_PRESENCE.Inshore Offshore Split.tiff",
#        height = 1080,width = 2560,unit = "px")
# 
# #let's zoom in to see what's happening
# ts_zoom <- time_series_dat %>% 
#   filter(Year %in% c(2010:2030))
# 
# ggplot(data = ts_zoom,aes(x = Date,y = Ice_pres,color = Scenario,linetype = Model)) +
#   geom_line() +
#   facet_wrap(~Shore,ncol = 1) +
#   labs(color = "SSP",
#        linetype = "Forcing",
#        y = "Ice Presence") +
#   theme(axis.title.x = element_blank()) +
#   theme_bw()
# ggsave("./Figures/NEMO-ERSEM/Ice/ICE_PRESENCE.Inshore Offshore Split zoom.tiff",
#        height = 1080,width = 2560,unit = "px")
# 
# ggplot(data = time_series_dat,aes(x = Date,y = Ice_Thickness,color = Scenario,linetype = Model)) +
#   geom_line() +
#   facet_wrap(~Shore,ncol = 1) +
#   labs(color = "SSP",
#        linetype = "Forcing",
#        y = "Ice Thickness") +
#   theme(axis.title.x = element_blank()) +
#   theme_bw()
# ggsave("./Figures/NEMO-ERSEM/Ice/ICE_THICKNESS.Inshore Offshore Split.tiff",
#        height = 1080,width = 2560,unit = "px")
# 
# #let's zoom in to see what's happening
# ts_zoom <- time_series_dat %>% 
#   filter(Year %in% c(2010:2030))
# 
# ggplot(data = ts_zoom,aes(x = Date,y = Ice_Thickness,color = Scenario,linetype = Model)) +
#   geom_line() +
#   facet_wrap(~Shore,ncol = 1) +
#   labs(color = "SSP",
#        linetype = "Forcing",
#        y = "Ice Thickness") +
#   theme(axis.title.x = element_blank()) +
#   theme_bw()
# ggsave("./Figures/NEMO-ERSEM/Ice/ICE_THICKNESS.Inshore Offshore Split zoom.tiff",
#        height = 1080,width = 2560,unit = "px")
# 
# ggplot(data = time_series_dat,aes(x = Date,y = Ice_Thickness,color = Scenario,linetype = Model)) +
#   geom_line() +
#   facet_wrap(~Shore,ncol = 1) +
#   labs(color = "SSP",
#        linetype = "Forcing",
#        y = "Snow Thickness") +
#   theme(axis.title.x = element_blank()) +
#   theme_bw()
# ggsave("./Figures/NEMO-ERSEM/Ice/SNOW_THICKNESS.Inshore Offshore Split.tiff",
#        height = 1080,width = 2560,unit = "px")
# 
# #let's zoom in to see what's happening
# ts_zoom <- time_series_dat %>% 
#   filter(Year %in% c(2010:2030))
# 
# ggplot(data = ts_zoom,aes(x = Date,y = Snow_Thickness,color = Scenario,linetype = Model)) +
#   geom_line() +
#   facet_wrap(~Shore,ncol = 1) +
#   labs(color = "SSP",
#        linetype = "Forcing",
#        y = "Snow Thickness") +
#   theme(axis.title.x = element_blank()) +
#   theme_bw()
# ggsave("./Figures/NEMO-ERSEM/Ice/SNOW_THICKNESS.Inshore Offshore Split zoom.tiff",
#        height = 1080,width = 2560,unit = "px")
# 
# ggplot(data = time_series_dat,aes(x = Date,y = Ice_conc,color = Scenario,linetype = Model)) +
#   geom_line() +
#   facet_wrap(~Shore,ncol = 1) +
#   labs(color = "SSP",
#        linetype = "Forcing",
#        y = "Ice Concentration") +
#   theme(axis.title.x = element_blank()) +
#   theme_bw()
# ggsave("./Figures/NEMO-ERSEM/Ice/ICE_PRESENCE.Inshore Offshore Split.tiff",
#        height = 1080,width = 2560,unit = "px")