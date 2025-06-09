## Script to plot the model domain

#### Setup ####
rm(list=ls()) # reset
Packages <- c("MiMeMo.tools", "exactextractr", "raster", "lubridate",
              "sf","rnaturalearth","tictoc","progressr")    # List packages
lapply(Packages, library, character.only = TRUE)   
source("../@_Region_file_BS.R")

col_grid <- rgb(235, 235, 235, 100, maxColorValue = 255) #faded gridlines

domain <- readRDS("../Objects/Domain_BS.RDS") %>% 
  st_transform(crs = 3035)
domain <- st_union(domain[,1],domain[,2])

basemap <- ne_countries(country = "Russia", scale = "large", returnclass = "sf") %>%
  st_transform(crs = 3035)
adm1 <- ne_states(country = "Norway", returnclass = "sf")

# Check for Svalbard
svalbard <- adm1 %>% filter(name == "Svalbard")

worldmap <- ne_countries(scale = "medium",returnclass = "sf") %>% 
  st_crop(xmin = -180,xmax = 45,ymin = 30,ymax = 90)

world <- ne_countries(scale = "large", returnclass = "sf")

ortho_proj <- "+proj=ortho +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs" #define orthographic proj

world_ortho <- st_transform(world, crs = ortho_proj) #change to orthographic

circle <- st_buffer(st_point(c(0, 0)), dist = 6500000) #create circle (for white background in orthographic earth)
circle_sf <- st_sfc(circle, crs = ortho_proj) #create geom
circle_sf <- st_transform(circle_sf, crs = st_crs(world_ortho)) #transform to crs

ggplot() +
  # geom_sf(data = domain,fill = "aquamarine",alpha = 0.6) +
  geom_sf(data = domain[4,], fill = "#377EB8", alpha = 0.4,color = "black") +  # Blue transparent domain
  geom_sf(data = basemap,fill = "black") +
  geom_sf(data = svalbard,fill = "black") +
  coord_sf(
    xlim = c(4200000, 6500000),  # Easting: ~25°E–35°E
    ylim = c(5000000, 7000000),  # Northing: ~70°N–80°N,  # meters Northing
    expand = FALSE
  ) +
  labs(x = "Longitude",
       y = "Latitude") +
  theme_minimal()

ggsave("../Figures/Transient/Barents_Sea/NM/Draft 1/Figure 1/Figure 1.png",
       dpi = 1200,
       bg = "white") #save out
