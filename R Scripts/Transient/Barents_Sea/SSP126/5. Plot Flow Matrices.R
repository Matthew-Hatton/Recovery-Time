## Plot fluxes to bears

rm(list = ls()) #Reset
library(tidyverse)

fluxes_fished <- readRDS("../Objects/Flux_To_Bears_fished.RDS") %>% mutate(Marker = "Fished") # Load fluxes
fluxes_unfished <- readRDS("../Objects/Flux_To_Bears_unfished.RDS") %>% mutate(Marker = "Unfished")
fluxes_no_crash <- readRDS("../Objects/Flux_To_Bears_noCrash.RDS") %>% mutate(Marker = "No Crash")
fluxes <- rbind(fluxes_fished,fluxes_unfished,fluxes_no_crash)




ggplot(data = fluxes,aes(x = Year,y = Flux,color = Marker)) +
  geom_line() +
  #geom_point() +
  labs(y = "Wholedomain Flux to Maritime Mammals (mmn/m2)",
       color = "Guild") +
  facet_wrap(~Species,scales = "free")

ggsave("../Figures/Preliminary/Fluxes_To_Bears.png",
       height = 1080,
       width = 1920,
       units = "px",
       dpi = 200)
