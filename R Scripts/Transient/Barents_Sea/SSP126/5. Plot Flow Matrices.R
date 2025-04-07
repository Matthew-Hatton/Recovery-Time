## Plot fluxes to bears

rm(list = ls()) #Reset
library(tidyverse)

fluxes <- readRDS("../Objects/Flux_To_Bears.RDS") # Load fluxes

ggplot(data = fluxes,aes(x = Year,y = Flux,color = Species)) +
  geom_line() +
  geom_point() +
  labs(y = "Wholedomain Flux to Maritime Mammals (mmn/m2)",
       color = "Guild")

ggsave("../Figures/Preliminary/Fluxes_To_Bears.png",
       height = 1080,
       width = 1920,
       units = "px",
       dpi = 200)
