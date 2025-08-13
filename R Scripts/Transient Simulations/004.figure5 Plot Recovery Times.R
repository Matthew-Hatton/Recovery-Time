## Compile Figures
rm(list = ls())
library(patchwork)

DFish_Biomass <- readRDS("../Objects/Figure Compilation/DFish Biomass.RDS")
PFish_Biomass <- readRDS("../Objects/Figure Compilation/PFish Biomass.RDS")
DFish_Recovery <- readRDS("../Objects/Figure Compilation/DFish Recovery.RDS")
PFish_Recovery <- readRDS("../Objects/Figure Compilation/PFish Recovery.RDS")

DFish_Recovery + PFish_Recovery + plot_layout(guides = "collect",axes = "collect") & theme(legend.position = "top")

ggsave("./Figures/Figure 5.png",
       dpi = 1200,width = 35,height = 20,unit = "cm",bg = "white")
