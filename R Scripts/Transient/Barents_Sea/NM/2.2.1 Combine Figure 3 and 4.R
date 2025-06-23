## Compile Figures
rm(list = ls())
library(patchwork)


DFish_Biomass <- readRDS("../Objects/Figure Compilation/DFish Biomass.RDS")
PFish_Biomass <- readRDS("../Objects/Figure Compilation/PFish Biomass.RDS")
DFish_Recovery <- readRDS("../Objects/Figure Compilation/DFish Recovery.RDS")
PFish_Recovery <- readRDS("../Objects/Figure Compilation/PFish Recovery.RDS")

DFish_Biomass + PFish_Biomass + plot_layout(guides = "collect") & theme(legend.position = "top")
ggsave("../Figures/Transient/Barents_Sea/NM/Draft 1/Figure 3/Figure 3 V3.png",
       dpi = 1200,width = 35,height = 20,unit = "cm",bg = "white") # will need cleaning up for publication

DFish_Recovery + PFish_Recovery + plot_layout(guides = "collect") & theme(legend.position = "top")

ggsave("../Figures/Transient/Barents_Sea/NM/Draft 1/Figure 3/Figure 4 V3.png",
       dpi = 1200,width = 35,height = 20,unit = "cm",bg = "white") # will need cleaning up for publication