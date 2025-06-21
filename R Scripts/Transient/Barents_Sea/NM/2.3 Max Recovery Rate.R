rm(list = ls()) #reset

# load packages
library(purrr)
library(dplyr)
library(tibble)
library(ggplot2)
library(patchwork)
library(slider)
library(zoo)
library(progressr)
library(ggrepel)

progressr::handlers("cli") # progress bars are nice


## WHOLE ECOSYSTEM - PFISH
pfish_all_upper <- readRDS("../Objects/Experiments/Maximum recovery time/PF_Recovery_All_Upper.rds")

# summarise this to take the maximum values each release year
pfish_summary <- pfish_all_upper %>%
  group_by(HR, Crash_Year) %>%
  slice_max(Recovery_Time, with_ties = FALSE) %>%
  ungroup() %>% 
  mutate(Guild_Crash = "Planktivorous Fish",
         Level = "Whole Ecosystem")

## FOCAL - PFISH
pfish_focal <- readRDS("../Objects/Experiments/Maximum recovery time/PF_Recovery_focal.rds") %>% 
  mutate(Guild_Crash = "Planktivorous Fish",
         Level = "Focal",
         Guild = "Planktivorous Fish")

## WHOLE ECOSYSTEM - DFISH
dfish_all_upper <- readRDS("../Objects/Experiments/Maximum recovery time/DF_Recovery_All_Upper.rds")

# summarise this to take the maximum values each release year
dfish_summary <- dfish_all_upper %>%
  group_by(HR, Crash_Year) %>%
  slice_max(Recovery_Time, with_ties = FALSE) %>%
  ungroup() %>% 
  mutate(Guild_Crash = "Demersal Fish",
         Level = "Whole Ecosystem")

## FOCAL - FISH
dfish_focal <- readRDS("../Objects/Experiments/Maximum recovery time/DF_Recovery_focal.rds") %>% 
  mutate(Guild_Crash = "Demersal Fish",
         Level = "Focal",
         Guild = "Demersal Fish")



facet <- rbind(pfish_summary,pfish_focal,dfish_summary,dfish_focal) %>% 
  mutate(across(everything(), ~ replace(.x, is.na(.x), 80)))

color_scale <- scale_color_manual(
  values = c("2020s Baseline" = "#1b9e77", "2020s MSY" = "#7570b3", "2020s 2x MSY" = "#d95f02"),
  name = "Harvest Rate"
)

ggplot(facet, aes(x = Crash_Year, y = Recovery_Time, color = HR, shape = Level)) +
  geom_point(size = 4) +
  geom_line(aes(group = interaction(Crash_Year, HR)), linewidth = 1.5, alpha = 0.3) +
  geom_text_repel(
    data = facet %>% 
      filter(Level == "Whole Ecosystem", Recovery_Time != 0),
    aes(label = Guild),
    size = 2,
    max.overlaps = Inf,
    show.legend = FALSE,
    box.padding = 0.3,
    point.padding = 0.2,
    force = 1,
    force_pull = 1,
    segment.alpha = 1,
    segment.color = "black"
  ) +
  geom_hline(yintercept = 20, linetype = "dashed") +
  scale_shape_manual(values = c("Focal" = 16, "Whole Ecosystem" = 1)) +
  scale_y_continuous(
    breaks = c(0, 20, 40, 60, 80),
    labels = c("0", "20", "40", "60", "NR")
  ) +
  facet_grid(Guild_Crash ~ HR) +
  color_scale +
  guides(color = "none") +
  theme_bw(base_size = 14) +
  labs(x = "Release Year", y = "Maximum Recovery Time") +
  theme(
    strip.background = element_rect(color = "black", fill = NA),
    strip.text = element_text(face = "bold"),
    legend.position = "top"
  )
ggsave("../Figures/Transient/Barents_Sea/NM/Draft 1/MAX RECOVERY TIME.png",
       dpi = 1200,width = 35,height = 20,unit = "cm",bg = "white") # will need cleaning up for publication
 
## and if you're happy
ggsave("./Figures/MAX RECOVERY TIME.png",
       dpi = 1200,width = 35,height = 20,unit = "cm",bg = "white")



