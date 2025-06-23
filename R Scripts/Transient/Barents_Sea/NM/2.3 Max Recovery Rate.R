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
         Level = "Ecosystem Maximum")

## FOCAL - PFISH
pfish_focal <- readRDS("../Objects/Experiments/Maximum recovery time/PF_Recovery_focal.rds") %>% 
  mutate(Guild_Crash = "Planktivorous Fish",
         Level = "Targetted Guild",
         Guild = "Planktivorous Fish")

## WHOLE ECOSYSTEM - DFISH
dfish_all_upper <- readRDS("../Objects/Experiments/Maximum recovery time/DF_Recovery_All_Upper.rds")

# summarise this to take the maximum values each release year
dfish_summary <- dfish_all_upper %>%
  group_by(HR, Crash_Year) %>%
  slice_max(Recovery_Time, with_ties = FALSE) %>%
  ungroup() %>% 
  mutate(Guild_Crash = "Demersal Fish",
         Level = "Ecosystem Maximum")

## FOCAL - FISH
dfish_focal <- readRDS("../Objects/Experiments/Maximum recovery time/DF_Recovery_focal.rds") %>% 
  mutate(Guild_Crash = "Demersal Fish",
         Level = "Targetted Guild",
         Guild = "Demersal Fish")



facet <- rbind(pfish_summary, pfish_focal, dfish_summary, dfish_focal) %>%
  mutate(across(where(is.numeric), ~ replace(.x, is.na(.x), 70))) %>% 
  mutate(NR_flag = ifelse(Recovery_Time == 70, TRUE, FALSE)
    )

color_scale <- scale_color_manual(
  values = c("Status Quo" = "#1b9e77", "MSY" = "#7570b3", "Overfishing" = "#d95f02"),
  name = "Harvest Rate"
)

# Group for line pairs (Focal vs Whole Ecosystem)
facet <- rbind(pfish_summary, pfish_focal, dfish_summary, dfish_focal) %>%
  mutate(across(everything(), ~ replace(.x, is.na(.x), 70))) %>%
  mutate(
    NR_flag = Recovery_Time == 70,
    pair_id = paste(Guild_Crash, HR, Crash_Year, sep = "_")
  ) %>%
  group_by(pair_id) %>%
  mutate(pair_NR = any(Recovery_Time == 70),  # Identify NR points
         HR = case_when(
           as.character(HR) == "2020s Baseline" ~ "Status Quo",
           as.character(HR) == "2020s MSY" ~ "MSY",
           as.character(HR) == "2020s 2x MSY" ~ "Overfishing",
           TRUE ~ as.character(HR)  # To keep unmatched values
         )) %>%
  ungroup()

# Annotations
guild_labels <- facet %>%
  filter(Level == "Ecosystem Maximum",Recovery_Time > 0) %>%
  group_by(Guild, Crash_Year, HR, Guild_Crash) %>%
  summarise(y = -15, .groups = "drop")  # y-position for label

median_points <- bind_rows(
  pfish_all_upper %>% mutate(Guild_Crash = "Planktivorous Fish"),
  dfish_all_upper %>% mutate(Guild_Crash = "Demersal Fish")
) %>%
  mutate(HR = case_when(
           as.character(HR) == "2020s Baseline" ~ "Status Quo",
           as.character(HR) == "2020s MSY" ~ "MSY",
           as.character(HR) == "2020s 2x MSY" ~ "Overfishing",
           TRUE ~ as.character(HR)  # To keep unmatched values
         )) %>% 
  group_by(Guild_Crash, HR, Crash_Year) %>%
  filter(Recovery_Time > 0) %>% #exlcude recovery of 0
  summarise(
    Median_Recovery = median(Recovery_Time, na.rm = TRUE),
    .groups = "drop"
  )

median_points$HR <- factor(median_points$HR, levels=c('Status Quo', 'MSY', 'Overfishing')) # reorder legend
facet$HR <- factor(facet$HR, levels=c('Status Quo', 'MSY', 'Overfishing')) # reorder legend

ggplot(facet, aes(x = Crash_Year, y = Recovery_Time)) +
  
  # # Colored lines for non-NR pairs
  geom_line(
    data = facet %>% filter(!pair_NR),
    aes(group = pair_id, color = HR),
    linewidth = 1.5, alpha = 0.3
  ) +

  # Grey lines for NR pairs
  geom_line(
    data = facet %>% filter(pair_NR),
    aes(group = pair_id),
    color = "grey30", linewidth = 1.5, alpha = 0.5,
    show.legend = FALSE
  ) +
  
  # Colored points for non-NR pairs
  geom_point(
    data = facet %>% filter(!pair_NR),
    aes(color = HR, shape = Level),
    size = 4
  ) +
  
  # Grey open circles for Whole Ecosystem in NR pairs (even if Recovery_Time != 80)
  geom_point(
    data = facet %>% filter(pair_NR, Level == "Ecosystem Maximum", !NR_flag),
    shape = 1, color = "grey30", size = 4, stroke = 1,alpha = 0.5,
    show.legend = FALSE
  ) +
  
  # Grey filled circles for Focal in NR pairs (and not 80)
  geom_point(
    data = facet %>% filter(pair_NR, Level == "Targetted Guild", !NR_flag),
    shape = 16, color = "grey30", size = 4,alpha = 0.5,
    show.legend = FALSE
  ) +
  
  # Grey crosses for all NR points
  geom_point(
    data = facet %>% filter(NR_flag),
    shape = 4, color = "grey30", size = 4, stroke = 1,alpha = 0.8,
    show.legend = FALSE
  ) +
  
  # geom_text_repel(
  #   data = facet %>%
  #     filter(Level == "Whole Ecosystem", Recovery_Time != 0),
  #   aes(label = Guild),
  #   size = 2,
  #   max.overlaps = Inf,
  #   show.legend = FALSE,
  #   box.padding = 0.3,
  #   point.padding = 0.2,
  #   force = 1,
  #   force_pull = 1,
  #   segment.alpha = 1,
  #   segment.color = "black"
  # ) +
#median notch
geom_segment(
  data = median_points,
  aes(
    x = Crash_Year - 1,       # left end of horizontal line (adjust width here)
    xend = Crash_Year + 1,    # right end of horizontal line
    y = Median_Recovery,      # same y position for horizontal line
    yend = Median_Recovery
  ),
  color = "black",
  linewidth = 0.5,
  inherit.aes = FALSE
) +
# Add labels
  # geom_text(
  #   data = guild_labels,
  #   aes(x = Crash_Year, y = y, label = Guild),
  #   angle = 90,
  #   size = 2,
  #   vjust = 0.5,
  #   color = "black",
  #   inherit.aes = FALSE
  # ) +
  # Add label centered below the bracket
  geom_hline(yintercept = 20, linetype = "dashed") +
  scale_shape_manual(values = c("Targetted Guild" = 16, "Ecosystem Maximum" = 1)) +
  scale_x_continuous(limits = c(2020,2085),
                     breaks = c(2020,2040,2060,2080)) +
  scale_y_continuous(
    limits = c(-10, 70),
    breaks = c(0, 20, 40, 60, 70),
    labels = c("0", "20", "40", "60", "NR")
  ) +
  facet_grid(Guild_Crash ~ HR) +
  color_scale +
  guides(color = "none") +
  theme_bw(base_size = 14) +
  labs(x = "Release Year", y = "Recovery Time (Years)") +
  theme(
    strip.background = element_rect(color = "black", fill = NA),
    strip.text = element_text(face = "bold"),
    legend.position = "top",
    legend.title = element_blank(),
    panel.grid.minor = element_blank()
  )




ggsave("../Figures/Transient/Barents_Sea/NM/Draft 1/Figure 6.png",
       dpi = 1200,width = 35,height = 20,unit = "cm",bg = "white") # will need cleaning up for publication
 
## and if you're happy
ggsave("./Figures/Figure 6.png",
       dpi = 1200,width = 35,height = 20,unit = "cm",bg = "white")



