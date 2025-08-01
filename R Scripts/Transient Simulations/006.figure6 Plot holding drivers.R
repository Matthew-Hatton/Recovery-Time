rm(list = ls()) # reset

library(MiMeMo.tools) # everything we need

transient_years <- seq(2020,2099)

## Demersal fish
df <- readRDS("../Objects/Experiments/Investigate Climate Effects/hold_climate_drivers_DF.RDS")
baseline_non_ss_df <- readRDS("../Objects/Experiments/Baseline/Baseline_0_fishing_Demersal_fish_1year.RDS")

baseline_non_ss_DF <- data.frame(
  year = transient_years[1:length(baseline_non_ss_df[["Biomasses"]])],
  baseline = map_dbl(baseline_non_ss_df[["Biomasses"]], ~ .x$Model_annual_mean[27])) %>% #extract DF biomass
  mutate(MSC = baseline * 0.8,
         species = "Demersal fish")

master_DF <- data.frame(year = numeric(0),
                     baseline = numeric(0),
                     hold = character(0),
                     species = character(0))

for (i in seq_along(df)) {
  df_df <- data.frame(
    year = transient_years,
    baseline = map_dbl(df[[i]][["Biomasses"]], ~ .x$Model_annual_mean[27])) %>% #extract DF biomass
    mutate(hold = df[[i]]["hold"],
           species = "Demersal fish")
  master_DF <- rbind(master_DF,df_df)
}

## Planktivorous fish
pf <- readRDS("../Objects/Experiments/Investigate Climate Effects/hold_climate_drivers_PF.RDS")

baseline_non_ss_PF <- data.frame(
  year = transient_years[1:length(baseline_non_ss_df[["Biomasses"]])],
  baseline = map_dbl(baseline_non_ss_df[["Biomasses"]], ~ .x$Model_annual_mean[24])) %>% #extract DF biomass
  mutate(MSC = baseline * 0.8,
         species = "Planktivorous fish")

master_PF <- data.frame(year = numeric(0),
                        baseline = numeric(0),
                        hold = character(0),
                        species = character(0))

for (i in seq_along(pf)) {
  pf_df <- data.frame(
    year = transient_years,
    baseline = map_dbl(pf[[i]][["Biomasses"]], ~ .x$Model_annual_mean[24])) %>% #extract DF biomass
    mutate(hold = pf[[i]]["hold"],
           species = "Planktivorous fish")
  master_PF <- rbind(master_PF,pf_df)
}

master <- rbind(master_DF,master_PF)
baseline <- rbind(baseline_non_ss_DF,baseline_non_ss_PF)

# Define custom colors
my_colors <- c(
  "Flows" = "#132280",  # deep purple
  "Boundary" = "#117733",  # green
  "Light" = "#DDCC77",  # sand/yellow
  "Temperature" = "#CC6677",  # reddish pink
  "Ice" = "#88CCEE"   # sky blue
)

ggplot() +
  geom_line(data = master, aes(x = year, y = baseline, color = as.character(hold)),linetype = "dashed",linewidth = 1) +
  geom_line(data = baseline, aes(x = year, y = baseline), linewidth = 1) +
  facet_wrap(~ species) +
  scale_color_manual(values = my_colors) +  # apply the custom palette here
  labs(
    x = "Year",
    y = "Unfished Biomass (N mmol⋅m⁻³)",
    color = "Constant"
  ) +
  theme_bw() +
  theme(
    strip.text = element_text(face = "bold"),
    strip.background = element_blank(),
    legend.position = "top",
    legend.text = element_text(size = 12),
    axis.text.x = element_text(size = 8),
    panel.grid.minor = element_blank()
  ) +
  guides(colour = guide_legend(override.aes = list(size=10))) +
  NULL


ggsave("../Figures/Transient/Barents_Sea/NM/Draft 2/Driver investigation.png",
       height = 1080,
       width = 1920,
       units = "px",
       dpi = 200,
       bg = "white")

ggsave("./Figures/Figure 5.png",
       height = 1080,
       width = 1920,
       units = "px",
       dpi = 200,
       bg = "white")
