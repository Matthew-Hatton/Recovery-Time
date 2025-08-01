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




ggplot() +
  geom_line(data = master,aes(x = year,y = baseline,color = as.character(hold))) +
  geom_line(data = baseline,aes(x = year,y = baseline),linetype = "dashed") +
  facet_wrap(~ species) +
  labs(x = "Year",
       y = "Biomass",
       color = "Hold Constant")

ggsave("../Figures/Transient/Barents_Sea/NM/Draft 2/Driver investigation.png",
       height = 1080,
       width = 1920,
       units = "px",
       dpi = 200,
       bg = "white")
