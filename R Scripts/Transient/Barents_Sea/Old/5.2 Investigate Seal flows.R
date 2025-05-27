## Script to investigate the flows to and from seals across intervals
rm(list = ls())

library(purrr)
library(dplyr)
library(tibble)
library(ggplot2)
library(patchwork)
library(slider)
library(zoo)
library(progressr)

handlers("progress")
handlers(global = TRUE)  # Make sure a handler is set


transient_years <- seq(2020,2099)
interval <- seq(2020,2085,5)


all <- readRDS("../Objects/Experiments/Rolling Crash/Rolling_Crash_and_MSY_Demersal.RDS")
baseline <- readRDS("../Objects/Experiments/Baseline/Baseline_0_fishing_Demersal_fish.RDS")
example <- all[[1]][["Network_Indicators"]][["2020"]][["HR = 1"]][["2021"]] %>% 
  mutate(row_number = seq(1,nrow(.)))

baseline_df <- data.frame(seal = numeric(0),
                          Year = numeric(0),
                          Flow_from = character(0))


with_progress({
  p <- progressor(along = baseline[["Flow_Matrices"]])
  for (i in seq_along(baseline[["Flow_Matrices"]])) {
    # i = 1
    current <- baseline[["Flow_Matrices"]][[i]]
    df <- current %>% 
      subset(select = seal) %>% 
      filter(seal > 0) %>% 
      mutate(Year = transient_years[i]) %>% 
      rownames_to_column("Flow_from")
    baseline_df <- rbind(df, baseline_df)
    p(message = sprintf("Finished interval %d", i))
  }
})



master <- data.frame(Baseline = numeric(0),
                     MSC = numeric(0),
                     Crash_Year = numeric(0),
                     Biomass = numeric(0),
                     year = numeric(0),
                     HR = character(0))


with_progress({
  p <- progressor(along = all)
  for (i in seq_along(all)) {
    current <- all[[i]][["Flows"]][[1]]
    for (k in 1:3) {
      hrs <- current[[k]]
      for (j in seq_along(hrs)) {
        df <- hrs[[j]] %>% 
          subset(select = seal) %>% 
          filter(seal > 0) %>% 
          mutate(Year = names(hrs)[j],
                 Crash_Year = interval[i],
                 HR = case_when(
                   k == 1 ~ "Baseline",
                   k == 2 ~ "MSY",
                   k == 3 ~ "2x MSY"
                 ),
                 HR_Prior = names(current)[k]) %>% 
          rownames_to_column("Flow_from")
        master <- rbind(df, master)
      }
    }
    p(message = sprintf("Finished interval %d", i))  # <-- Move this inside the outer loop
  }
})

# pfish <- master %>% filter(Flow_from == "pfish")
# 
# ggplot() +
#   geom_line(data = pfish,aes(x = as.numeric(Year),y = bird,color = as.character(HR),group = HR)) +
#   facet_wrap(~ Crash_Year,
#              scale = "free_x") +
#   geom_line(data = baseline_df %>% filter(Flow_from == "pfish"),aes(x = Year,y = bird),linetype = "dashed") +
#   scale_x_continuous(limits = c(2020,2099)) +
#   labs(x = "Year",
#        y = "Flow to Birds",
#        title = "Planktivorous Fish - Birds",
#        color = "HR")
# 
# carnzoo <- master %>% filter(Flow_from == "carnzoo")
# 
# ggplot() +
#   geom_line(data = carnzoo,aes(x = as.numeric(Year),y = bird,color = as.character(HR),group = HR)) +
#   facet_wrap(~ Crash_Year,
#              scale = "free_x") +
#   scale_x_continuous(limits = c(2020,2099)) +
#   labs(x = "Year",
#        y = "Flow to Birds",
#        title = "Carn Zoo - Birds",
#        color = "HR")
# 
# DF <- master %>% filter(Flow_from == "dfish")
# 
# ggplot() +
#   geom_line(data = DF,aes(x = as.numeric(Year),y = bird,color = as.character(HR),group = HR)) +
#   facet_wrap(~ Crash_Year,
#              scale = "free_x") +
#   scale_x_continuous(limits = c(2020,2099)) +
#   labs(x = "Year",
#        y = "Flow to Birds",
#        title = "Demersal Fish - Birds",
#        color = "HR")
# 
# 



## ALL FLOWS
`%notin%` <- Negate(`%in%`)

flows <- master %>%
  group_by(Year, Crash_Year, HR) %>%
  filter(Flow_from %notin% c("ocean","discards","corpses")) %>% 
  mutate(Proportion = seal / sum(seal, na.rm = TRUE)) %>%
  ungroup() %>% 
  filter(Crash_Year == 2020) %>% 
  mutate(HR = factor(HR, levels = c("Baseline", "MSY", "2x MSY"))) %>% 
  filter(Flow_from %in% c("pfish"))

baseline_df <- baseline_df %>% 
  group_by(Year) %>%
  filter(Flow_from %notin% c("ocean","discards","corpses")) %>% 
  mutate(Proportion = seal / sum(seal, na.rm = TRUE)) %>% 
  filter(Flow_from %in% c("pfish"))

ggplot() +
  geom_line(data = flows,aes(x = as.numeric(Year),y = Proportion)) +
  geom_line(data = baseline_df,aes(x = Year,y = Proportion),linetype = "dashed") +
  facet_wrap(~ Flow_from + HR,nrow = 2,ncol = 3) +
  labs(title = "Pfish -> Seals") +
  NULL

