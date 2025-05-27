## why are pfish dying?

#### Setup ####
rm(list=ls()) # reset
Packages <- c("MiMeMo.tools", "exactextractr", "raster", "lubridate",
              "StrathE2EPolar","furrr","tictoc","progressr")    # List packages
lapply(Packages, library, character.only = TRUE)   
source("../@_Region_file_BS.R")
handlers(global = T)
handlers("cli") # progress bar

plan(multisession,workers = availableCores()-1) # parallel processing is good, but not that good

tic() # time


focal = 17
interval <- seq(2020,2085,5)
transient_years <- seq(2020,2099) # How far do we want to compute?
all <- readRDS("../Objects/Experiments/Rolling Crash/Rolling_Crash_and_MSY_Demersal.RDS")
baseline <- readRDS("../Objects/Experiments/Baseline/Baseline_0_fishing_Demersal_fish.RDS")
baseline_df <- data.frame(
  year = transient_years[1:length(baseline[["Biomasses"]])],
  baseline = map_dbl(baseline[["Biomasses"]], ~ .x$Model_annual_mean[focal])) %>% #extract DF biomass
  mutate(MSC = baseline * 0.8)

master <- data.frame(Baseline = numeric(0),
                     MSC = numeric(0),
                     Crash_Year = numeric(0),
                     Biomass = numeric(0),
                     year = numeric(0),
                     HR = character(0))

for (i in 1:length(all)) {
  ## DEBUG
  # i = 1
  current <- all[[i]][["Biomasses"]][[1]]
  
  for (k in 1:3) {
    ## DEBUG
    # k=1
    hrs <- current[[k]]
    df <- data.frame(
      year = (interval[i] + 1):max(transient_years),
      Biomass = map_dbl(hrs, ~ .x$Model_annual_mean[focal])) %>% #extract DF biomass
      mutate(Crash_Year = interval[i],
             HR = case_when(
               k == 1 ~ "Baseline",
               k == 2 ~ "MSY",
               k == 3 ~ "2x MSY"
             ))
    base <- baseline_df %>% filter(year > interval[i])
    df <- df %>% mutate(baseline = base$baseline,
                        MSC = base$baseline * 0.8)
    master <- rbind(df,master)
  }
}
library(patchwork)
p1 <- ggplot(data = filter(master,Crash_Year == 2020)) +
  geom_line(
    data = baseline_df,
    aes(x = year, y = baseline), linetype = "solid", inherit.aes = FALSE,alpha = 1
  ) +
  # # geom_smooth(se = FALSE) +
  # geom_ribbon(
  #   data = baseline_df,
  #   aes(x = year, y = baseline,ymin = baseline - (baseline * 0.05),ymax = baseline + (baseline * 0.05)),alpha = 0.1) +
  facet_wrap(~ Crash_Year, ncol = 3, scales = "free_x",strip.position = "top") +
  labs(
    # title = "Demersal Fish Biomass Post-Crash by Crash Year and Harvest Rate",
    x = "Year", y = "OmnivZoo Biomass (mmN/m2)", color = "Harvest Rate"
  ) +
  scale_x_continuous(limits = c(2020,2099)) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"),
        legend.position = "top",
        legend.text = element_text(size = 12)) +
  NULL

## Check TS
My_volumes <- readRDS("../Objects/Barents_Sea/NM/TS.rds") %>%
  group_by(Compartment, Month) %>%                                          # By compartment and month
  #summarise(across(c(DIN_avg,Phytoplankton_avg,Detritus_avg,Temperature_avg), mean, na.rm = T)) %>%         # Average across years for multiple columns
  ungroup() %>%
  arrange(Month)

ggplot() +
  geom_line(data = My_volumes,aes(x = date,Phytoplankton_avg)) +
  facet_wrap(~ Compartment)


## Check flow from omniv zoo
handlers("cli")
`%notin%` <- Negate(`%in%`)

baseline_df <- data.frame(Year = numeric(0),
                          Flow_from = numeric(0),
                          Flow_to = character(0),
                          value = numeric(0))

with_progress({
  p <- progressor(along = baseline[["Flow_Matrices"]])
  for (i in seq_along(baseline[["Flow_Matrices"]])) {
    # i = 1
    current <- baseline[["Flow_Matrices"]][[i]]
    df <- current %>%
      as.data.frame() %>%
      rownames_to_column("Flow_from") %>%
      filter(Flow_from == "omnivzoo") %>%
      pivot_longer(cols = -Flow_from, names_to = "Flow_to", values_to = "value") %>%
      filter(value > 0) %>%
      mutate(Year = transient_years[i])
    
    
    baseline_df <- rbind(df, baseline_df)
    p(message = sprintf("Finished interval %d", i))
  }
})

p2 <- ggplot() +
  geom_line(data = baseline_df,aes(x = Year,y = value)) +
  facet_wrap(~ Flow_to,scales = "free_y") +
  labs(y = "Flow from Omnivorous Zooplankton")


p1 + p2
ggsave("../Figures/Transient/Barents_Sea/NM/Draft 1/omniv zooplankton biomass and flows.png",
       dpi = 1200,width = 35,unit = "cm",bg = "white")
# 
# baseline_df <- baseline_df %>% filter(Flow_to %in% c("bird","ceta","seal"))
# 
# ggplot() +
#   geom_line(data = baseline_df,aes(x = Year,y = value)) +
#   facet_wrap(~ Flow_to) +
#   labs(y = "Flow from Pfish",title = "As climate change occurs, pfish biomass drops to 0, so flows drop to 0")
# 
# ## "As climate change occurs, pfish biomass drops to 0, so flows drop to 0"
# 
# ## How is this affected when we fish?
# master <- data.frame(Baseline = numeric(0),
#                      MSC = numeric(0),
#                      Crash_Year = numeric(0),
#                      Biomass = numeric(0),
#                      year = numeric(0),
#                      HR = character(0))
# 
# 
# with_progress({
#   p <- progressor(along = all)
#   for (i in seq_along(all)) {
#     i = 1
#     current <- all[[i]][["Flows"]][[1]]
#     for (k in 1:3) {
#       k = 1
#       hrs <- current[[k]]
#       for (j in seq_along(hrs)) {
#         j = 1
#         df <- hrs[[j]] %>%
#           as.data.frame() %>%
#           rownames_to_column("Flow_from") %>%
#           filter(Flow_from == "pfish") %>%
#           pivot_longer(cols = -Flow_from, names_to = "Flow_to", values_to = "value") %>%
#           filter(value > 0) %>%
#           mutate(Year = names(hrs)[j],
#                  Crash_Year = interval[i],
#                  HR = case_when(
#                    k == 1 ~ "Baseline",
#                    k == 2 ~ "MSY",
#                    k == 3 ~ "2x MSY"
#                  ),
#                  HR_Prior = names(current)[k]) %>% 
#           rownames_to_column("Flow_from")
#         master <- rbind(df, master)
#       }
#     }
#     p(message = sprintf("Finished interval %d", i))  # <-- Move this inside the outer loop
#   }
# })
# 
# pfish <- master %>% filter(Flow_from == "pfish")
# 
