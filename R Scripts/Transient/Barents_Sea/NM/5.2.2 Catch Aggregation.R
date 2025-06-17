## Aggregate catch values for plot

rm(list = ls())

library(tidyverse)

# load
HR <- seq(0, 5.6, 0.2)
all_catch <- readRDS("../Objects/Experiments/Crash/Paper/Catch_Road_To_Recovery2020.RDS")

# tag on HR
all_catch <- map2(all_catch, HR, ~{ .x$HR <- .y; .x })

foo <- all_catch[[1]][["Network_Indicators"]] %>% mutate(row = seq(1,length(all_catch[[1]][["Network_Indicators"]]$NetworkData)))

# extract totals
finished <- map_dfr(all_catch, function(current) {
  total_catch <- sum(as.numeric(current$inshore_catch_mat[2, ]) +
                       as.numeric(current$offshore_catch_mat[2, ]))
  
  total_land <- sum(as.numeric(current$inshore_land_mat[2, ]) +
                      as.numeric(current$offshore_land_mat[2, ]))
  
  total_disc <- sum(as.numeric(current$inshore_disc_mat[2, ]) +
                      as.numeric(current$offshore_disc_mat[2, ]))
  
  data.frame(
    HR = current$HR,
    catch = total_catch,
    landings = total_land,
    discards = total_disc,
    Bird_Omnivory = current[["Network_Indicators"]]$NetworkData[63]
  )
})

saveRDS(finished,"../Objects/Experiments/Crash/Paper/Crash_Aggregation_2020.RDS")

ggplot(finished, aes(x = HR, y = catch)) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = c(1,2.8,5.6),linetype = "dashed") +
  labs(y = "Catch (mmN/m^2)")

