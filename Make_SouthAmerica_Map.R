library(tidyverse)
library(here)



# Use custom world map ----------------------------------------------------

load(here::here("data", "tidy_data","world_map2_project.rda"))
load(here::here("data", "tidy_data", "Sino_Invest_SAmerica.rda"))



# SA nations --------------------------------------------------------------


twelve_nations <- c("Argentina", "Bolivia", "Brazil", "Chile", 
                    "Colombia", "Ecuador", "Guyana", "Paraguay", 
                    "Peru", "Suriname", "Uruguay", "Venezuela")


SAmer_map <- world_map2 %>%
  filter(country %in% twelve_nations) %>% droplevels()

## Check
unique(SAmer_map$country)




# Quick Test and Join -----------------------------------------------------


Invest_totals <- Sino_Invest_SAmerica %>% 
  group_by(Country) %>% 
  summarize(Total_Invest = sum(`Quantity in Millions`, na.rm = TRUE))

Invest_totals %>% glimpse()



## Set Map Limits

breaks_X <- seq(-90, -30, by = 15)

breaks_Y <- seq(-60, 15, by = 15)



## Update with data
SAmer_map2 <- SAmer_map %>%
  left_join(Invest_totals, by= c("country" = "Country"))

## Removing works better
SAmer_map2 <- SAmer_map2 %>% 
  filter(between(long, -95, -30) & between(lat, -60, 15))

## Save
save(SAmer_map2,  file = here::here("data", "tidy_data", "South_Amer_Map.rda"))
