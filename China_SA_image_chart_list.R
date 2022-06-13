## Images List

library(tidyverse)
library(patchwork)

load("~/R_STUDIO/China_Global/data/tidy_data/dashboard_mess.RData")

# Map data ----------------------------------------------------------------


big_picture_country  

big_picture_country_numbers  %>% 
  knitr::kable(caption = "Chinese Investment in Millions USD ($) for Years 2005-2021")


# Yearly graphs -----------------------------------------------------------

## Gross by Year
yearly_china_plot

yearly_china_table %>% 
  knitr::kable(caption = " Annual Total of Chinese Investment in South America: USD ($) Millions")


## Year breakdown into sector

sector_yearly_plot 


## Combo plot

combined +  plot_annotation(
  title = 'Chinese Investment in South America (USD Millions): Annual Gross & Sectors Breakdown',
  subtitle = 'Data source @ aei.org/china-global-investment-tracker',
  caption = 'Data Humanist, CC0 (Public Domain)'
)




# Sectors -----------------------------------------------------------------

sector_all_plot +
  guides(fill = "none") +
  coord_flip() +
  geom_text( aes( label = Total_Invest), size = 2.5, 
             color= "red", hjust = -0.1)



# Nation by Sector --------------------------------------------------------


## PERU
peru_invest

peru_invest + labs(title = "Peru: Chinese Investment by Sector (Years 2005-2021)")
  

Peru_sector_chart %>% 
  knitr::kable(caption = "Peru: Chinese Investment (USD Millions) by Sector")


## Argentian


argentina_invest +
  labs(title = "Argentina: Chinese Investment by Sector (Years 2005-2021)")
  

Argentina_sector_chart %>% 
  knitr::kable(caption = "Argentina: Chinese Investment (USD Millions) by Sector")




# Brazil ------------------------------------------------------------------



brazil_invest  + 
  labs(title = "Brazil: Chinese Investment by Sector (Years 2005-2021)")



Brazil_sector_chart%>% 
  knitr::kable(caption = "Brazil: Chinese Investment (USD Millions) by Sector")


## Chile

chile_invest

Chile_sector_chart %>% 
  knitr::kable(caption = "Chile: Chinese Investment (USD Millions) by Sector")




## Colombia, Ecuador, Guyana, Venezuela
small_four_plot  +
  theme(strip.text.x = element_text(size = 12))



small_four_chart %>%
  knitr::kable(caption = "Colombia, Ecuador, Guyana, Venezuela : Chinese Investment (USD Millions) by Sector")




# Energy & Metals ---------------------------------------------------------


energy_metals_plot +
  labs(title = "Energy & Metals Sectors: Chinese Investment Annually in South America (Millions USD)")



energy_metals_chart %>% 
  knitr::kable(caption = "Energy & Metals Sectors: Chinese Investment (USD Millions) for South America")


# Energy ------------------------------------------------------------------


energy_plot +
  labs(title = "Energy Subsectors: Chinese Investment Annually in South America (Millions USD)")


energy_stats_wide %>% 
  knitr::kable(caption = "Energy Sector: Chinese Investment (USD Millions) in South America")


# Metals ------------------------------------------------------------------

metals_plot +
  labs(title = "Metal Subsectors: Chinese Investment Annually in South America (Millions USD)")



metals_stats_wide  %>% 
  knitr::kable(caption = "Metals Sector: Chinese Investment (USD Millions) in South America")














# breathing room ----------------------------------------------------------


