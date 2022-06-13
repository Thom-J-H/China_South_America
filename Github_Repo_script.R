## China in South America
# Github Repo Script ------------------------------------------------------



# Libraries ---------------------------------------------------------------


library(tidyverse)
library(here)
library(RColorBrewer)
library(patchwork)
library(glue)
library(scales)


# Data --------------------------------------------------------------------

load(file = here::here("data", "tidy_data", "Sino_Invest_SAmerica.rda"))
## Making the map a separate script
load(here::here("data", "tidy_data", "South_Amer_Map.rda"))



# Wide Version  -----------------------------------------------------------


## For Excel-like spreadsheet displays
Wide_sector <- Sino_Invest_SAmerica %>%
  group_by(Country, Sector) %>%
  summarize(Total_Invest = sum(`Quantity in Millions`, na.rm = TRUE)) %>%
  pivot_wider(names_from = Sector, values_from = Total_Invest)




# Plots and Charts --------------------------------------------------------




# Big Picture -------------------------------------------------------------



## Set Map Boundaries
breaks_X <- seq(-90, -30, by = 15)

breaks_Y <- seq(-60, 15, by = 15)


## Create Map
invest_map <- SAmer_map2 %>%
  ggplot(aes(x = long, 
             y = lat, 
             group = group, 
             label = country)) +
  geom_polygon(aes(fill = Total_Invest), 
               color = "white", size = 0.05 )+
  scale_fill_viridis_c(option = "C", 
                       trans = scales::log2_trans() ) +
  scale_x_continuous(breaks = breaks_X ) +
  scale_y_continuous(breaks = breaks_Y) +
  theme_minimal() +
  labs(fill = "USD ($)\nMillions",
       x = "Longitude", y = "Latitude") 


## Create Bar Chart  ## Adding a visual separator using y label
invest_bar <- Sino_Invest_SAmerica %>% group_by(Country) %>% 
  summarize(Total_Invest = sum(`Quantity in Millions`, na.rm = TRUE)) %>%
  ggplot( aes(x = reorder(Country, Total_Invest),
              y = Total_Invest, fill = Total_Invest)) +
  geom_col() +
  scale_fill_viridis_c(option = "C", 
                       trans = scales::log2_trans() ) +
  guides(fill = "none") +
  theme_minimal() +
  labs( x = " . . . .    . . . .    . . . .    . . . .  \n ", 
        y = "In USD ($) Millions") +
  scale_y_continuous(labels = scales::dollar_format()) +
  coord_flip()  


## Combine with Patchwork

combined <- invest_map + invest_bar

## Final version

big_picture_country <- combined + plot_annotation(
  title = "Chinese Investment in South America: Years 2005-2021",
  subtitle = "Data source @ aei.org/china-global-investment-tracker" ,
  caption = 'Data Humanist, CC0 (Public Domain)')


big_picture_country 


## Display stats
big_picture_country_numbers <-  Sino_Invest_SAmerica %>% group_by(Country) %>% 
  summarize(Total_Invest = sum(`Quantity in Millions`, 
                               na.rm = TRUE) ) %>%
  #arrange(desc(Total_Invest)) %>%
  pivot_wider(names_from = Country, values_from = Total_Invest)


big_picture_country_numbers  %>% 
  knitr::kable(caption = "Chinese Investment in USD ($) Millions for Years 2005-2021")



# Annual -- by Year -------------------------------------------------------


breaks_years <- seq(2005, 2023, by = 2)



## By Year  Gross totals

yearly_china_plot <- Sino_Invest_SAmerica %>% 
  group_by(Year) %>% 
  summarize(Total_Invest = sum(`Quantity in Millions`, na.rm = TRUE)) %>%
  ggplot( aes(x = Year, y = Total_Invest, fill = Total_Invest)) +
  geom_col() +
  scale_fill_viridis_c() +
  theme_minimal() +
  scale_x_continuous(breaks = breaks_years) +
  labs(title ="Chinese Investment in South America Yearly: 2005-2021",
       x = "Year", y = "In Millions USD ($)",
       subtitle = "Data source @ aei.org/china-global-investment-tracker" ,
       caption = 'Data Humanist, CC0 (Public Domain)',
       fill = "Millions\nUSD ($)") +
  scale_y_continuous(labels = scales::dollar_format()) +
  theme(legend.position = "none")



yearly_china_plot



## Display Chart

yearly_china_table <- Sino_Invest_SAmerica %>% 
  group_by(Year) %>% 
  summarize(Total_Invest = sum(`Quantity in Millions`, na.rm = TRUE)) %>%
  pivot_wider(names_from = Year, values_from = Total_Invest)

yearly_china_table %>% 
  knitr::kable(caption = " Annual Total of Chinese Investment in South America: USD ($) Millions")




# By year sector breakdown ------------------------------------------------


## KEEP colors consisent for ALL sector graphs
#1 Agriculture "#A6CEE3"
#2 Chemicals  "#1F78B4" 
#3 Energy    "#B2DF8A"   
#4 Finance    "#33A02C"
#5 Health     "#FB9A99"
#6 Logistics  "#E31A1C"
#7 Metals     "#FDBF6F"
#8 Real estate "#FF7F00" 
#9 Technology  "#CAB2D6" 
#10 Transport  "#6A3D9A" 
#11 Utilities "#FFFF99"
##########################



## By Sector Yearly
sector_yearly_plot <- Sino_Invest_SAmerica %>% 
  group_by(Year, Sector) %>% 
  summarize(Total_Invest = sum(`Quantity in Millions`, na.rm = TRUE)) %>%
  ggplot( aes(x = Year, y = Total_Invest, fill = Sector)) +
  geom_col() +
  scale_fill_brewer(palette = "Paired") +
  theme_minimal() +
  scale_x_continuous(breaks = breaks_years) +
  labs(title ="Chinese Investment in South America Yearly by Sector",
       x = "Year", y = "In Millions USD ($)",
       subtitle = "Data source @ aei.org/china-global-investment-tracker" ,
       caption = 'Data Humanist, CC0 (Public Domain)') +
  scale_y_continuous(labels = scales::dollar_format()) 



sector_yearly_plot 



## COMBO Version 

# turn off these labels
fig1 <- yearly_china_plot + labs(title = NULL, 
                                 subtitle = NULL,
                                 caption = NULL,
) 

## use y label as visual separator
fig2 <- sector_yearly_plot + labs(title = NULL, 
                                  subtitle = NULL,
                                  caption = NULL, y = " . . . .    . . . .    . . . .    . . . .  \n ")

combined <- fig1 + fig2 

## Add shared title, subtile, and caption
combined +  plot_annotation(
  title = 'Chinese Investment in South America: Annual Gross & Annual Gross per Sector Breakdown',
  subtitle = 'In Millions USD ($). Data source @ aei.org/china-global-investment-tracker',
  caption = 'Data Humanist, CC0 (Public Domain)'
)


# By sector all -----------------------------------------------------------



### By Sector ALL

sector_all_plot  <- Sino_Invest_SAmerica %>% group_by( Sector) %>% 
  summarize(Total_Invest = sum(`Quantity in Millions`, na.rm = TRUE)) %>%
  ggplot( aes(x = Sector, y = Total_Invest, fill = Sector)) +
  geom_col() +
  scale_fill_brewer(palette = "Paired") +
  theme_minimal() +
  labs(title ="Chinese Investment in South America by Sector (Years 2005-2021)",
       x = "", y = "In Millions USD ($)",
       subtitle = "Data source @ aei.org/china-global-investment-tracker" ,
       caption = 'Data Humanist, CC0 (Public Domain)') +
  scale_y_continuous(labels = scales::dollar_format(),
                     breaks = c(0, 10000,
                                seq(20000, 100000, by = 10000))) 



sector_all_plot +
  guides(fill = "none") +
  coord_flip() +
  geom_text( aes( label = Total_Invest), size = 2.5, 
             color= "red", hjust = -0.1)


### Alternate "Small Seven Sectors" plot

these_four<- c("Metals", "Energy", "Transport", "Agriculture")

small_sectors_plot <- Sino_Invest_SAmerica %>% 
  filter(!Sector %in% these_four) %>% 
  group_by( Sector) %>% 
  summarize(Total_Invest = sum(`Quantity in Millions`, na.rm = TRUE)) %>%
  ggplot( aes(x = Sector, y = Total_Invest, fill = Sector)) +
  geom_col() +
  scale_fill_brewer(palette = "Paired") +
  theme_minimal() +
  labs(title ="Chinese Investment in South America (2005-2021) by Sector (Excludes Big Four)",
       x = "Year", y = "In Millions USD ($)",
       subtitle = "Data source @ aei.org/china-global-investment-tracker" ,
       caption = 'Data Humanist, CC0 (Public Domain)') +
  scale_y_continuous(labels = scales::dollar_format())  +
  guides(fill = "none") +
  coord_flip() +
  geom_text( aes( label = Total_Invest), size = 2.5, 
             color= "red", hjust = -0.1)




small_sectors_plot 





# By Nation: Total as Sector Breakdown ------------------------------------



# Peru --------------------------------------------------------------------


peru_invest <- Sino_Invest_SAmerica %>% 
  filter(Country == "Peru") %>% 
  replace_na( list(Subsector = "Unspecified") ) %>% 
  group_by(Sector) %>% 
  summarize(Total_Invest = sum(`Quantity in Millions`, na.rm = TRUE)) %>%
  ggplot( aes(x = Sector, y = Total_Invest, fill = Sector)) +
  geom_col() +
  theme_minimal() +
  guides(fill = "none")+
  scale_fill_manual(values = c( "#B2DF8A" , "#33A02C" , "#E31A1C",  "#FDBF6F", "#6A3D9A" )) +
  labs(title = "Peru: Chinese Investment Total by Sector (Years 2005-2021)",
       y = "In Millions USD ($)",
       subtitle = "Data source @ aei.org/china-global-investment-tracker" ,
       caption = 'Data Humanist, CC0 (Public Domain)') +
  scale_y_continuous(labels = scales::dollar_format())



peru_invest


## Display Chart
Peru_sector_chart <- Wide_sector %>% 
  filter(Country == "Peru") %>%
  select_if(~ !any(is.na(.))) 

Peru_sector_chart %>% 
  knitr::kable(caption = "Peru: Chinese Investment by Sector (Years 2005-2021)")



# Argentina ---------------------------------------------------------------



argentina_invest <- Sino_Invest_SAmerica %>% 
  filter(Country == "Argentina") %>% 
  replace_na( list(Subsector = "Unspecified") ) %>% 
  group_by(Sector) %>% 
  summarize(Total_Invest = sum(`Quantity in Millions`, na.rm = TRUE)) %>%
  ggplot( aes(x = Sector, y = Total_Invest, fill = Sector)) +
  geom_col() +
  theme_minimal() +
  guides(fill = "none")+
  scale_fill_manual(values = c( "#1F78B4" ,  "#B2DF8A" , "#33A02C",
                                "#FDBF6F" ,"#CAB2D6" , "#6A3D9A" )) +
  labs(title = "Argentina: Chinese Investment by Sector (Years: 2005-2021)",
       y = "In Millions USD ($)",
       subtitle = "Data source @ aei.org/china-global-investment-tracker" ,
       caption = 'Data Humanist, CC0 (Public Domain)') +
  scale_y_continuous(labels = scales::dollar_format())



argentina_invest



### Display Chart
Argentina_sector_chart <- Wide_sector %>% 
  filter(Country == "Argentina") %>%
  select_if(~ !any(is.na(.))) 

Argentina_sector_chart %>% 
  knitr::kable(caption = "Argentina: Chinese Investment by Sector (Years 2005-2021)")



# Brazil ------------------------------------------------------------------

brazil_invest <- Sino_Invest_SAmerica %>% 
  filter(Country == "Brazil") %>% 
  replace_na( list(Subsector = "Unspecified") ) %>% 
  group_by(Sector) %>% 
  summarize(Total_Invest = sum(`Quantity in Millions`, na.rm = TRUE)) %>%
  ggplot( aes(x = Sector, y = Total_Invest, fill = Sector)) +
  geom_col() +
  theme_minimal() +
  guides(fill = "none")+
  scale_fill_manual(values = c( "#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", 
                                "#E31A1C", "#FDBF6F", "#FF7F00" , "#CAB2D6" , 
                                "#6A3D9A", "#FFFF99")) +
  labs(title = "Brazil: Chinese Investment by Sector (Years: 2005-2021)",
       y = "In Millions USD ($)",
       subtitle = "Data source @ aei.org/china-global-investment-tracker" ,
       caption = 'Data Humanist, CC0 (Public Domain)') +
  scale_y_continuous(labels = scales::dollar_format())



brazil_invest



### Display Chart


Brazil_sector_chart <- Wide_sector %>% 
  filter(Country == "Brazil") %>%
  select_if(~ !any(is.na(.))) 

Brazil_sector_chart%>% 
  knitr::kable(caption = "Brazil: Chinese Investment by Sector (Years 2005-2021)")




# Chile  ------------------------------------------------------------------



chile_invest <- Sino_Invest_SAmerica %>% 
  filter(Country == "Chile") %>% 
  replace_na( list(Subsector = "Unspecified") ) %>% 
  group_by(Sector) %>% 
  summarize(Total_Invest = sum(`Quantity in Millions`, na.rm = TRUE)) %>%
  ggplot( aes(x = Sector, y = Total_Invest, fill = Sector)) +
  geom_col() +
  theme_minimal() +
  guides(fill = "none")+
  scale_fill_manual(values = c( "#A6CEE3", "#B2DF8A" , "#FB9A99", "#FDBF6F", 
                                "#CAB2D6" , "#6A3D9A" )) +
  labs(title = "Chile: Chinese Investment by Sector (Years 2005-2021)",
       y = "In Millions USD ($)",
       subtitle = "Data source @ aei.org/china-global-investment-tracker" ,
       caption = 'Data Humanist, CC0 (Public Domain)') +
  scale_y_continuous(labels = scales::dollar_format())

chile_invest



### Display Chart
Chile_sector_chart <- Wide_sector %>% 
  filter(Country == "Chile" ) %>%
  select_if(~ !any(is.na(.))) 

Chile_sector_chart %>% 
  knitr::kable(caption = "Chile: Chinese Investment in by Sector (Years 2005-2021)")





# Colombia, Ecuador, Guyana, Venezuela  -----------------------------------


small_four <- c("Colombia", "Ecuador" ,
                "Guyana", "Venezuela")

small_four_plot <- Sino_Invest_SAmerica %>% 
  filter(Country %in% small_four) %>% 
  group_by(Country, Sector) %>% 
  summarize(Total_Invest = sum(`Quantity in Millions`, na.rm = TRUE)) %>%
  ggplot( aes(x = Sector, y = Total_Invest, fill = Sector)) +
  geom_col() +
  theme_minimal() +
  guides(fill = "none")+
  scale_fill_manual(values = c( "#B2DF8A" , "#FDBF6F" , "#6A3D9A" )) +
  facet_wrap(~ Country, scales = "free_x") +
  labs(title = "Colombia, Ecuador, Guyana, Venezuela: Chinese Investment by Sector (Years 2005-2021)",
       y = "In Millions USD ($)",
       subtitle = "Data source @ aei.org/china-global-investment-tracker" ,
       caption = 'Data Humanist, CC0 (Public Domain)') +
  scale_y_continuous(labels = scales::dollar_format())


small_four_plot  +
  theme(strip.text.x = element_text(size = 12))



### Display Table
small_four_chart <- Sino_Invest_SAmerica %>% 
  filter(Country %in% small_four, Sector %in%  these_sectors ) %>% 
  group_by(Country, Sector) %>% 
  summarize(Total_Invest = sum(`Quantity in Millions`, na.rm = TRUE)) %>% 
  pivot_wider(names_from = Sector, values_from = Total_Invest)


small_four_chart %>%
  knitr::kable(caption = "Colombia, Ecuador, Guyana, Venezuela : Chinese Investment by Sector (Years 2005-2021)")



# Energy & Metals ---------------------------------------------------------



these_two <- c("Metals", "Energy")

energy_metals_plot <- Sino_Invest_SAmerica %>% group_by(Year, Sector) %>% 
  filter(Sector %in% these_two) %>% 
  summarize(Total_Invest = sum(`Quantity in Millions`, na.rm = TRUE)) %>%
  ggplot( aes(x = Year, y = Total_Invest, fill = Sector)) +
  geom_col() +
  scale_fill_manual(values = c( "#B2DF8A" ,"#FDBF6F")) +
  theme_minimal() +
  scale_x_continuous(breaks = breaks_years) +
  labs(title ="Energy & Metals Sectors: Chinese Investment in South America (Millions USD)",
       x = "Year", y = "In Millions USD ($)",
       subtitle = "Data source @ aei.org/china-global-investment-tracker" ,
       caption = 'Data Humanist, CC0 (Public Domain)') +
  scale_y_continuous(labels = scales::dollar_format())



energy_metals_plot  





### Display Chart
energy_metals_chart <-Sino_Invest_SAmerica %>% 
  group_by(Year, Sector) %>% 
  filter(Sector %in% these_two) %>% 
  summarize(Total_Invest = sum(`Quantity in Millions`, na.rm = TRUE)) %>%
  pivot_wider(names_from = Year, values_from = Total_Invest)


energy_metals_chart %>% 
  knitr::kable(caption = "Energy & Metals Sectors: Chinese Investment in South America (Years 2005-2021)")




# Energy  -----------------------------------------------------------------


energy_plot <- Sino_Invest_SAmerica %>% 
  filter(Sector == "Energy") %>% 
  replace_na( list(Subsector = "Unspecified") ) %>% 
  group_by(Year, Subsector) %>% 
  summarize(Total_Invest = sum(`Quantity in Millions`, na.rm = TRUE)) %>%
  ggplot( aes(x = Year, y = Total_Invest, fill = Subsector)) +
  geom_col() +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  scale_x_continuous(breaks = breaks_years) +
  labs(title ="Energy Sector: Chinese Investment in South America",
       x = "Year", y = "In Millions USD ($)",
       subtitle = "Data source @ aei.org/china-global-investment-tracker" ,
       caption = 'Data Humanist, CC0 (Public Domain)') +
  scale_y_continuous(labels = scales::dollar_format())

energy_plot 


### Display Chart

energy_stats_wide <- Sino_Invest_SAmerica %>% 
  filter(Sector == "Energy") %>% 
  replace_na( list(Subsector = "Unspecified") ) %>% 
  group_by(Year, Subsector) %>% 
  summarize(Total_Invest = sum(`Quantity in Millions`, na.rm = TRUE)) %>%
  pivot_wider(names_from = Year, values_from = Total_Invest)

energy_stats_wide %>% 
  knitr::kable(caption = "Energy Sector: Chinese Investment in South America (Millions USD)")


# Metals  -----------------------------------------------------------------



metals_plot <- Sino_Invest_SAmerica %>% 
  filter(Sector == "Metals") %>% 
  replace_na( list(Subsector = "Unspecified") ) %>% 
  group_by(Year, Subsector) %>% 
  summarize(Total_Invest = sum(`Quantity in Millions`, na.rm = TRUE)) %>%
  ggplot( aes(x = Year, y = Total_Invest, fill = Subsector)) +
  geom_col() +
  scale_fill_brewer(palette = "Dark2") +
  theme_minimal() +
  scale_x_continuous(breaks = breaks_years) +
  labs(title ="Metals Sector: Chinese Investment in South America (Millions USD)",
       x = "Year", y = "In Millions USD ($)",
       subtitle = "Data source @ aei.org/china-global-investment-tracker" ,
       caption = 'Data Humanist, CC0 (Public Domain)') +
  scale_y_continuous(labels = scales::dollar_format())


metals_plot 




### Display Chart

metals_stats_wide <-  Sino_Invest_SAmerica %>% 
  filter(Sector == "Metals") %>% 
  replace_na( list(Subsector = "Unspecified") ) %>% 
  group_by(Year, Subsector) %>% 
  summarize(Total_Invest = sum(`Quantity in Millions`, na.rm = TRUE)) %>%
  pivot_wider(names_from = Year, values_from = Total_Invest) 

metals_stats_wide  %>% 
  knitr::kable(caption = "Metals Sector: Chinese Investment in South America (Years 2005-2021)")


# END ---------------------------------------------------------------------


















