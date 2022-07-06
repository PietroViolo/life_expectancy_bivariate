#---------------------------------------------------------------------------#
# Nom : mc_expectancy_script.R                                			        #
# Description : Compares life expectancy to the number of mcdonald's in a   #
# given county                                                              #
# Auteur: Pietro Violo                                                      #
# Date :  5 juillet 2022                                                    #
# Modifications :                                                           #
#---------------------------------------------------------------------------#

rm(list=ls(all=TRUE))

#'* Libraries *

#devtools::install_github("UrbanInstitute/urbnmapr")
# remotes::install_github('ropensci/osmdata')

library(tidyverse)
library(viridis)
library(RColorBrewer)
library(urbnmapr)
library(ggmap)
library(raster)
library(zipcodeR)

#'* County life expectancy data, population and McDonald's location*

e_county <- read.csv("./Data/U.S._Life_Expectancy_at_Birth_by_State_and_Census_Tract_-_2010-2015.csv")

mc_locations <- read.csv("./Data/McDonalds.csv")

#'* Transform McDonald's zipcode to county name *

mc_locations <- mc_locations %>% 
  mutate(zipcode = substr(properties.postcode,1,5),
         county = reve)






#'* Join data and plot it *

#urbnmapr::counties

household_data <- left_join(counties, df_joined, by = "county_fips") %>% 
  mutate(years_to_buy = case_when(years_to_buy < 2 ~ "[0, 2[",
                                 years_to_buy>= 2 & years_to_buy < 4 ~ "[2, 4[",
                                  years_to_buy>= 4 & years_to_buy < 6 ~ "[4, 6[",
                                  years_to_buy >= 6 & years_to_buy < 8 ~ "[6, 8[",
                                  years_to_buy >= 8 & years_to_buy < 10 ~ "[8, 10[",
                                  years_to_buy >= 10 & years_to_buy < 16 ~ "[10, 15[",
                                  years_to_buy >= 15 ~ "15 +"))

my_palette <- inferno(7)


png("typical_house_prices.png", res = 300, width = 4400, height = 2600)

pp <- ggplot(data = household_data,
       aes(x = long, y = lat, group = group, fill = years_to_buy)) +
  geom_polygon() +
  ggtitle("Number of years of the median household income to equate to the value of a typical home, for US counties in 2022",
          subtitle = "The Zillow Home Value Index (ZHVI) is a smoothed, seasonally adjusted measure of the typical home value and market changes across a given region and housing type. \nIt reflects the typical value for homes in the 35th to 65th percentile range. \n The measure is then divided by the county's median household income to obtain the number of years required to equate the home value.")+
  coord_map(projection = "albers", lat = 45, lat1 = 55) +
  theme(legend.position="bottom",
        axis.line=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        plot.background = element_rect(fill = "#f5f5f2", color = NA),
        panel.background = element_rect(fill = "#f5f5f2", color = NA), 
        legend.background = element_rect(fill = "#f5f5f2", color = NA),
        panel.border=element_blank(),
        panel.grid=element_blank(),
        plot.title = element_text(size= 18, hjust=0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
        plot.subtitle = element_text(size= 10, hjust=0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")))+
  scale_fill_manual( 
    values=my_palette, 
    name="Number of years of median household income", 
    guide = guide_legend( keyheight = unit(3, units = "mm"), keywidth=unit(8, units = "mm"), label.position = "bottom", title.position = 'top', nrow=1),
    limits = c("[0, 2[","[2, 4[", "[4, 6[",
               "[6, 8[", "[8, 10[", "[10, 15[", "15 +"),
    na.value = "grey35"
    

  )

dev.off()

