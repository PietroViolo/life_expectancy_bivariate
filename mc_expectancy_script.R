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
library(urbnmapr)
library(ggmap)
library(raster)
library(zipcodeR)
library(biscale)
library(stringr)
library(osmdata)

library(sp)
library(maps)
library(maptools)
library(sf)

#'* County life expectancy data, population and McDonald's location*

e_county <- read.csv("./Data/U.S._Life_Expectancy_at_Birth_by_State_and_Census_Tract_-_2010-2015.csv")

mc_locations <- read.csv("./Data/McDonalds.csv")

pop <- read.csv("./Data/county_pop.csv", skip = 1) %>% 
  dplyr::select(id, Estimate..SEX.AND.AGE..Total.population, Geographic.Area.Name) 




#'* Transform McDonald's zipcode to county name *

mc_locations <- mc_locations %>% 
  mutate(zipcode = substr(properties.postcode,1,5),
         zipcode = ifelse(nchar(zipcode)!=5, "99999", zipcode),
         nchar = nchar(zipcode))

# Find by zipcode

county <- c()

for(zipcode in mc_locations$zipcode){
  county<- c(county, reverse_zipcode(zipcode)$county[1])
}

mc_locations$county <- county



# Non-NA's

mc_locations_non_NA <- mc_locations %>% filter(!is.na(county))

# NA's

mc_locations_NA <- mc_locations %>% filter(is.na(county))

# Try to find county by searching city name

county <- c()

for(i in 1:length(mc_locations_NA$county)){
  
  skip_to_next <- FALSE
  
  # Note that print(b) fails since b doesn't exist
  
  tryCatch( county <- c(county, search_city(tolower(mc_locations_NA$properties.addressLine3[i]), mc_locations_NA$properties.subDivision[i])$county[1]), error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { county <- c(county,NA) }    

}

mc_locations_NA$county <- county

# append together NA's and non-NA's

mc_locations <- rbind(mc_locations_non_NA, mc_locations_NA)


# Calculate number of Macdonalds per county

mc_locations_count <- mc_locations %>% 
  group_by(county, properties.subDivision) %>% 
  summarise(n= n()) %>% 
  mutate(County = paste(county,", ", properties.subDivision, sep = ""))





# Number of fast food restaurants per county ? 

# United States box boundary
m <- c(-125.064, 23.0486, -63.2310, 49.7254)

# Building the query

q <- m %>% 
  opq (timeout = 300*100) %>%
  add_osm_feature("amenity", "fast_food")

fast_food_data <- osmdata_sf(q)

data <- tibble(fast_food_data$osm_points)

# From longitude/latitude to name of county

latlong2county <- function(pointsDF) {
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per county
  counties <- map('county', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(counties$names, ":"), function(x) x[1])
  counties_sp <- map2SpatialPolygons(counties, IDs=IDs,
                                     proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Convert pointsDF to a SpatialPoints object 
  pointsSP <- SpatialPoints(pointsDF, 
                            proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Use 'over' to get _indices_ of the Polygons object containing each point 
  indices <- over(pointsSP, counties_sp)
  
  # Return the county names of the Polygons object containing each point
  countyNames <- sapply(counties_sp@polygons, function(x) x@ID)
  countyNames[indices]
}


fips <- county.fips$fips[match(latlong2county(st_coordinates(data$geometry)), county.fips$polyname)]

# Number of fast food restaurants per fips

fips <- tibble(fips) %>% group_by(fips) %>% summarise(n = n())

# characterize fips

fips <- fips %>% mutate(fips = ifelse(nchar(fips) != 5, paste("0",fips, sep = ""),paste(fips, sep = "")))



# Population data

county <- c()
state_abb <- c()

for(i in 1:length(pop$id)){
  
  county <- c(county, (search_fips(substr(pop$id[i],10,11), substr(pop$id[i],12,14))$county[1]))
  state_abb <- c(state_abb,state.abb[match(str_split(pop$Geographic.Area.Name[i], ", ")[[1]][2],state.name)])
  
}

pop$County <- county
pop$state_abb <- state_abb

pop <- pop %>% mutate(County = paste(County, ", ", state_abb, sep = ""))

# Isolate fips code

pop <- pop %>% mutate(fips = substr(id, 10, 14))


#'*Left join data*

# We assume that the life expectancy of a county is the mean of its census tracts

e_county_mean <- e_county %>% 
  group_by(County) %>% 
  summarise(e0 = mean(Life.Expectancy, na.rm = T))

# mc_expectancy <- left_join(e_county_mean, mc_locations_count, by = "County") %>% 
#   na.omit()

mc_expectancy <- left_join(e_county_mean, pop, by = "County") 

mc_expectancy <- left_join(mc_expectancy, fips, by = "fips")

mc_expectancy <- mc_expectancy %>% mutate(mc_habs = n/Estimate..SEX.AND.AGE..Total.population * 1000)

mc_expectancy <- mc_expectancy %>% 
  dplyr::select(e0,County, mc_habs, fips)



mc_expectancy_bivariate <- bi_class(mc_expectancy, e0, mc_habs, style = "quantile", dim = 3) %>% 
  mutate(bi_class = ifelse(str_detect(bi_class, "NA"), NA, bi_class))







# custom palette

custom_pal <- c(
  "1-1" = "#A44200",
  "2-1" = "#E2D58B",
  "3-1" = "#C1D37F",
  "1-2" = "#D58936",
  "2-2" = "#3F2949",
  "3-2" = "#4F518C",
  "1-3" = "#FFF94F",
  "2-3" = "#F8F05E",
  "3-3" = "#E8EB61"
  )

# bivariate legend

legend2 <- bi_legend(pal = "DkViolet", #or custom_pal
                     dim = 3,
                     xlab = "Higher Life Expectancy",
                     ylab = "More Fast food restaurants",
                     size = 9)

# county <- c()
# 
# for(i in 1:length(counties$county_fips)){
#   
#   county <- c(county, (search_fips(substr(counties$county_fips[i],1,2), substr(counties$county_fips[i],3,5))$county[1]))
# 
# }
# 
#                                                                           counties$County <- county
# 
# counties <- counties %>% mutate(County = paste(County,", ",state_abbv, sep = ""))

counties_plot <- left_join(counties, mc_expectancy_bivariate %>% rename(county_fips = fips), by = "county_fips")


#'*Creating map*


png("typical_house_prices.png", res = 300, width = 4400, height = 2600)

pp <- ggplot(data = counties_plot,
       aes(x = long, y = lat, group = group, fill = bi_class)) +
  geom_polygon() +
  ggtitle("Mc Life Expectancy",
          subtitle = "life expectancy in function of number of McDonald's per habitant")+ 
  coord_sf(crs = 4326) +
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
        plot.subtitle = element_text(size= 10, hjust=0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm"))) +
  bi_scale_fill(pal = "DkViolet", 
                dim = 3, 
                na.value = "black") +
  annotation_custom(ggplotGrob(legend2), 
                    xmin = 80, xmax = 70,
                    ymin = 20, ymax = 30)+
  theme(legend.position = "none") +
  borders("state", colour = "white") +
  scale_size_area() +
  coord_quickmap()



dev.off()


