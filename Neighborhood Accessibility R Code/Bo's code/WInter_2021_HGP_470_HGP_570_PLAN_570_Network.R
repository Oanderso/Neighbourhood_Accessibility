#R functionality overview for Advanced Geographical Information Systems class
#Bogdan Kapatsila | 3/15/2021 | kapatsil@ualberta.ca

#1.SETUP
rm(list=ls()) #Clears work environment
par(mfrow = c(1,1)) #Return to defaults for plot matrix
par(mar=c(1,1,1,1)) #Return to defaults for plot area

install.packages('tidyverse') #Install a package
library(tidyverse) #Load a package into R

devtools::install_github("hrbrmstr/nominatim") #Use for installation when a library is not available for your version of R

--------------------------------------------------------------------------------
  
#2.LEARNING R

#Swirl is a package of lessons to learn R by doing. Learn more at https://swirlstats.com/students.html
install.packages("swirl")
library(swirl)
swirl() #Read and use prompts in console

--------------------------------------------------------------------------------

#3.WORKING WITH DATA IN R
library(tidyverse)

#Load csv file (Paste your path here, pay attention to \\)
census_prov <- read.csv("D:\\Teaching\\2021_Winter_Plan_570\\R_Demonstration\\98-401-X2016059_English_CSV_data.csv", header = TRUE, na.strings = c("","NA"))

#Basic exploration of data
summary(census_prov)
glimpse(census_prov)
head(census_prov)

#Counts and shares
table(census_prov$GEO_NAME)
round(prop.table(table(census_prov$GEO_NAME)),2)

#Let's rename a column to something more practical
names(census_prov)[names(census_prov) == 'DIM..Profile.of.Canada..Provinces.and.Territories..2247.'] <- 'variable_name'

#Create a new data frame with population, land, and density variables
census_prov_pop <- census_prov %>% filter(variable_name=='Population, 2016')
names(census_prov_pop)[names(census_prov_pop) == 'Dim..Sex..3...Member.ID...1...Total...Sex'] <- 'population'
census_prov_land <- census_prov %>% filter(variable_name=='Land area in square kilometres')
names(census_prov_land)[names(census_prov_land) == 'Dim..Sex..3...Member.ID...1...Total...Sex'] <- 'land'
census_prov_density <- census_prov %>% filter(variable_name=='Population density per square kilometre')
names(census_prov_density)[names(census_prov_density) == 'Dim..Sex..3...Member.ID...1...Total...Sex'] <- 'density'
#Here we start combining 3 daraframes
census_prov_join <- census_prov_pop %>% full_join(census_prov_land, by = 'GEO_NAME') 
census_prov_join <- census_prov_join %>% full_join(census_prov_density, by = 'GEO_NAME')
census_prov_join <- select(census_prov_join, GEO_NAME, population, land, density)

#Convert columns to proper variable type
str(census_prov_join) 
census_prov_join$population <- as.numeric(census_prov_join$population)
census_prov_join$land <- as.numeric(census_prov_join$land)

#Create a new column using data from the other two
census_prov_join <- mutate(census_prov_join, calc_density=population/land)

view(census_prov_join)

#Simple barplot
par(mfrow = c(1,1))
par(mar=c(12,5,2,1))
barplot(census_prov_join$calc_density, names=census_prov_join$GEO_NAME, col='#69b3a2', 
        las=2,
        main = "Population density by province",
        xlab=NULL,
        ylab='Density')

#Visualize everyting
census_prov_all <- census_prov_join[-c(1), ] #Remove Canada totals (first row) for clarity

par(mfrow = c(1,3))
par(mar=c(13,4,2,1))
barplot(census_prov_all$population/1000000, names=census_prov_all$GEO_NAME, col='#69b3a2', 
        las=2,
        main = "2016 Population by province",
        xlab=NULL,
        ylab='People, million',
        ylim=c(0, 14))
barplot(census_prov_all$land/1000000, names=census_prov_all$GEO_NAME, col='#69b3a2', 
        las=2,
        main = "Land area by province",
        xlab=NULL,
        ylab='Square kilometres, million',
        ylim=c(0, 2))
barplot(census_prov_all$calc_density, names=census_prov_all$GEO_NAME, col='#69b3a2', 
        las=2,
        main = "2016 Pop. density by province",
        xlab=NULL,
        ylab='Density')

--------------------------------------------------------------------------------

#4.CANCENSUS
library(tidyverse)
library(cancensus) #Register for your own API key at https://mountainmath.github.io/cancensus/index.html and paste it below
options(cancensus.api_key = "CensusMapper_21928685ecb7a453776c5f2347ecc491")
options(cancensus.cache_path = "D:\\Research\\RCache", overwrite=TRUE)
set_cache_path("D:\\Research\\RCache", install = TRUE)
library(ggplot2)
library(geojsonsf)
library(ggsn)
library(grid)


#Explore
list_census_regions("CA16")
explore_census_regions(dataset = "CA16") #Opens a web-based interactive tool


#Make a chart
vancouver_hhs <- get_census(dataset='CA16', regions=list(CMA="59933"), 
              vectors=c("v_CA16_425"), labels="detailed", geo_format=NA, level='CSD')

names(vancouver_hhs)[names(vancouver_hhs) == 'v_CA16_425: Average household size'] <- 'ahhs' #Rename some columns
names(vancouver_hhs)[names(vancouver_hhs) == 'Region Name'] <- 'region'

ggplot(vancouver_hhs, aes(x = region, y = ahhs)) + 
      geom_bar(stat="identity", fill = '#eba534') +
      geom_text(aes(label = ahhs), nudge_x = .1, nudge_y = -.1, colour = 'white') +
      labs(title='2016 Metro Vancouver average HH size', x ='Region', y = 'Average housdehold size') +
      coord_flip()


#Mapping Census data (Median household income for Edmonton CT's)
edmonton <- get_census(dataset='CA16', regions=list(CSD="4811061"),
            vectors=c("median_hh_income"="v_CA16_2397"), level='CT', quiet = TRUE, 
            geo_format = 'sf', labels = 'short')


ggplot(edmonton) + geom_sf(aes(fill = median_hh_income), colour = "white") +
  scale_fill_viridis_c("Median HH Income", labels = scales::dollar) + theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) + 
  coord_sf(datum=NA) +
  labs(title = "Median Household Income in Edmonton by Census Tracts",
       subtitle = "Source: Statistics Canada, 2016 Census")+
#Adding scale bar and north arrow    
  blank() +
  north(edmonton, scale = 0.05, symbol = 10) +
  scalebar(edmonton, dist = 5, dist_unit = "km", height = 0.01, location="topleft",
           transform = TRUE, model = "WGS84")

--------------------------------------------------------------------------------

#5.DATA SCRAPING

# Let's scrape some data from Open Street Maps (Same can be done with google maps)
# YOu have to get your API key for Open Street Maps before starting, register at https://developer.mapquest.com/ (It's free)

library(magrittr)
library(nominatim) #This library does all the scraping
library(ggplot2)
library(ggrepel)
library(DT)
library(ggthemes)
library(sp)



#I've decided to find some 50 pubs in Edmonton
edmonton_pubs <- osm_search("pubs near edmonton, ab", limit=50, key='rpH67DBpcL1WNMfVsmAVktLnDpUHjbO1')

#Let's take a look at what we have
glimpse(edmonton_pubs)

#If you want to just save the dataframe and use it outside of R
write.csv(edmonton_pubs,'D:\\Teaching\\2021_Winter_Plan_570\\Edmonton_Pubs.csv', row.names = FALSE)

#Although we can map these pubs on top of our income CTs
bnd_box <- bbox(SpatialPoints(as.matrix(edmonton_pubs[, c("lon", "lat")]))) #This line will help to zoom in to the area where pubs are
edmonton_pubs %<>% separate(display_name, c("name", "address"), ", ", remove=FALSE) #Split the column "display_name" to get names and addresses in separate columns

#Let's map it!
ggplot(edmonton) + geom_sf(aes(fill = median_hh_income), colour = "white") +
  scale_fill_viridis_c("Median HH Income", labels = scales::dollar) + theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) + 
  labs(title = "Edmonton Pubs & Median Household Income by Census Tracts",
       subtitle = "Source: Statistics Canada, 2016 Census; OSM")+
  geom_point(data=edmonton_pubs, aes(x=lon, y=lat),
                        color="#ffff33", fill="#ff7f00",
                        shape=21, size=4, alpha=1/2)+
  
#Two ways to add labels (both not ideal)
 geom_label(data=edmonton_pubs, aes(x=lon, y=lat, label=name), nudge_x = 0.002, nudge_y = 0.002, size=2, check_overlap = T)+ #Add labels
# geom_label_repel(data=edmonton_pubs, aes(x=lon, y=lat, label=name))+ #Add nicer labels using ggrepel library

  coord_sf("gilbert",
            xlim=extendrange(bnd_box["lon",], f=0.1),
            ylim=extendrange(bnd_box["lat",], f=0.1), 
            expand = FALSE, crs= NULL, datum = sf::st_crs(3400))

--------------------------------------------------------------------------------

#6.NETWORK ANALYSIS 1 (OPTIMAL ROUTE)
  
#Check out https://geocompr.robinlovelace.net/ for more details on geocomputation in R

library(osrm)
options(osrm.profile = 'foot') #The default is 'car', can be 'foot', and 'bicycle'
library(leaflet)

#Let's calculate the optimal route to visit all Edmonton Pubs
just_pubs <- select(edmonton_pubs, name, lat, lon) #Create new dataframe by selecting columns of interest only
just_pubs <- just_pubs[, c(1, 3, 2)] #Reorder columns for osrm's schema
pub_crawl <- osrmTrip(just_pubs) #Perform analysis
pub_crawl_sp <- pub_crawl[[1]]$trip # Get the spatial lines dataframe 

# Plot the result
leaflet(data = pub_crawl_sp) %>% 
  setView(lng = -113.50, lat = 53.545, zoom = 13) %>%
  addTiles() %>% 
  addMarkers(lng = just_pubs$lon, lat = just_pubs$lat, popup = just_pubs$name) %>%
  addPolylines()

# Some analytics (Time in minutes, distance in kilometres)
stretch_summary <- pub_crawl_sp@data %>%  #Distance and time between pubs
  mutate(duration = round(duration, 1),
         distance = round(distance, 1))
stretch_summary

crawl_summary <- pub_crawl[[1]]$summary #Total
crawl_summary

--------------------------------------------------------------------------------

#7.NETWORK ANALYSIS 2 (AREA/ISOCHRONES)
pub_zero <- c(-113.5021, 53.54108) #The central pub  to calculate the isochrones
iso <- osrmIsochrone(pub_zero, breaks = seq(from = 0, to = 30, by = 5), res = 30) #A series of isochrones in 5 minute increments


iso@data$crawl_times <- factor(paste(iso@data$min, "to", iso@data$max, "min"),  # Create factor for colour scale with labels for time intervals
                         levels = c("0 to 5 min", "5 to 10 min", "10 to 15 min", 
                              "15 to 20 min", "20 to 25 min", "25 to 30 min"))

factpal <- colorFactor(rev(heat.colors(6)), iso@data$crawl_times) # Colour palette for each area

# And plot
leaflet(data = iso) %>% 
  setView(lng = -113.50, lat = 53.542, zoom = 14) %>%
  addTiles() %>% 
addMarkers(lng = just_pubs$lon, lat = just_pubs$lat, popup = just_pubs$name) %>% #All pubs
  addMarkers(lng = -113.5021, lat = 53.54108, popup = "Red Star") %>% #Just pub zero
  addPolygons(fill = TRUE, stroke=TRUE, color = "black",
              fillColor = ~factpal(iso@data$crawl_times),
              weight = 0.5, fillOpacity = 0.3,
              data = iso, popup = iso@data$run_times,
              group = "Crawl Time") %>% 

# Legend
  addLegend("bottomright", pal = factpal, 
            values = iso@data$crawl_times,
            title = "Crawl Time")

#END