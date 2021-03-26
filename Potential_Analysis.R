#Packages
library(rtweet)
library(ggplot2)
library(dplyr)
library("rnaturalearth")
library("rnaturalearthdata")
library(httr)
library(rgdal)
library(tmap)
library(sf)


library(plyr)
library(readr)

#Import all twitter data 

#setwd(" ") #Path should be set to overall project
mydir = "Collected_Data" #Folder with data in
myfiles = list.files(path=mydir, pattern="*.csv", full.names=TRUE)
myfiles
dat_csv = ldply(myfiles, read_csv)
Tweets = data.frame(dat_csv)


Tweets = dplyr::distinct(Tweets) #checking that duplicate rows are removed - this should not be needed in final code


#How many unique usernames?

#Timeline of tweeting?

#Things to change - 
  #"3 hours" - this is the level of aggregation, needs to be adjusted to the dataset
  #Consider themes etc
  #Check titles

ts_plot(Tweets, "3 hours") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of tweets over x time period",
    subtitle = "Twitter status (tweet) counts aggregated using three-hour intervals",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

#Where are the tweets located? - making a plot of places that have the most tweets

Place= Tweets[,c(1,2,3,64,65,66,69,70,71,73,76,80)]
Place %>%
  count(place_full_name, sort = TRUE) %>%
  mutate(place_full_name = reorder(place_full_name,n)) %>%
  na.omit() %>%
  top_n(25) %>%
  ggplot(aes(x = place_full_name,y = n)) +
  geom_col() +
  coord_flip() +
  labs(x = "Location",
       y = "Count",
       title = "Twitter users - unique locations ")

#Making a map

#Experimenting with using bounding box
Bound_Box = Place[,c(2,3,7,9)]
BB <- tidyr::separate(data = Bound_Box,
                      col = bbox_coords,
                      into = c("Long1", "Long2", "Long3", "Long4", "Lat1", "Lat2", "Lat3", "Lat4"),
                      sep = " ",
                      remove = FALSE)


BB = BB[,-c(4,6,8,10,12)]

# Store as numeric
BB$Long1 <- as.numeric(BB$Long1)
BB$Long3 <- as.numeric(BB$Long3)
BB$Lat1 <- as.numeric(BB$Lat1)
BB$Lat3 <- as.numeric(BB$Lat3)

# Keep only those tweets where geo information is available
BB <- subset(BB, !is.na(Long1) & !is.na(Long3) & !is.na(Lat1) & !is.na(Lat3))

BB$BB_Lat = ((BB$Lat1 + BB$Lat3)/2)
BB$BB_Long = ((BB$Long1 + BB$Long3)/2)


LSOA = readOGR(dsn = ('C:/Users/b9054751/OneDrive - Newcastle University/Location-Twitter-Data-'),layer="england_lsoa_2011")
Population = read.csv("LSOA_Population.csv")
LSOA<-merge (x = LSOA, y = Population, by.x = c("name"), by.y = c("LSOA_Name"))
LSOA %>% left_join (Population)

tm_shape(LSOA) + tm_fill(All_Ages)


Tweet_points <- BB %>% 
  st_as_sf(coords = c('BB_Long', 'BB_Lat'))


tm_shape(LSOA) +
  tm_fill("All_Ages", palette = "YlGnBu", title = "Population")#+
tm_shape(student_points_rastered) +
  tm_raster(alpha = 0.7, title = '# of students')




ggplot(data = world) +
  geom_sf() +
  geom_point(data = BB, aes(x = BB_Long, y = BB_Lat), size = 1, 
             shape = 23, fill = "darkred") +
  coord_sf(xlim = c(-10.78, 1.89), ylim = c(49.74, 58.77), expand = FALSE)

# Seperate Geo-Information (Lat/Long) Into Two Variables
Loc = Test_RS


Loc <- tidyr::separate(data = Tweets,
                       col = geo_coords,
                       into = c("Latitude", "Longitude"),
                       sep = " ",
                       remove = FALSE)



# Remove Parentheses
Loc$Latitude <- stringr::str_replace_all(Loc$Latitude, "[c(]", "")
Loc$Longitude <- stringr::str_replace_all(Loc$Longitude, "[)]", "")

# Store as numeric
Loc$Latitude <- as.numeric(Loc$Latitude)
Loc$Longitude <- as.numeric(Loc$Longitude)
# Keep only those tweets where geo information is available
Loc <- subset(Loc, !is.na(Latitude) & !is.na(Longitude))

Location = Loc[,c(1,2,3,64,65,66,69,70,71,73,76,80)]
Loc_Foc = Location[,c(1,3,8,9)]

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

ggplot(data = world) +
  geom_sf() +
  geom_point(data = Loc, aes(x = Longitude, y = Latitude), size = 1, 
             shape = 23, fill = "darkred") +
  coord_sf(xlim = c(-14.02, 2.09), ylim = c(49.67, 61.06), expand = FALSE)

ggplot(data = world) +
  geom_sf() +
  geom_point(data = BB, aes(x = BB_Long, y = BB_Lat), size = 1, 
             shape = 23, fill = "darkred") +
  coord_sf(xlim = c(-10.78, 1.89), ylim = c(49.74, 58.77), expand = FALSE)
