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
library(rtweet)

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

#Making coordinates out of the geocode and bounding box - need to pick geocode first, THEN bounding box


Geo_Code <- function(DATA){ #function to find coordinates for each tweet
  data = DATA
  #turn geo_coords & Bounding box columns into Latitude/Longitude columns
  data <- tidyr::separate(data = data,
                       col = geo_coords,
                       into = c("geo_lat", "geo_long"),
                       sep = " ",
                       remove = FALSE)
  data <- tidyr::separate(data = data,
                      col = bbox_coords,
                      into = c("BB_long1", "BB_long2", "BB_long3", "BB_long4", "BB_lat1", "BB_lat2", "BB_lat3", "BB_lat4"),
                      sep = " ",
                      remove = FALSE)
  
  #Delete duplicate columns from bounding box
  drop <- c("BB_long2", "BB_long4", "BB_lat2", "BB_lat4")
  data <- data[,!(names(data) %in% drop)]
  
  #Turn all lat/long into numeric - will have NAs in output
  cols <- c("geo_lat", "geo_long", "BB_long1", "BB_long3", "BB_lat1", "BB_lat3")
  data[cols] <- sapply(data[cols],as.numeric)
  #sapply(data, class)
  
  #Latitude column = geo first as preferred
  data["Calc_Lat"] = data$geo_lat
  data["Calc_Long"] = data$geo_long
  
  #Remove rows with no bounding box data - if they have a geo-code, they'll have a bounding box too
  data <- subset(data, !is.na(BB_lat1))  
  
  #assiging column numbers as variables - this may change by dataframe!
  C = nrow(data)
  D = which(colnames(data)=="Calc_Lat")
  E = which(colnames(data)== "BB_lat1")
  G = which(colnames(data)== "BB_lat3")
  H = which(colnames(data)=="Calc_Long")
  I = which(colnames(data)== "BB_long1")
  J = which(colnames(data)== "BB_long3")
  
  #if geoded is NA, and bounding box is not NA, need to put in midpoint 
  
  
#Latitude - if the row in the column is NA, it will be replaced by the equivalent midpoint of the bounding box coords
  for (i in 1:C){ 
    if (is.na(data[i,D])){
      data[i,D]<- ((data[i,E]+ data[i,G])/2)
      }
  }
  
  #Longitude - if the row in the column is NA, it will be replaced by the equivalent midpoint of the bounding box coords
  for (i in 1:C){ 
    if (is.na(data[i,H])){
      data[i,H]<- ((data[i,I]+ data[i,J])/2)
    }
  }
  #Output: the reduced and located dataset
  return(data)
}

Tweets = Geo_Code(Tweets)
     


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
Loc = Tweets


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
