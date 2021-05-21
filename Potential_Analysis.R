#Packages
#library(rtweet)
#library(ggplot2)
library(plyr)
library(dplyr)
#library("rnaturalearth")
#library("rnaturalearthdata")
#library(httr)
library(rgdal)
library(tmap)
library(sf)
library(readr)
#library("sp")
library("rgeos")
library("tcltk")
library(raster)
library(adehabitatHR)
#library(tidyr)
#library(lubridate)


#Import all twitter data 

Path = getwd()
#setwd(Path) #Path should be set to overall project


mydir = "Collected_Data" #Folder with data in
myfiles = list.files(path=mydir, pattern="*.csv", full.names=TRUE)
#myfiles
dat_csv = ldply(myfiles, read_csv)
Tweets = data.frame(dat_csv)

#Tweets = Tweets %>% select(-"X1")

Tweets = dplyr::distinct(Tweets) #checking that duplicate rows are removed - this should not be needed in final code


#How many unique usernames?

#Timeline of tweeting?

#Things to change - 
  #"3 hours" - this is the level of aggregation, needs to be adjusted to the dataset
  #Consider themes etc
  #Check titles

ts_plot(Tweets, "4 hours") +
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

#User information

# How many unique users?
Tweet_uniq <- unique(Tweets$user_id)
length(Tweet_uniq) # How many unique users?

Tweets %>%
  count(user_id, sort = TRUE) %>%
  mutate(user_id = reorder(user_id,n)) %>%
  na.omit() %>%
  top_n(25) %>%
  ggplot(aes(x = user_id,y = n)) +
  geom_col() +
  coord_flip() +
  labs(x = "Users",
       y = "Count",
       title = "Twitter users - Unique Ids ")

#EG noticed one user with over 3000 tweets

#ID = filter(Tweets, Tweets$user_id == "x819222966")
Tweets = filter(Tweets, Tweets$user_id !="x824637752574488576")


#Sources <- unique(Tweets$source) # which sources am I interesting - what kinds of connections? If businesses are connecting, isn't this still content?
#length(Sources) # How many unique users?


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
  #data <- filter(data$place_type != "admin")  
  
  
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
  pb <- txtProgressBar(min = 0, max = C, style = 3)
  for (i in 1:C){ 
    if (is.na(data[i,D])){
      data[i,D]<- ((data[i,E]+ data[i,G])/2)
    }
    setTxtProgressBar(pb, i)
  }
  close(pb)
  print ("latitude complete")
  
  #Longitude - if the row in the column is NA, it will be replaced by the equivalent midpoint of the bounding box coords
  pb <- txtProgressBar(min = 0, max = C, style = 3)
  for (i in 1:C){ 
    if (is.na(data[i,H])){
      data[i,H]<- ((data[i,I]+ data[i,J])/2)
    }
    setTxtProgressBar(pb, i)
  }
  close(pb)
  print ("longitude complete")
  
  #Output: the reduced and located dataset
  return(data)
}

Tweets = Geo_Code(Tweets)
     
#simple visualisation of all included tweets as points
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

ggplot(data = world) +
  geom_sf() +
  geom_point(data = Tweet_Points, aes(x = Calc_Long, y = Calc_Lat), size = 1, 
            shape = 23, fill = "darkred") +
  coord_sf(xlim = c(-14.02, 2.09), ylim = c(49.67, 61.06), expand = FALSE)


#https://data.cdrc.ac.uk/system/files/practical6_0.html

Tweet_Points = Tweets[,c("user_id", "created_at","source","Calc_Lat", "Calc_Long")]

#Extracting temporal data - may be used later
Tweet_Points <- tidyr::separate(data = Tweet_Points,
                                col = created_at,
                                into = c("Date", "Time"),
                                sep = " ",
                                remove = FALSE)
Tweet_Points <- tidyr::separate(data = Tweet_Points,
                       col = Time,
                       into = c("Hour", "Minute", "Second"),
                       sep = ":",
                       remove = FALSE)
Tweet_Points$Hour<- as.numeric(Tweet_Points$Hour)
Tweet_Points$Minute<- as.numeric(Tweet_Points$Minute)
Tweet_Points$Second<- as.numeric(Tweet_Points$Second)

#write.csv(Tweet_Points, "Tweet_Arc_4326.csv")

#Need to transform the twitter points into British National Grid
X<- Tweet_Points %>%
  st_as_sf(coords = c("Calc_Long", "Calc_Lat"), crs = 4326) %>%
  st_transform(27700) %>%
  st_coordinates() %>%
  as_tibble()
Tweet_Points[,"Calc_Long"] = X[,2]
Tweet_Points[,"Calc_Lat"] = X[,1]
#write.csv(Tweet_Points, "Tweet_Arc_27700_2.csv")
rm(X, Tweets)
#Turn the coordinates into a spatial file
Tweet_Points <-SpatialPointsDataFrame(Tweet_Points[,9:10], Tweet_Points, proj4string = CRS("+init=EPSG:27700"))

#writeOGR(Tweet_Points, dsn = "D:/Location-Twitter-Data-2/Shapes", layer =  "Tweet_Points_27700_4", driver="ESRI Shapefile")

#Import MSOA Information

MSOA_Population = read.csv("MSOA_Population.csv")
MSOA = st_read(dsn = (Path) ,layer="MSOA_Boundaries")
MSOA <- merge(MSOA, MSOA_Population, by.x="msoa11cd", by.y="MSOA_Code")
MSOA_SP = as(MSOA, "Spatial")

##proj4string(MSOA_SP) <- CRS("+init=EPSG:27700")
#proj4string(Tweet_Points) <- CRS("+init=EPSG:27700")

tm_shape(MSOA) + tm_fill(col = "#f0f0f0") + tm_borders(alpha=.8, col = "black") +
  tm_shape(Tweet_Points) + tm_dots(col = "blue")

#Assign an MSOA for each tweet
pip <- over(Tweet_Points, MSOA_SP)
Tweet_Points@data <- cbind(Tweet_Points@data, pip)
rm(MSOA_)
#How many tweets are in each MSOA?
Tweet_Points@data<- Tweet_Points@data[,c(1:18)]
TperMSOA<-Tweet_Points@data %>% count(MSOA_Name)

C <-MSOA %>% left_join(TperMSOA, by="MSOA_Name", all = T)
C<-merge (x = MSOA, y = TperMSOA, by="MSOA_Name", all = T)


tm_shape (C) +
  tm_polygons ("n")
###################

#Kernel Density Analysis
#https://data.cdrc.ac.uk/system/files/practical8_0.html


kde.output <- kernelUD(Tweet_Points, h="href", grid = 1000)

plot(kde.output)

# converts to raster
kde <- raster(kde.output)
# sets projection to British National Grid
projection(kde) <- CRS("+init=EPSG:27700")


#library(tmap)

# maps the raster in tmap, "ud" is the density variable
tm_shape(kde) + tm_raster("ud")


library(tmaptools) # provides a set of tools for processing spatial data

# creates a bounding box based on the extents of the Output.Areas polygon
bounding_box <- bb(MSOA)

# maps the raster within the bounding box
tm_shape(kde, bbox = bounding_box) + tm_raster("ud")

# mask the raster by the output area polygon
masked_kde <- mask(kde, MSOA)

# maps the masked raster, also maps white output area boundaries
tm_shape(masked_kde, bbox = bounding_box) + tm_raster("ud", style = "quantile", n = 100, legend.show = FALSE, palette = "YlGnBu") +
  tm_shape(MSOA) + tm_borders(alpha=.3, col = "white") +
  tm_layout(frame = FALSE)

# compute homeranges for 75%, 50%, 25% of points, objects are returned as spatial polygon data frames
range75 <- getverticeshr(kde.output, percent = 75)
range50 <- getverticeshr(kde.output, percent = 50)
range25 <- getverticeshr(kde.output, percent = 25)

tmap_mode("view") 
# the code below creates a map of several layers using tmap
tm_shape(MSOA) + tm_fill(col = "#f0f0f0") + tm_borders(alpha=.8, col = "black") +
  tm_shape(Tweet_Points) + tm_dots(col = "blue") +
  tm_shape(range75) + tm_borders(alpha=.7, col = "#fb6a4a", lwd = 2) + tm_fill(alpha=.1, col = "#fb6a4a") +
  tm_shape(range50) + tm_borders(alpha=.7, col = "#de2d26", lwd = 2) + tm_fill(alpha=.1, col = "#de2d26") +
  tm_shape(range25) + tm_borders(alpha=.7, col = "#a50f15", lwd = 2) + tm_fill(alpha=.1, col = "#a50f15") +
  tm_layout(frame = FALSE)


#writeOGR(Tweet_Points, dsn = "D:/Location-Twitter-Data-2/Shapes", layer =  "Tweet_Points", driver="ESRI Shapefile")





