#Packages
library(plyr)
library(dplyr)
library(rgdal)
library(tmap)
library(ggplot2)
library(RColorBrewer)
library(sf)
library(readr)
library("rgeos")
library("tcltk")
library(raster)
library(adehabitatHR)
library(tmaptools) # provides a set of tools for processing spatial data
library(dplyr)
library(stringr)
library(spatialEco)
#library(lubridate)


#Import all twitter data that has been collected
#This is expecting an unzipped foulder called "Collected_Data", with all data as csv files

Path = getwd()

mydir = "Collected_Data" #Folder with data in
myfiles = list.files(path=mydir, pattern="*.csv", full.names=TRUE) #retrieve list of files
dat_csv = ldply(myfiles, read_csv) #import all csv files
Tweets = data.frame(dat_csv) #produces a dataframe of all tweets
Tweets = dplyr::distinct(Tweets) #checking that duplicate rows are removed


#Timeline of tweeting - this plots a chart of when all tweets were collected

#Things to change - 
  #"3 hours" - this is the level of aggregation, needs to be adjusted to the dataset
  #Consider themes etc
  #Check titles

ts_plot(Test2, "15 mins") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of tweets over a 24hour time period",
    subtitle = "Twitter status (tweet) counts aggregated using 15 minute intervals",
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
  #turn geo_coords & Bounding box columns into Latitude/Longitude columns
  data<-DATA
  data$geo_coords<- as.character(data$geo_coords)
  data$geo_coords<- str_remove(data$geo_coords,"c")
  data$geo_coords<-gsub("\\(|\\)", "", data$geo_coords)
  data <- tidyr::separate(data = data,
                       col = geo_coords,
                       into = c("geo_lat", "geo_long"),
                       sep = ", ",
                       remove = FALSE)
  data <- tidyr::separate(data = data,
                      col = bbox_coords,
                      into = c("BB_long1", "BB_long2", "BB_long3", "BB_long4", "BB_lat1", "BB_lat2", "BB_lat3", "BB_lat4"),
                      sep = ", ",
                      remove = FALSE)
  
  #Delete duplicate columns from bounding box
  drop <- c("BB_long1", "BB_long3", "BB_lat2", "BB_lat4")
  data <- data[,!(names(data) %in% drop)]
  
  #Turn all lat/long into numeric - will have NAs in output
  cols <- c("geo_lat", "geo_long", "BB_long2", "BB_long4", "BB_lat1", "BB_lat3")
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
  I = which(colnames(data)== "BB_long2")
  J = which(colnames(data)== "BB_long4")
  
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
  
  #remove admin or country level codes that don't have a geo-code
  C = nrow(data)
  X = which(colnames(data)=="geo_lat")
  P = which(colnames(data)=="place_type")
  
  pb <- txtProgressBar(min = 0, max = C, style = 3)
  for (i in 1:C){ 
    if (is.na(data[i,X]) & data[i,P] == "admin") {
      data[i,P] <- " "
      }
    setTxtProgressBar(pb, i)
    }
  close(pb)
  print ("Admins Replaced")  
  
  pb <- txtProgressBar(min = 0, max = C, style = 3)
  for (i in 1:C){ 
    if (is.na(data[i,X]) & data[i,P] == "country") {
      data[i,P] <- " "
    }
    setTxtProgressBar(pb, i)
  }
  close(pb)
  print ("Countries Replaced")
  
  data<-data %>% filter(data$place_type != " ")
  #Output: the reduced and located dataset
  return(data)
} #This is a function that identifies coordinates where available, using the centrepoints of bounding boxes


Tweets = Geo_Code(Tweets)


myFun <- function(data) {
  temp1 <- sapply(data, is.list)
  temp2 <- do.call(
    cbind, lapply(data[temp1], function(x) 
      data.frame(do.call(rbind, x), check.names=FALSE)))
  cbind(data[!temp1], temp2)
}

Tweets<- myFun(Tweets)
Tweets<-Tweets[,c(1:82)] 

write.csv(Tweets, "Recent_Convert.csv")
Tweets = read.csv("Recent_Convert.csv")


#map idea - Using Google Maps
library(ggmap)
Tweets = Tweets[,c("user_id", "created_at","source","Calc_Lat", "Calc_Long")]
Tweets = Tweets[,c(4:5)]

#df <- data.frame(lon, lat, place, stringsAsFactors = FALSE)
Tweets <- st_as_sf(Tweets, coords = c("Calc_Long", "Calc_Lat"), crs = 4326)
#leaflet(Tweets) %>% addTiles() %>% addMarkers()

leaflet(Tweets) %>% addTiles() %>% addMarkers(
  clusterOptions = markerClusterOptions()
)


lx_map <- get_map(location = c(-2.421976,	53.825564), maptype = "roadmap", zoom = 6)
# plot the map with a line for each group of shapes (route)
ggmap(lx_map, extent = "device") +
  geom_point(aes(x=Calc_Long, y=Calc_Lat ),data=Tweets, color="red", size=0.4, alpha = 0.5)


#Map idea-  without google maps
#simple visualisation of all included tweets as points
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

ggplot(data = world) +
  geom_sf() +
  geom_point(data = Tweet_Points, aes(x = Calc_Long, y = Calc_Lat), size = 1, 
            shape = 23, fill = "darkred") +
  coord_sf(xlim = c(-14.02, 2.09), ylim = c(49.67, 61.06), expand = FALSE)


#https://data.cdrc.ac.uk/system/files/practical6_0.html - working with point data

#Simplify the table to focus on aspects of interest
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

#Import MSOA Information
MSOA_Population = read.csv("MSOA_Population.csv")
MSOA_Population = MSOA_Population[,c(1,2,3,53)]
MSOA = st_read(dsn = (paste0(Path, "/MSOA")),layer="MSOA_Boundaries")
MSOA <- merge(MSOA, MSOA_Population, by.x="msoa11nm", by.y="MSOA_Name")
MSOA_SP = as(MSOA, "Spatial")

#This makes a map of the points on the MSOA shapefile
whatever <- Tweet_Points[!is.na(over(Tweet_Points, as(MSOA, "SpatialPolygons"))), ]
pip <- erase.point(Tweet_Points, MSOA_SP, inside = FALSE)
tm_shape(MSOA) + tm_fill(col = "#f0f0f0") + tm_borders(alpha=.8, col = "black") +
  tm_shape(pip) + tm_dots(col = "blue")

#Assign an MSOA for each tweet
pip <- over(Tweet_Points, MSOA_SP)
Tweet_Points@data <- cbind(Tweet_Points@data, pip)
rm(MSOA_SP)

Tweet_Points@data<- Tweet_Points@data[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,18,19)]
#How many tweets are in each MSOA?
TperMSOA<-Tweet_Points@data %>% count(msoa11nm)

MSOA <-MSOA %>% left_join(TperMSOA, by="msoa11nm", all = T)
MSOA$TweetPop = (MSOA$n/MSOA$All_Ages)*10000
MSOA$TweetYoung = (MSOA$n/MSOA$Age_16_to_64)*10000

#C<-merge (x = MSOA, y = TperMSOA, by="MSOA_Name", all = T)

#This makes a map of the MSOAs coloured by the number of tweets.
tm_shape (MSOA) +
  tm_polygons ("n")
tm_shape(MSOA) + tm_fill("n", palette = "Greens", title = "Count")+
  tm_layout(title = "Tweets per MSOA", main.title.position = "center",legend.position = c("left", "bottom"))

tmap_mode("view")
tm_shape (MSOA) +
  tm_polygons ("TweetPop")

tm_shape(MSOA) + tm_fill("TweetPop", palette = "Greens", style= "quantile", title = "Count",colorNA = "black")+
  tm_layout(title = "Tweets per 10,000", main.title.position = "center",legend.position = c("left", "bottom"))


tm_shape(MSOA) + tm_fill("TweetYoung", palette = "Greens", style= "quantile", title = "Count",colorNA = "black")+
  tm_layout(title = "Tweets per 10,000 16 - 64 year olds", main.title.position = "center",legend.position = c("left", "bottom"))

###################

#Kernel Density Analysis
#https://data.cdrc.ac.uk/system/files/practical8_0.html


kde.output <- kernelUD(pip, h="href", grid = 1000)
plot(kde.output)

# converts to raster
kde <- raster(kde.output)
# sets projection to British National Grid
projection(kde) <- CRS("+init=EPSG:27700")

# maps the raster in tmap, "ud" is the density variable
tm_shape(kde) + tm_raster("ud")


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

#tmap_mode("view") 
# the code below creates a map of several layers using tmap
tm_shape(MSOA) + tm_fill(col = "#f0f0f0") + tm_borders(alpha=.8, col = "black") +
  tm_shape(Tweet_Points) + tm_dots(col = "blue") +
  tm_shape(range75) + tm_borders(alpha=.7, col = "#fb6a4a", lwd = 2) + tm_fill(alpha=.1, col = "#fb6a4a") +
  tm_shape(range50) + tm_borders(alpha=.7, col = "#de2d26", lwd = 2) + tm_fill(alpha=.1, col = "#de2d26") +
  tm_shape(range25) + tm_borders(alpha=.7, col = "#a50f15", lwd = 2) + tm_fill(alpha=.1, col = "#a50f15") +
  tm_layout(frame = FALSE)




#Sentiment analysis
#Adapt to compare across the UK or geodemographic clusters?
#https://towardsdatascience.com/twitter-sentiment-analysis-and-visualization-using-r-22e1f70f6967
library(tidyr)
library(tidytext)
tweets.cut = Tweets[, c(2,6)]

head(tweets.cut$text)
tweets.cut$stripped_text1 <- gsub("https://t.co","", tweets.cut$text)
tweets.cut_stem<- tweets.cut %>%
  select(stripped_text1) %>%
  unnest_tokens (word, stripped_text1)

head(tweets.cut_stem)

cleaned_tweets <- tweets.cut_stem %>%
  anti_join (stop_words)

#Top 10 words

cleaned_tweets %>%
  count(word, sort = TRUE) %>%
  top_n (10) %>%
  mutate (word = reorder(word,n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col()+
  xlab(NULL) +
  coord_flip()+
  theme_classic()+
  labs(x = "Count", 
       y = "Unique words",
       title = "Unique word counts found")

bing_country1 = cleaned_tweets %>% 
  inner_join (get_sentiments ("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_country1 = cleaned_tweets %>% 
  inner_join (get_sentiments ("afinn")) %>%
  count(word, value, sort = TRUE) %>%
  ungroup()


bing_country1 %>%
  group_by (value) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word,n)) %>%
  ggplot (aes(word, n, fill = value))+
  geom_col (show.legend = FALSE)+
  facet_wrap (~value, scales = "free_y")+
  labs(title = "Tweets containing",
       y = "Contribution to sentiment",
       x = NULL)+
  coord_flip () + theme_bw()

#https://rforjournalists.com/2019/12/23/how-to-perform-sentiment-analysis-on-tweets/

sentiment_dataset <- get_sentiments("afinn")
sentiment_dataset <- arrange(sentiment_dataset, -value)
sentiment <- Tweets[,c("user_id", "text", "created_at","source","Calc_Lat", "Calc_Long")]%>% unnest_tokens(output = 'word', input = 'text')
#merge
sentiment <- merge(sentiment, sentiment_dataset, by = 'word')
sentiment$word <- NULL
sentiment$value <- as.numeric(sentiment$value)
sentiment<- sentiment %>% group_by(user_id, created_at, Calc_Lat, Calc_Long) %>% summarise(value = mean(value))
sentiment$value<-round(sentiment$value, digits =0)
#clean



#Need to transform the twitter points into British National Grid
X<- sentiment %>%
  st_as_sf(coords = c("Calc_Long", "Calc_Lat"), crs = 4326) %>%
  st_transform(27700) %>%
  st_coordinates() %>%
  as_tibble()
sentiment[,"Calc_Long"] = X[,2]
sentiment[,"Calc_Lat"] = X[,1]


#Turn the coordinates into a spatial file
Sent_Points <-SpatialPointsDataFrame(sentiment[,3:4], sentiment, proj4string = CRS("+init=EPSG:27700"))



tm_shape(MSOA) + tm_fill(col = "#f0f0f0") + tm_borders(alpha=.8, col = "black") +
  tm_shape(Sent_Points) + tm_dots(col = "value", palette = "RdYlBu")


