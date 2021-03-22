
Path = "C:/Users/b9054751/OneDrive - Newcastle University/Location-Twitter-Data-"

# Set Working Directory
setwd(Path)

#Packages
install.packages("ggplot2")
library(rtweet)
library(ggplot2)
library(dplyr)
library("rnaturalearth")
library("rnaturalearthdata")
library(httr)
library(rgdal)
library(tmap)
library(sf)

# store api keys (Replace with project specific keys)
api_key <- "p384T1Ddaet8KSel3VfuKE4xq"
api_secret_key <- "Bvaw3ztBETvJU3EfLTK788mVeom98jg2sx7w5eARSIw64qLb3j"
access_token <- "584062133-bEjxU7NHZYI3ufUy7yNDX6Fphj7hOoTVOFzZifsT"
access_token_secret <- "uIV2kma0a5EnZKxfkavIObi5LaCMp7QuulbNXopSOpIS6"

# authenticate via web browser - don't forget to change the app!
token <- create_token(
  app = "Tweet_Gaps",
  consumer_key = api_key,
  consumer_secret = api_secret_key,
  access_token = access_token,
  access_secret = access_token_secret)

stream_tweets(
  c(-10.78,49.74,1.89,58.77), 
  timeout = 10,
  file_name = "test.json",
  parse = FALSE
)
test <- parse_stream("test.json")
save(test, file = "test_live.Rda")


## search 30day for up to 300 rstats tweets sent before the last week
rt <-search_30day("place_country:GB",
                  n = 30,
                  fromDate = 202103120000,
                  toDate = 202103182249,
                  env_name = "Tweets",
                  safedir = "C:/Users/b9054751/OneDrive - Newcastle University/Location-Twitter-Data-/Twitter_Data",
                  parse = TRUE,
                  token = token)

#Dataframe and save as CSV file
Twitter20210318233427 = rt
Twitter20210318233427 = data.frame(Twitter20210318233427)
write_as_csv(Twitter20210318233427, "Twitter20210318233427.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")

#Read in to combine from each run of the algorithm
X1 = read.csv("Twitter20210318232128.csv")
X2 = read.csv("Twitter20210318231400.csv")
X3 = read.csv("Twitter20210318230940.csv")
X4 = read.csv("Twitter20210318225922.csv")
X5 = read.csv("Twitter20210318225028.csv")
X6 = read.csv("Twitter20210318224859.csv")
X7 = read.csv("Twitter20210318223427.csv")
X8 = read.csv("Twitter20210318221849.csv")


#Combine all collection phases
Tweets = rbind(X1, X2, X3, X4, X5, X6, X7, X8)

#clean up any duplicate tweets from the data frame using #dplyr::distinct
Tweets = dplyr::distinct(Tweets)

#Where are the tweets located? - making a plot of places
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
  geom_point(data = Loc, aes(x = Longitude, y = Latitude), size = 3, 
             shape = 23, fill = "darkred") +
  coord_sf(xlim = c(-10.78, 1.89), ylim = c(49.74, 58.77), expand = FALSE)
