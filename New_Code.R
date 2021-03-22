
Path = "C:/Users/b9054751/OneDrive - Newcastle University/Location-Twitter-Data-"

# Set Working Directory
setwd(Path)

#Packages
library(rtweet)
library(ggplot2)
library(dplyr)
library("rnaturalearth")
library("rnaturalearthdata")

# store api keys (Replace with project specific keys)
api_key <- "xZ55XQya1s0o8h8xiYvPsIGH7"
api_secret_key <- "CbaM0gV6YcU0gTD0kl6n2jIvr3blCYbYBZh5rpiawSBrFngWix"
access_token <- "584062133-Gjn8op72kqx3dSM9bS8mkmMtU55bCUwxsFaCXh0w"
access_token_secret <- "BaC6HdpgpWoaJH0JvhLM7IWG87mzdg8qwKd4rsMT5myqN"

# authenticate via web browser - don't forget to change the app!
token <- create_token(
  app = "Tweet_Gaps",
  consumer_key = api_key,
  consumer_secret = api_secret_key,
  access_token = access_token,
  access_secret = access_token_secret)

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
#X1 = read.csv("Twitter20210318232128.csv")
#X2 = read.csv("Twitter20210318231400.csv")
#X3 = read.csv("Twitter20210318230940.csv")
#X4 = read.csv("Twitter20210318225922.csv")
#X5 = read.csv("Twitter20210318225028.csv")
#X6 = read.csv("Twitter20210318224859.csv")
#X7 = read.csv("Twitter20210318223427.csv")
#X8 = read.csv("Twitter20210318221849.csv")


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
