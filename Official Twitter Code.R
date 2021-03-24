#Twitter code for different forms of collation and analysis collected from a variety of online sources, 
#example ref -  https://www.mzes.uni-mannheim.de/socialsciencedatalab/article/collecting-and-analyzing-twitter-using-r/
#https://www.earthdatascience.org/courses/earth-analytics/get-data-using-apis/use-twitter-api-r/


#Stage 1: Packages/Initial setup

#install.packages("rtweet")
#install.packages("ggmap")
#install.packages("igraph")
#install.packages("ggraph")
#install.packages("tidytext")
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("readr")
#install.packages("askpass")
#install.packages("mapproj")
#install.packages(c("rnaturalearth", "rnaturalearthdata"))
#
if (!requireNamespace("httpuv", quietly = TRUE)) {
  install.packages("httpuv")
}

#open library
library(rtweet)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggmap)
library("httr")
#library("rnaturalearth")
#library("rnaturalearthdata")

Path = "C:/Users/b9054751/OneDrive - Newcastle University/Location-Twitter-Data-" 

# Set Working Directory
setwd(Path)

#Stage2: setting up the API

# store api keys (Replace with project specific keys)
api_key <- "*********"
api_secret_key <- "***********"
access_token <- "**********"
access_token_secret <- "************"

# authenticate via web browser - don't forget to change the app!
token <- create_token(
  app = "Tweet_Gaps",
  consumer_key = api_key,
  consumer_secret = api_secret_key,
  access_token = access_token,
  access_secret = access_token_secret)

#Testing - will it collect tweets?
#Stream all tweets 
#c = bounding box https://boundingbox.klokantech.com/, select coordinates as 'CSV' - in this case for the UK
#timeout = how many seconds to collect tweets for (here 120 sec/2mins)
#file_name = output

stream_tweets(
  c(-10.78,49.74,1.89,58.77), 
  timeout = 100,
  file_name = "test.json",
  parse = FALSE
)
test <- parse_stream("test.json")
save(test, file = "test_live.Rda")


#Collecting historical tweets with academic API
## search 30day for up to 300 rstats tweets sent before the last week
rt <-search_30day("place_country:GB",
                  n = 3000,
                  fromDate = 202103120000,
                  toDate = 202103182330,
                  env_name = "Tweets",
                  safedir = "C:/Users/b9054751/OneDrive - Newcastle University/PhD/Data/Twitter/DATA",
                  parse = TRUE,
                  token = token)

#3000 was limit - this was approx 30 minutes of twitter data


test %>%
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

# Seperate Geo-Information (Lat/Long) Into Two Variables
Loc <- tidyr::separate(data = test,
                       col = geo_coords,
                       into = c("Latitude", "Longitude"),
                       sep = ",",
                       remove = FALSE)

# Remove Parentheses
Loc$Latitude <- stringr::str_replace_all(Loc$Latitude, "[c(]", "")
Loc$Longitude <- stringr::str_replace_all(Loc$Longitude, "[)]", "")
Loc
# Store as numeric
Loc$Latitude <- as.numeric(Loc$Latitude)
Loc$Longitude <- as.numeric(Loc$Longitude)
# Keep only those tweets where geo information is available
Loc <- subset(Loc, !is.na(Latitude) & !is.na(Longitude))

# Set up empty map

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

ggplot(data = world) +
  geom_sf() +
  geom_point(data = Loc, aes(x = Longitude, y = Latitude), size = 3, 
             shape = 23, fill = "darkred") +
  coord_sf(xlim = c(-10.78, 1.89), ylim = c(49.74, 58.77), expand = FALSE)


#Experimenting with time
Time = test
Time <- tidyr::separate(data = Time,
                        col = created_at,
                        into = c("Date", "Time"),
                        sep = " ",
                        remove = FALSE)

Time$Year = format(as.Date(Time$Date, format="%Y-%m-%d"),"%Y")
Time$Month = format(as.Date(Time$Date, format="%Y-%m-%d"),"%m")
Time$Day = format(as.Date(Time$Date, format="%Y-%m-%d"),"%d")



ts_plot(Time, by = "mins", trim = 0L, tz = "UTC")

#Need to find an alternative - seeing tweets for a longer period of time using Academic API

