Path = "C:/Users/b9054751/OneDrive - Newcastle University/Location-Twitter-Data-"

# Set Working Directory
setwd(Path)

#Packages
library(rtweet)
library(ggplot2)
library(dplyr)

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
                  n = 100,
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
Place= Tweets
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

# Seperate Geo-Information (Lat/Long) Into Two Variables
Loc = Tweets
Loc <- subset(Loc, !is.na(geo_coords))

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
 #55/3000 had location information...