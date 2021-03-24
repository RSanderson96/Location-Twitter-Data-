
Path = "C:/Users/b9054751/OneDrive - Newcastle University/Location-Twitter-Data-"

# Set Working Directory
setwd(Path)

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
  c(-10.79,49.8,1.99,58.86), 
  timeout = 3600,
  file_name = "test.json",
  parse = FALSE
)
TweetColl_3 <- parse_stream("test.json")
save(TweetColl_3, file = "TweetColl_3.Rda")
TweetColl_3 = data.frame(TweetColl_3)
write_as_csv(TweetColl_3, "TweetColl_3.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")


############################################LIMITED BY SUBSCRIPTIONS - ONLY MANAGED TO GET APPROXIMATELY 2 HOURS OF TWEETS

## search 30day for up to 300 rstats tweets sent before the last week
rt <-search_30day("place_country:GB",
                  n = 100,
                  fromDate = 202103120000,
                  toDate = 202103182159,
                  env_name = "Tweets",
                  safedir = "C:/Users/b9054751/OneDrive - Newcastle University/Location-Twitter-Data-/Twitter_Data",
                  parse = TRUE,
                  token = token)

rt = search_fullarchive("place_country:GB",
                   n = 500,
                   fromDate = 202103120000,
                   toDate = 202103182159,
                   env_name = "Tweets",
                   safedir = "C:/Users/b9054751/OneDrive - Newcastle University/Location-Twitter-Data-/Twitter_Data",
                   parse = TRUE,
                   token = token)

#Dataframe and save as CSV file
Twitter20210318215826 = rt
Twitter20210318215826 = data.frame(Twitter20210318215826)
write_as_csv(Twitter20210318215826, "Twitter20210318215826.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")

#Read in to combine from each run of the algorithm
X1 = read.csv("Twitter20210318232128.csv")
X2 = read.csv("Twitter20210318231400.csv")
X3 = read.csv("Twitter20210318230940.csv")
X4 = read.csv("Twitter20210318225922.csv")
X5 = read.csv("Twitter20210318225028.csv")
X6 = read.csv("Twitter20210318224859.csv")
X7 = read.csv("Twitter20210318223427.csv")
X8 = read.csv("Twitter20210318221849.csv")
X9 = read.csv("Twitter20210318221350.csv")
X10 = read.csv("Twitter20210318220931.csv")
X11 = read.csv("Twitter20210318220358.csv")
X12 = read.csv("Twitter20210318215826.csv")

#Combine all collection phases
Tweets = rbind(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12)

#clean up any duplicate tweets from the data frame using #dplyr::distinct
Tweets = dplyr::distinct(Tweets)


