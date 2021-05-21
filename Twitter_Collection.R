
Path = "C:/Users/b9054751/OneDrive - Newcastle University/Location-Twitter-Data-"

# Set Working Directory
setwd(Path)

#Packages
library(rtweet)
#library(ggplot2)
#library(dplyr)
#library("rnaturalearth")
#library("rnaturalearthdata")
#library(httr)
#library(rgdal)
#library(tmap)
#library(sf)

# store api keys (Replace with project specific keys, these won't work!)
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
  timeout = 10800,
  file_name = "test.json",
  parse = FALSE
)


TweetColl_18 <- parse_stream("test.json")
save(TweetColl_18, file = "TweetColl_18.Rda")
TweetColl_18 = data.frame(TweetColl_18)
write_as_csv(TweetColl_18, "TweetColl_18.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")


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





rt <- search_tweets(include_rts=FALSE, "lang:en", geocode = UK, n = 500000, retryonratelimit = TRUE, type="recent")

Test_RS5 = rt
Test_RS5 = data.frame(Test_RS5)
write_as_csv(Test_RS5, "Test_RS5.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")


TweetColl_7 <- parse_stream("test.json")
save(TweetColl_6, file = "TweetColl_6.Rda")
TweetColl_6 = data.frame(TweetColl_6)
write_as_csv(TweetColl_6, "TweetColl_6.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")

rt2<-lat_lng(rt, coords = c("coords_coords", "bbox_coords", "geo_coords"))



UK = lookup_coords_nominatim("uk")


lookup_coords_nominatim <- function(address) {
  if (missing(address)) stop("must supply address", call. = FALSE)
  stopifnot(is.atomic(address))
  place <- address
  if (grepl("^us$|^usa$|^united states$|^u\\.s",
            address,
            ignore.case = TRUE
  )) {
    boxp <- c(
      sw.lng = -124.848974,
      sw.lat = 24.396308,
      ne.lng = -66.885444,
      ne.lat = 49.384358
    )
    point <- c(
      lat = 36.89,
      lng = -95.867
    )
  } else if (grepl("^world$|^all$|^globe$|^earth$",
                   address,
                   ignore.case = TRUE
  )) {
    boxp <- c(
      sw.lng = -180,
      sw.lat = -90,
      ne.lng = 180,
      ne.lat = 90
    )
    point <- c(
      lat = 0,
      lng = 0
    )
  } else {
    ## encode address
    address <- gsub(" ", "+",  address)
    ## compose query
    params <- list(
      q = address,
      format = "json",
      limit = 1
    )
    params <- params[!vapply(params, is.null, logical(1))]
    params <- paste0(
      mapply(
        function(x, y) paste0(x, "=", y),
        names(params), params
      ),
      collapse = "&"
    )
    ## build URL - final name in English
    geourl <- paste0(
      "https://nominatim.openstreetmap.org/search?",
      params,
      "&accept-language=en"
    )
    ## read and convert to list obj
    r <- jsonlite::fromJSON(geourl)
    ## extract and name box and point data frames
    bbox <- as.double(unlist(r$boundingbox))
    boxp <- c(
      sw.lng = bbox[3],
      sw.lat = bbox[1],
      ne.lng = bbox[4],
      ne.lat = bbox[2]
    )
    point <- c(
      lat = as.double(r$lat),
      lng = as.double(r$lon)
    )
    # Full name from Nominatim
    place <- r$display_name
  }
  rtweet:::as.coords(place = place, box = boxp, point = point) # call an internal function
}

rt <- parse_stream("LB_Tweets.json")
