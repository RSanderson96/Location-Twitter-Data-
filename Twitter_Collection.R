#This code shows a range of options to collect twitter data through the API using RStudio


Path = "C:/Users/b9054751/OneDrive - Newcastle University/Location-Twitter-Data-"

# Set Working Directory
setwd(Path)

#Packages
library(rtweet)

#Get twitter keys from seperate (private) R file
source("Twitter_Keys.R")

#Basic streaming - connect to API for 1% live tweets

stream_tweets(
  c(-10.79,49.8,1.99,58.86), 
  timeout = 10800,
  file_name = "test.json",
  parse = FALSE
)


#Save the tweets collected 
Tweets <- parse_stream("test.json")
Tweets = data.frame(Tweets)
write_as_csv(Tweets, "Tweets.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")


############################################LIMITED BY SUBSCRIPTIONS - ONLY MANAGED TO GET APPROXIMATELY 2 HOURS OF TWEETS

## search 30day for up to 300 rstats tweets sent before the last week
rt <-search_30day("place_country:GB",
                  n = 10,
                  fromDate = 202105180000, #Earlier date
                  toDate = 202105190000, #Later date
                  env_name = "Tweets",
                  safedir = paste0(Path, "/Tweets"),
                  parse = TRUE,
                  token = token)

#Remember to turn tweets into a dataframe and save as CSV file
Tweets = data.frame(rt)
write_as_csv(Tweets, "Tweets.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")

rt = search_fullarchive("place_country:GB",
                   n = 500,
                   fromDate = 202105180000,
                   toDate = 202105190000,
                   env_name = "Tweets",
                   safedir = paste0(Path, "/Tweets"),
                   parse = TRUE,
                   token = token)

#Remember to turn tweets into a dataframe and save as CSV file
Tweets = data.frame(rt)
write_as_csv(Tweets, "Tweets.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")



#Search tweets option - last 7 days
#Geocode needs to be set up - 

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
} #Code found on Github
UK = lookup_coords_nominatim("uk") #UK chosen as location

#Collect tweets
rt <- search_tweets(include_rts=FALSE, "lang:en", geocode = UK, n = 10, retryonratelimit = TRUE, type="recent")
Tweets = data.frame(rt)
write_as_csv(Tweets, "Tweets.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")





