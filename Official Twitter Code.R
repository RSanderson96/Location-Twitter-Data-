Path = "C:/Users/b9054751/OneDrive - Newcastle University/Location-Twitter-Data-"

# Set Working Directory
setwd(Path)

library(rtweet)
library(ggplot2)
library(dplyr)

# store api keys (Replace with project specific keys)
api_key <- "tlmRvxEQ3t2wzdiF1GKSUVThG"
api_secret_key <- "jY6gwA0WJTeqENlI3fFB35FH65LorpLnqM0fUYCIvc3mD3yYsG"
access_token <- "584062133-RFn4eRHcomGguOOjowNfi5gzSOV2Rj1XYXjPEnCv"
access_token_secret <- "hTYHRErjjZBBx6gbZjCQGvIwPiMVNbGov9lcfIwhVN4tw"

# authenticate via web browser - don't forget to change the app!
token <- create_token(
  app = "Tweet_Gaps",
  consumer_key = api_key,
  consumer_secret = api_secret_key,
  access_token = access_token,
  access_secret = access_token_secret)

## search 30day for up to 300 rstats tweets sent before the last week
rt <-search_30day("place_country:GB",
                  n = 5000,
                  fromDate = 202103120000,
                  toDate = 202103190000,
                  env_name = "Tweets",
                  safedir = "C:/Users/b9054751/OneDrive - Newcastle University/PhD/Data/Twitter/DATA",
                  parse = TRUE,
                  token = token)

#3000 was limit - this was 
#Where are the tweets located? - making a plot of places
test= rt

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

# Store as numeric
Loc$Latitude <- as.numeric(Loc$Latitude)
Loc$Longitude <- as.numeric(Loc$Longitude)
# Keep only those tweets where geo information is available
Loc <- subset(Loc, !is.na(Latitude) & !is.na(Longitude))
 #55/3000 had location information...