#Identifying different Twitter appoaches to larger chunks of data

#For all approaches - need the packages and API Key

#Packages
library(rtweet)
library(dplyr)
library(httr)


# store api keys (Replace with project specific keys)
account_app = "Tweet_Gaps"
api_key <- "p384T1Ddaet8KSel3VfuKE4xq"
api_secret_key <- "Bvaw3ztBETvJU3EfLTK788mVeom98jg2sx7w5eARSIw64qLb3j"
access_token <- "584062133-bEjxU7NHZYI3ufUy7yNDX6Fphj7hOoTVOFzZifsT"
access_token_secret <- "uIV2kma0a5EnZKxfkavIObi5LaCMp7QuulbNXopSOpIS6"

# authenticate via web browser
token <- create_token(
  app = account_app,
  consumer_key = api_key,
  consumer_secret = api_secret_key,
  access_token = access_token,
  access_secret = access_token_secret)

#Option 1 - academic track access to history

## search 30day for up to 300 rstats tweets sent before the last week
rt <-search_30day("place_country:GB",
                  n = 100,  #How many tweets? - limit in a request appears to be 3000
                  fromDate = 202103120000, #Earliest
                  toDate = 202103182159,#latest - tweets are collected reverse chronologically
                  env_name = "Tweets", #check twitter for development environment
                  safedir = "C:/Users/b9054751/OneDrive - Newcastle University/Location-Twitter-Data-/Twitter_Data", #saves the back up data
                  parse = TRUE, #Turns it into a dataframe
                  token = token)

#When this has been done, I prefer to process as a dataframe

#Dataframe and save as CSV file
Twitter_Run_1 = rt
Twitter_Run_1 = data.frame(Twitter_Run_1)
write_as_csv(Twitter_Run_1, "Twitter_Run_1.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")

#The same can be done with a full archive search.
rt = search_fullarchive("place_country:GB",
                        n = 500,
                        fromDate = 202103120000,
                        toDate = 202103182159,
                        env_name = "Tweets",
                        safedir = "C:/Users/b9054751/OneDrive - Newcastle University/Location-Twitter-Data-/Twitter_Data",
                        parse = TRUE,
                        token = token)

#Reflections - flawed, as these are collected in chunks, maximum seems to be 3000, and it takes a long time to only get an hour of data (in my case)
#Limited by subscription limits.
#To use all data collected, read in each csv file, combine and remove duplicates - duplicates may occur as the collection will need to be run multiple times!
#Combine all collection phases
Tweets = rbind(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12)
#clean up any duplicate tweets from the data frame using #dplyr::distinct
Tweets = dplyr::distinct(Tweets)


