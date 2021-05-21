#https://cran.r-project.org/web/packages/rtweet/vignettes/stream.html
library(rtweet)

Path = getwd()

#1) 
source("Twitter_Keys.R")

#Store api keys (Replace with project specific keys, these won't work!)
#api_key <- "p384T1Ddaet8KSel3VfuKE4xq"
#api_secret_key <- "Bvaw3ztBETvJU3EfLTK788mVeom98jg2sx7w5eARSIw64qLb3j"
#access_token <- "584062133-bEjxU7NHZYI3ufUy7yNDX6Fphj7hOoTVOFzZifsT"
#access_token_secret <- "uIV2kma0a5EnZKxfkavIObi5LaCMp7QuulbNXopSOpIS6"

# authenticate via web browser - don't forget to change the app!
#token <- create_token(
#  app = "Tweet_Gaps",
#  consumer_key = api_key,
#  consumer_secret = api_secret_key,
#  access_token = access_token,
# access_secret = access_token_secret)

filename <- "LB_Tweets.json" #File to store tweets - this can be accessed in another R window during collection

## Stream location used to filter tweets
Bounding_box <- c(-10.79,49.8,1.99,58.86) #location bounding box for where tweets can be collected from - this has been set to the UK

## Stream time in seconds so for one minute set timeout = 60
## For larger chunks of time, I recommend multiplying 60 by the number
## of desired minutes. This method scales up to hours as well
## (x * 60 = x mins, x * 60 * 60 = x hours)
## Stream for 30 minutes

Ten_Seconds = 10
Five_Minutes = 60L * 5
One_Day = 60L * 60L * 24L
One_Week = 60L * 60L * 24L* 7L
Two_WeekS = 60L * 60L * 24L *7L * 2L
Four_WeekS = 60L * 60L * 24L *7L * 4L

## stream_tweets

stream_tweets(
  q = Bounding_box,
  parse = FALSE,
  timeout = Five_Minutes,
  file_name = filename,
)

## Parse from json file
rt <- parse_stream("LB_Tweets.json")
