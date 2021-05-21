#https://cran.r-project.org/web/packages/rtweet/vignettes/stream.html

#1) Import packages - only rtweet needed at this stage
library(rtweet)

#2) Set Path
Path = getwd()

#3) Get twitter keys from seperate (private) R file (see ReadMe introduction)
source("Twitter_Keys.R")

#4) Set parameters
filename <- "LB_Tweets.json" #File to store tweets - this can be accessed in another R window during collection
## Stream location used to filter tweets
Bounding_box <- c(-10.79,49.8,1.99,58.86) #location bounding box for where tweets can be collected from - this has been set to the UK
## Stream time in seconds so for one minute set timeout = 60
## For larger chunks of time, I recommend multiplying 60 by the number
## of desired minutes. This method scales up to hours as well
## (x * 60 = x mins, x * 60 * 60 = x hours

Ten_Seconds = 10
Five_Minutes = 60L * 5
One_Day = 60L * 60L * 24L
One_Week = 60L * 60L * 24L* 7L
Two_WeekS = 60L * 60L * 24L *7L * 2L
Four_WeekS = 60L * 60L * 24L *7L * 4L

#5) stream_tweets
stream_tweets(
  q = Bounding_box,
  parse = FALSE,
  timeout = Five_Minutes,
  file_name = filename,
)

## Parse from json file
rt <- parse_stream("LB_Tweets.json")
