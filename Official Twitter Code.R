#Twitter code for different forms of collation and analysis collected from a variety of online sources, 
#example ref -  https://www.mzes.uni-mannheim.de/socialsciencedatalab/article/collecting-and-analyzing-twitter-using-r/
#https://www.earthdatascience.org/courses/earth-analytics/get-data-using-apis/use-twitter-api-r/


#Stage 1: Packages/Initial setup

install.packages("rtweet")
install.packages("ggmap")
install.packages("igraph")
install.packages("ggraph")
install.packages("tidytext")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("readr")
install.packages("askpass")

if (!requireNamespace("httpuv", quietly = TRUE)) {
  install.packages("httpuv")
}

#open library
library(rtweet)
library(ggplot2)
library(dplyr)
library(tidyr)

Path = "C:/Users/b9054751/OneDrive - Newcastle University/Location-Twitter-Data-" 

# Set Working Directory
setwd(Path)

#Stage2: setting up the API

## store api keys (Replace with project specific keys)
api_key <- "13WsdQLKogoVzTUtZ0GcTGMZs"
api_secret_key <- "hRM5zIOpHn0FTY0ilq33uxa9hisUrgdriddUqu8XZl0NMkEN43"
access_token <- "584062133-vT9vwdtiLHVyxH6QXuJXuzMHIGy6kyZMIv9ohgUy"
access_token_secret <- "ylY5um8tiJc49xdfKi5tCtfIANAEXxIXzFzRGhjuw4rbk"

## authenticate via web browser - don't forget to change the app!
token <- create_token(
  app = "Left Behind Connections", 
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
  timeout = 120,
  file_name = "test.json",
  parse = FALSE
)
test <- parse_stream("test.json")
save(test, file = "test_live.Rda")

