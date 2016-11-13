#The following packages are installed in the first step
install.packages("bitops")
install.packages("rjson")
install.packages("RCurl") 
install.packages("RJSONIO") 
install.packages("twitteR") 
install.packages("ROAuth")
install.packages("jsonlite")

#The following packages are loaded in session
library(bitops)
library(rjson)
library(RCurl)
library(RJSONIO)
library(twitteR)
library(ROAuth)
library(jsonlite)

#The following keys and secret tokens authorises the user to collect tweets using twitter API
consumer_key <- "RQGcRA27gAH0IXWDiOFCMQ9kp"
consumer_secret <- "hZGnJIRsYmma2QKqxmrEfiQ3snV3zQZd3daZCJOcf93YLR3hsV"
access_token <- "2466308148-BtSYKkgxcXG812eExHJwerlReWYJRZiQSsKDrbb"
access_secret <- "nVFW4iQ42KfHPP0nm45rS5fauaGsxj7IXMiNO65lxWQdC"

# The following script is used to create Twitter Connection
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

# The following command searches for tweets based on specific keywords from twitter API
search_tweets <- searchTwitter("rent apartment new york", lang="en", n=100)
class(search_tweets)

# The following command converts object returned by twitter API into a data frame object
tweets_df <- do.call("rbind", lapply(search_tweets, as.data.frame))
class(tweets_df)

# The toJSON() method from jsonlite library converts data frame object into a json object
tweets_json <- toJSON(tweets_df)
tweets_json <- jsonlite::prettify(tweets_json, indent = 4)
class(tweets_json)

#prints tweets json
tweets_json

# The following command writes the json data into a specific file
write(tweets_json, "rentalTweets.json")

# The above json file is saved into the following directory
getwd()