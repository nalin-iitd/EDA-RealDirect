#The following code reads json data from the tweets json file saved in part1 of Problem1 in the current working directory
getwd()
json_data <- rjson::fromJSON(file="rentalTweets.json")

#converts json file data to data frame object
json_data_df <- do.call("rbind", lapply(json_data, as.data.frame))
class(tweets_df)

#converts data frame to json format
print_json <- toJSON(json_data_df)
class(tweets_json)

#prints the json read from the file
print_json