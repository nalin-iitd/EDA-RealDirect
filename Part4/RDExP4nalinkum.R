#install the required packages
install.packages("twitteR")
install.packages("wordcloud")
install.packages("tm")
install.packages("ggplot2")
install.packages("Rgraphviz")


#load the required libraries
library("twitteR")
library("wordcloud")
library("tm")
library("ggplot2")
library("Rgraphviz")

#perform real esate tweet analysis for Real Direct
#Read twitter data in json format from a file on disk and extract the text portion
json_data1 <- readLines('Mar1001.json', warn = FALSE)
json_df <- jsonlite::fromJSON(json_data1)
tweets_text <- json_df$text

#remove unnecessary garbage charcters from tweets text
rent_tweets_text <- iconv(tweets_text, 'UTF-8', 'ASCII')

#create a corpus using tm package and remove stopwords, punctuations, whitespace and other unnecessary charcters from tweets text
rent_clean_text <- Corpus(VectorSource(rent_tweets_text))
rent_clean_text <- tm_map(rent_clean_text, content_transformer(function (x , pattern ) gsub(pattern, " ", x)), "/")
rent_clean_text <- tm_map(rent_clean_text, content_transformer(function (x , pattern ) gsub(pattern, " ", x)), "@")
rent_clean_text <- tm_map(rent_clean_text, content_transformer(function (x , pattern ) gsub(pattern, " ", x)), "\\|")
rent_clean_text <- tm_map(rent_clean_text, removePunctuation)
rent_clean_text <- tm_map(rent_clean_text, content_transformer(tolower))
rent_clean_text <- tm_map(rent_clean_text, removeWords, stopwords("english"))
rent_clean_text <- tm_map(rent_clean_text, stripWhitespace)
rent_clean_text <- tm_map(rent_clean_text, removeWords, c("http","https", "tco")) 

#construct a term document matrix and make data frame which consists of terms and their respective term frequencies
tdm <- TermDocumentMatrix(rent_clean_text)
tmatrix <- as.matrix(tdm)
term_freqs <- sort(rowSums(tmatrix),decreasing=TRUE)
term_freqs <- subset(term_freqs, term_freqs >= 100)
termfreq_df <- data.frame(term = names(term_freqs),freq=term_freqs)
termfreq_df[1:10, ]

#visualize term frequencies using a term freq ggplot and constructing a wordcloud
ggplot(termfreq_df, aes(x = term, y = freq)) + geom_bar(stat = "identity") + xlab("Terms") + ylab("Frequency") + coord_flip()
set.seed(1234)
wordcloud(words = termfreq_df$term, freq = termfreq_df$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#use some functions such as findAssociations and findFreqTerms for analysis of most frequent terms
findAssocs(tdm, "rent", 0.2)
frequent_terms <- findFreqTerms(tdm, lowfreq = 25)
print(frequent_terms)


#construct a cluster dendrogram to perform a final correlation among most frequent terms at this stage
#remove sparse terms
tdm2 <- removeSparseTerms(tdm, sparse = 0.95)
tmatrix2 <- as.matrix(tdm2)
#cluster terms
distMatrix <- dist(scale(tmatrix2))
fit <- hclust(distMatrix, method = "ward.D2")
plot(fit)
rect.hclust(fit, k=6) #cut tree into 6 clusters

