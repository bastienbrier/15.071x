# 1.1
tweets = read.csv("tweets.csv")
library(tm)
library(SnowballC)
corpus = Corpus(VectorSource(tweets$Tweet))
corpus = tm_map(corpus,tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
dtm = DocumentTermMatrix(corpus)
allTweets = as.data.frame(as.matrix(dtm))
ncol(allTweets)

# 2.1
library(wordcloud)
?wordcloud
colnames(allTweets)

# 2.2
head(colSums(allTweets))

# 2.3
wordcloud(colnames(allTweets), colSums(allTweets))

# 2.4
corpus = Corpus(VectorSource(tweets$Tweet))
corpus = tm_map(corpus,tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))
dtm = DocumentTermMatrix(corpus)
allTweets = as.data.frame(as.matrix(dtm))
wordcloud(colnames(allTweets), colSums(allTweets))

# 3.1
?wordcloud
negativeTweets = subset(allTweets, tweets$Avg <= -1)
wordcloud(colnames(negativeTweets), colSums(negativeTweets), min.freq=4, colors=brewer.pal(9, "Purples")[5:9])

# 3.2
wordcloud(colnames(allTweets), colSums(allTweets), rot.per=0.5)

# 3.3
wordcloud(colnames(allTweets), colSums(allTweets), min.freq=10, random.order=FALSE)

# 3.5
wordcloud(colnames(allTweets), colSums(allTweets), min.freq=10, random.order=FALSE, random.color=TRUE, colors=brewer.pal(9, "Purples")[5:9])

# 4.1
library(RColorBrewer)
display.brewer.all()

# 4.2
display.brewer.pal(7, "Greys")

# 4.3
wordcloud(colnames(negativeTweets), colSums(negativeTweets), min.freq=4, colors=brewer.pal(9, "Blues"))
wordcloud(colnames(negativeTweets), colSums(negativeTweets), min.freq=4, colors=brewer.pal(9, "Blues")[c(5,6,7,8,9)])