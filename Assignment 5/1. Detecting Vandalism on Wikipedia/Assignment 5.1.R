# 1.1
wiki = read.csv("wiki.csv", stringsAsFactors = FALSE)
wiki$Vandal = as.factor(wiki$Vandal)
sum(wiki$Vandal==1)

# 1.2
library(tm)
corpusAdded = Corpus(VectorSource(wiki$Added))
corpusAdded = tm_map(corpusAdded, removeWords, stopwords("english"))
corpusAdded = tm_map(corpusAdded, stemDocument)
dtmAdded = DocumentTermMatrix(corpusAdded)
str(dtmAdded)
dtmAdded

# 1.3
sparseAdded = removeSparseTerms(dtmAdded, 0.997)
sparseAdded

# 1.4
wordsAdded = as.data.frame(as.matrix(sparseAdded))
colnames(wordsAdded) = paste("A", colnames(wordsAdded))
corpusRemoved = Corpus(VectorSource(wiki$Remove))
corpusRemoved = tm_map(corpusRemoved, removeWords, stopwords("english"))
corpusRemoved = tm_map(corpusRemoved, stemDocument)
dtmRemoved = DocumentTermMatrix(corpusRemoved)
sparseRemoved = removeSparseTerms(dtmRemoved, 0.997)
wordsRemoved = as.data.frame(as.matrix(sparseRemoved))
colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))
ncol(wordsRemoved)

# 1.5
wikiWords = cbind(wordsAdded, wordsRemoved)
wikiWords$Vandal = wiki$Vandal
set.seed(123)
library(caTools)
split = sample.split(wikiWords$Vandal, SplitRatio = 0.7)
train = subset(wikiWords, split == TRUE)
test = subset(wikiWords, split == FALSE)
table(test$Vandal)
618 / (618+545)

# 1.6
library(rpart)
library(rpart.plot)
treeModel = rpart(Vandal ~ ., data=train, method="class")
treePredict = predict(treeModel, newdata=test)
table(test$Vandal, treePredict[,2] >= 0.5)
(618+12) / (618+533+12)

# 1.7
prp(treeModel)

# 2.1
wikiWords2 = wikiWords
wikiWords2$HTTP = ifelse(grepl("http", wiki$Added, fixed=TRUE), 1, 0)
sum(wikiWords2$HTTP)

# 2.2
wikiTrain2 = subset(wikiWords2, split == TRUE)
wikiTest2 = subset(wikiWords2, split == FALSE)
treeModel2 = rpart(Vandal ~ ., data=wikiTrain2, method="class")
treePredict2 = predict(treeModel2, newdata=wikiTest2)
table(wikiTest2$Vandal, treePredict2[,2] >= 0.5)
(609+57) / (609+9+488+57)

# 2.3
wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))
mean(wikiWords2$NumWordsAdded)

# 2.4
wikiTrain3 = subset(wikiWords2, split == TRUE)
wikiTest3 = subset(wikiWords2, split == FALSE)
treeModel3 = rpart(Vandal ~ ., data=wikiTrain3, method="class")
treePredict3 = predict(treeModel3, newdata=wikiTest3)
table(wikiTest3$Vandal, treePredict3[,2] >= 0.5)
(514+248) / (514+104+297+248)

# 3.1
wikiWords3 = wikiWords2
wikiWords3$Minor = wiki$Minor
wikiWords3$Loggedin = wiki$Loggedin
wikiTrain4 = subset(wikiWords3, split == TRUE)
wikiTest4 = subset(wikiWords3, split == FALSE)
treeModel4 = rpart(Vandal ~ ., data=wikiTrain4, method="class")
treePredict4 = predict(treeModel4, newdata=wikiTest4)
table(wikiTest4$Vandal, treePredict4[,2] >= 0.5)
(595+241) / (595+23+304+241)

# 3.2
prp(treeModel4)