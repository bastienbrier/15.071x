# 1.1
emails = read.csv("emails.csv", stringsAsFactors = FALSE)
str(emails)

# 1.2
table(emails$spam)

# 1.5
max(nchar(emails$text))

# 1.6
which.min(nchar(emails$text))

# 2.1
library(tm)
corpus = Corpus(VectorSource(emails$text))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)
dtm = DocumentTermMatrix(corpus)
dtm

# 2.2
spdtm = dtm
spdtm = removeSparseTerms(spdtm, 0.95)
spdtm = as.data.frame(as.matrix(spdtm))
str(spdtm)

# 2.3
emailsSparse = spdtm
colnames(emailsSparse) = make.names(colnames(emailsSparse))
which.max(colSums(emailsSparse))

# 2.4
emailsSparse$spam = emails$spam
ham = subset(emailsSparse, spam == 0)
sum(colSums(ham) >= 5000)

# 2.5
table(colSums(emailsSparse[emailsSparse$spam==1,names(emailsSparse) !="spam"]) >= 1000)

# 3.1
emailsSparse$spam = as.factor(emailsSparse$spam)
library(caTools)
set.seed(123)
spl = sample.split(emailsSparse$spam, SplitRatio = 0.7)
train = subset(emailsSparse, spl == TRUE)
test = subset(emailsSparse, spl == FALSE)
spamLog = glm(spam ~ ., data=train, family="binomial")
library(rpart)
library(rpart.plot)
spamCART = rpart(spam ~ ., data=train, method="class")
library(randomForest)
set.seed(123)
spamRF = randomForest(spam ~ ., data=train)
predTrainLog = predict(spamLog, type="response")
predTrainCART = predict(spamCART)[,2]
predTrainRF = predict(spamRF, type="prob")[,2]
table(predTrainLog < 0.00001)
table(predTrainLog > 0.99999)
table(predTrainLog >= 0.00001 & predTrainLog <= 0.99999)

# 3.2
summary(spamLog)

# 3.3
prp(spamCART)

# 3.4
table(train$spam, predTrainLog >= 0.5)
(3052+954) / (3052+4+954)

# 3.5
library(ROCR)
logROCRpred = prediction(predTrainLog, train$spam)
as.numeric(performance(logROCRpred, "auc")@y.values)

# 3.6
table(train$spam, predTrainCART >= 0.5)
(2885+894) / (2885+167+64+894)

# 3.7
predCARTROCR = prediction(predTrainCART, train$spam)
as.numeric(performance(predCARTROCR, "auc")@y.values)

# 3.8
table(train$spam, predTrainRF >= 0.5)
(3013+914) / (3013+39+44+914)

# 3.9
predRFROCR = prediction(predTrainRF, train$spam)
as.numeric(performance(predRFROCR, "auc")@y.values)

# 4.1
predTestLog = predict(spamLog, newdata=test, type="response")
predTestCART = predict(spamCART, newdata=test)[,2]
predTestRF = predict(spamRF, newdata=test, type="prob")[,2]
table(test$spam, predTestLog >= 0.5)
(1257+376) / (1257+51+34+376)

# 4.2
predictionLogROCR = prediction(predTestLog, test$spam)
as.numeric(performance(predictionLogROCR, "auc")@y.values)

# 4.3
table(test$spam, predTestCART >= 0.5)
(1228+386) / (1228+80+24+386)

# 4.4
predictionCARTROCR = prediction(predTestCART, test$spam)
as.numeric(performance(predictionCARTROCR, "auc")@y.values)

# 4.5
table(test$spam, predTestRF >= 0.5)
(1290+386) / (1290+18+24+386)

# 4.6
predictionRFROCR = prediction(predTestRF, test$spam)
as.numeric(performance(predictionRFROCR, "auc")@y.values)