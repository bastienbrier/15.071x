# 1.1
stocks = read.csv("StocksCluster.csv")
str(stocks)

# 1.2
table(stocks$PositiveDec>0)
6324 / (5256+6324)

# 1.3
sort(cor(stocks))

# 1.4
summary(stocks)

# 2.1
library(caTools)
set.seed(144)
spl = sample.split(stocks$PositiveDec, SplitRatio = 0.7)
stocksTrain = subset(stocks, spl == TRUE)
stocksTest = subset(stocks, spl == FALSE)
StocksModel = glm(PositiveDec ~ ., data=stocksTrain, family="binomial")
predTrain = predict(StocksModel, data=stocksTrain, type="response")
table(stocksTrain$PositiveDec, predTrain > 0.5)
(990+3640) / (990+2689+787+3640)

# 2.2
predTest = predict(StocksModel, newdata=stocksTest, type="response")
table(stocksTest$PositiveDec, predTest > 0.5)
(417+1553) / (417+1160+344+1553)

# 2.3
table(stocksTest$PositiveDec)
1897 / (1577+1897)

# 3.1
limitedTrain = stocksTrain
limitedTrain$PositiveDec = NULL
limitedTest = stocksTest
limitedTest$PositiveDec = NULL

# 3.2
library(caret)
preproc = preProcess(limitedTrain)
normTrain = predict(preproc, limitedTrain)
normTest = predict(preproc, limitedTest)
mean(normTrain$ReturnJan)
mean(normTest$ReturnJan)

# 3.4
set.seed(144)
km = kmeans(normTrain, centers = 3)
stockCluster = km$cluster
table(stockCluster)

# 3.5
library(flexclust)
km.kcca = as.kcca(km, normTrain)
clusterTrain = predict(km.kcca)
clusterTest = predict(km.kcca, newdata=normTest)
table(clusterTest)

# 4.1
stocksTrain1 = subset(stocksTrain, stockCluster == 1)
stocksTrain2 = subset(stocksTrain, stockCluster == 2)
stocksTrain3 = subset(stocksTrain, stockCluster == 3)
stocksTest1 = subset(stocksTest, clusterTest == 1)
stocksTest2 = subset(stocksTest, clusterTest == 2)
stocksTest3 = subset(stocksTest, clusterTest == 3)
mean(stocksTrain1$PositiveDec)
mean(stocksTrain2$PositiveDec)
mean(stocksTrain3$PositiveDec)

# 4.2
StocksModel1 = glm(PositiveDec ~ ., data=stocksTrain1, family="binomial")
StocksModel2 = glm(PositiveDec ~ ., data=stocksTrain2, family="binomial")
StocksModel3 = glm(PositiveDec ~ ., data=stocksTrain3, family="binomial")
summary(StocksModel1)
summary(StocksModel2)
summary(StocksModel3)

# 4.3
PredictTest1 = predict(StocksModel1, newdata=stocksTest1, type="response")
PredictTest2 = predict(StocksModel2, newdata=stocksTest2, type="response")
PredictTest3 = predict(StocksModel3, newdata=stocksTest3, type="response")
table(stocksTest1$PositiveDec, PredictTest1 > 0.5)
(30+774) / (30+471+23+774)
table(stocksTest2$PositiveDec, PredictTest2 > 0.5)
(388+757) / (388+626+309+757)
table(stocksTest3$PositiveDec, PredictTest3 > 0.5)
(49+13) / (49+13+21+13)

# 4.4
AllPredictions = c(PredictTest1, PredictTest2, PredictTest3)
AllOutcomes = c(stocksTest1$PositiveDec, stocksTest2$PositiveDec, stocksTest3$PositiveDec)
table(AllOutcomes, AllPredictions > 0.5)
(467+1544) / (467+1110+353+1544)