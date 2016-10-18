# 1.1
letters = read.csv("letters_ABPR.csv")
letters$isB = as.factor(letters$letter == "B")
library(caTools)
set.seed(1000)
split = sample.split(letters$isB, SplitRatio=0.5)
train = subset(letters, split == TRUE)
test = subset(letters, split == FALSE)
table(letters$isB)
2350 / (2350+766)

# 1.2
library(rpart)
library(rpart.plot)
CARTb = rpart(isB ~ . - letter, data=train, method="class")
predictTest = predict(CARTb, newdata=test, type="class")
table(test$isB, predictTest)
(1118+340) / (1118+57+43+340)

# 1.3
library(randomForest)
set.seed(1000)
forestModel = randomForest(isB ~ . - letter, data=train)
predictForest = predict(forestModel, newdata=test)
table(test$isB, predictForest)
(1165+374) / (1165+10+9+374)

# 2.1
letters$letter = as.factor(letters$letter)
set.seed(2000)
split = sample.split(letters$letter, SplitRatio=0.5)
train = subset(letters, split == TRUE)
test = subset(letters, split == FALSE)
table(test$letter)
401 / (395+383+401+379)

# 2.2
treeModel = rpart(letter ~ . - isB, data=train, method="class")
treePredict = predict(treeModel, newdata=test, type="class")
table(test$letter, treePredict)
(348+318+363+340) / nrow(test)

# 2.3
set.seed(1000)
forestModel = randomForest(letter ~ . - isB, data=train)
forestPredict = predict(forestModel, newdata=test)
table(test$letter, forestPredict)
(390+380+393+364) / nrow(test)