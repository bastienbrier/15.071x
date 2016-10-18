# 1.1
census = read.csv("census.csv")
library(caTools)
set.seed(2000)
split = sample.split(census$over50k, SplitRatio=0.6)
train = subset(census, split == TRUE)
test = subset(census, split == FALSE)
logModel = glm(over50k ~ ., data=train, family=binomial)
summary(logModel)

# 1.2
logPredict = predict(logModel, newdata=test, type="response")
table(test$over50k, logPredict >= 0.5)
(9051+1888) / (9051+662+1190+1888)

# 1.3
(9051+662) / (9051+662+1190+1888)

# 1.4
library(ROCR)
pred = prediction(logPredict, test$over50k)
as.numeric(performance(pred, "auc")@y.values)

# 2.1 - 2.3
library(rpart)
library(rpart.plot)
treeModel = rpart(over50k ~ ., data=train, method="class")
rpart.plot(treeModel)

# 2.4
treePredict = predict(treeModel, newdata=test, type="class")
table(test$over50k, treePredict)
(9243+1596) / (9243+470+1482+1596)

# 2.5
treePredict = predict(treeModel, newdata=test)
treePred = prediction(treePredict[,2], test$over50k)
treePerf = performance(treePred, "tpr", "fpr")
plot(treePerf)

# 2.6
as.numeric(performance(treePred, "auc")@y.values)

# 3.1
set.seed(1)
trainSmall = train[sample(nrow(train), 2000),]
library(randomForest)
set.seed(1)
forestModel = randomForest(over50k ~ ., data=trainSmall)
forestPredict = predict(forestModel, newdata=test)
table(test$over50k, forestPredict)
(9586+1093) / (9586+127+1985+1093)

# 3.2
vu = varUsed(forestModel, count=TRUE)
vusorted = sort(vu, decreasing=FALSE, index.return=TRUE)
dotchart(vusorted$x, names(forestModel$forest$xlevels[vusorted$ix]))

# 3.3
varImpPlot(forestModel)

# 4.1
set.seed(2)
library(caret)
library(e1071)
tr.Control = trainControl(method="cv", number=10)
cartGrid = expand.grid(.cp=seq(0.002,0.1,0.002))
tr = train(over50k ~ ., data=train, method="rpart", trControl=tr.Control, tuneGrid=cartGrid)
tr

# 4.2
newTreeModel = rpart(over50k ~ ., data=train, cp=0.002, method="class")
newTreePredict = predict(newTreeModel, newdata=test, type="class")
table(test$over50k, newTreePredict)
(9178+1838) / (9178+535+1240+1838)

# 4.3
prp(newTreeModel)