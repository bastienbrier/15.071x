# 1.1
trainingSet = read.csv("pisa2009train.csv")
testSet = read.csv("pisa2009test.csv")
str(trainingSet)

# 1.2
tapply(trainingSet$readingScore, trainingSet$male, mean)

# 1.3
summary(trainingSet)

# 1.4
pisaTrain = na.omit(trainingSet)
pisaTest = na.omit(testSet)
str(pisaTrain)
str(pisaTest)

# 3.1
pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")
lmScore = lm(readingScore ~ ., data=pisaTrain)
summary(lmScore)

# 3.2
RMSE = sqrt(mean(lmScore$residuals^2))
RMSE

# 3.3
summary(lmScore)

# 4.1
predTest = predict(lmScore, newdata = pisaTest)
summary(predTest)

# 4.2
SSE = sum((predTest - pisaTest$readingScore)^2)
RMSE = sqrt(sum(SSE)/length(predTest))
SSE
RMSE

# 4.3
baseline = mean(pisaTrain$readingScore)
SST = sum((pisaTest$readingScore - baseline)^2)
baseline
SST

# 4.4
Rsquared = 1 - SSE/SST
Rsquared