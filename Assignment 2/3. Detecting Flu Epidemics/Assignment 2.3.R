# 1.1
FluTrain = read.csv("FluTrain.csv")
FluTrain[which.max(FluTrain$ILI), "Week"]
FluTrain[which.max(FluTrain$Queries), "Week"]

# 1.2
hist(FluTrain$ILI)

# 1.3
plot(log(FluTrain$ILI), FluTrain$Queries)


# 2.2
FluTrend1 = lm(log(ILI) ~ Queries, data=FluTrain)
summary(FluTrend1)

# 2.3
cor(log(FluTrain$ILI), FluTrain$Queries)^2

# 3.1
FluTest = read.csv("FluTest.csv")
PredTest1 = exp(predict(FluTrend1, newdata=FluTest))
PredTest1[which(FluTest$Week == "2012-03-11 - 2012-03-17")]

# 3.2
(FluTest[which(FluTest$Week == "2012-03-11 - 2012-03-17"), "ILI"] - PredTest1[which(FluTest$Week == "2012-03-11 - 2012-03-17")])/FluTest[which(FluTest$Week == "2012-03-11 - 2012-03-17"), "ILI"]

# 3.3
SSE = sum((PredTest1 - FluTest$ILI)^2)
RMSE = sqrt(SSE/length(PredTest1))
RMSE

# 4.1
install.packages("zoo")
library(zoo)
ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)
FluTrain$ILILag2 = coredata(ILILag2)
sum(is.na(ILILag2))

# 4.2
plot(log(ILILag2), log(FluTrain$ILI))

# 4.3 - 4.4
FluTrend2 = lm(log(ILI) ~ Queries + log(ILILag2), data=FluTrain)
summary(FluTrend2)

# 5.1
ILILag2 = lag(zoo(FluTest$ILI), -2, na.pad=TRUE)
FluTest$ILILag2 = coredata(ILILag2)
sum(is.na(FluTest$ILILag2))

# 5.3
FluTest$ILILag2[1:2] = FluTrain$ILI[416:417]
FluTest$ILILag2[1]
FluTest$ILILag2[2]

# 5.4
PredTest2 = exp(predict(FluTrend2, newdata=FluTest))
SSE = sum((PredTest2 - FluTest$ILI)^2)
RMSE = sqrt(SSE/length(PredTest2))
RMSE