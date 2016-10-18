# 1.1
parole = read.csv("parole.csv")
str(parole)

# 1.2
sum(parole$violator == 1)

# 1.3
parole$state = as.factor(parole$state)
parole$crime = as.factor(parole$crime)

# 3.1 - 3.2
set.seed(144)
library(caTools)
split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split == TRUE)
test = subset(parole, split == FALSE)

# 4.1
Model1 = glm(violator ~ ., data=train, family="binomial")
summary(Model1)

# 4.3
exampleMan = data.frame(male = 1, race = 1, age = 50, state = as.factor(1), time.served = 3, max.sentence = 12, multiple.offenses = 0, crime = as.factor(2))
probability = predict(Model1, type="response", newdata=exampleMan)
odds = probability / (1 - probability)
odds
probability

# 5.1
predictedProb = predict(Model1, type="response", newdata=test)
max(predictedProb)

# 5.2
table(test$violator, predictedProb > 0.5)
12 / (12 + 11) #sensitivity
167 / (167 + 12) #specificity
(167 + 12) / (167 + 12 + 12 + 11) #accuracy

# 5.3
(167 + 12) / (167 + 12 + 12 + 11)

# 5.6
library(ROCR)
ROCRpred = prediction(predictedProb, test$violator)
as.numeric(performance(ROCRpred, "auc")@y.values)