# 1.1
gerber = read.csv("gerber.csv")
str(gerber)
mean(gerber$voting)

# 1.2
civicVote = gerber$civicduty[gerber$voting==1]
table(civicVote) / length(civicVote)
hawVote = gerber$hawthorne[gerber$voting==1]
table(hawVote) / length(hawVote)
selfVote = gerber$self[gerber$voting==1]
table(selfVote) / length(selfVote)
neiVote = gerber$neighbors[gerber$voting==1]
table(neiVote) / length(neiVote)

# 1.3
logModel = glm(voting ~ neighbors + self + civicduty + hawthorne, data=gerber)
summary(logModel)

# 1.4
predicts = predict(logModel)
table(gerber$voting, predicts > 0.3)
(134513+51966) / (134513+100875+56730+51966)

# 1.5
table(gerber$voting, predicts > 0.5)
235388 / (235388+108696)

# 1.6
table(gerber$voting)
library(ROCR)
predictROC = prediction(predicts, gerber$voting)
as.numeric(performance(predictROC, "auc")@y.values)

# 2.1
library(rpart)
library(rpart.plot)
CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber)
prp(CARTmodel)

# 2.2
CARTmodel2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)
prp(CARTmodel2)

# 2.3
CARTmodel3 = rpart(voting ~ civicduty + hawthorne + self + neighbors + sex, data=gerber, cp=0.0)
prp(CARTmodel3)

# 3.1
controlTree1 = rpart(voting ~ control, data=gerber, cp=0.0)
controlTree2 = rpart(voting ~ control + sex, data=gerber, cp=0.0)
prp(controlTree1, digits=6)

# 3.2
prp(controlTree2, digits=6)

# 3.3
logModel = glm(voting ~ control + sex, data=gerber, family="binomial")
summary(logModel)

# 3.4
Possibilities = data.frame(sex=c(0,0,1,1), control=c(0,1,0,1))
predict(logModel, newdata=Possibilities, type="response")
abs(0.2908065 - 0.290456)

# 3.5
logModel2 = glm(voting ~ sex + control + sex:control, data=gerber, family="binomial")
summary(logModel2)

# 3.6
predict(logModel2, newdata=Possibilities, type="response")
abs(0.2904558 - 0.290456)