# 1.1
loans = read.csv("loans.csv")
table(loans$not.fully.paid)
1533 / (8045 + 1533)

# 1.2
summary(loans)

# 1.3
sum(is.na(loans))
missing = subset(loans, is.na(log.annual.inc) | is.na(days.with.cr.line) | is.na(revol.util) | is.na(inq.last.6mths) | is.na(delinq.2yrs) | is.na(pub.rec))
table(missing$not.fully.paid)
12/50

# 1.4
library(mice)
set.seed(144)
vars.for.imputation = setdiff(names(loans), "not.fully.paid")
imputed = complete(mice(loans[vars.for.imputation]))
loans[vars.for.imputation] = imputed
loans_imputed = read.csv("loans_imputed.csv")

# 2.1
set.seed(144)
library(caTools)
split = sample.split(loans_imputed$not.fully.paid, SplitRatio = 0.7)
train = subset(loans_imputed, split == TRUE)
test = subset(loans_imputed, split == FALSE)
logModel = glm(not.fully.paid ~ ., data=train, family=binomial)
summary(logModel)

# 2.2
difference = (-9.317e-03 * 700) - (-9.317e-03 * 710)
difference
ratio = exp((-9.317e-03 * 700) - (-9.317e-03 * 710))
ratio

# 2.3
predicted.risk = predict(logModel, type="response", newdata=test)
test$predicted.risk = predicted.risk
table(test$not.fully.paid, test$predicted.risk > 0.5)
(2400 + 3) / (2400 + 13 + 457 + 3) #accuracy
(2400 + 13) / (2400 + 13 + 457 + 3) #accuracy baseline model

# 2.4
library(ROCR)
ROCRpred = prediction(test$predicted.risk, test$not.fully.paid)
as.numeric(performance(ROCRpred, "auc")@y.values)

# 3.1
intModel = glm(not.fully.paid ~ int.rate, data=train, family=binomial)
summary(intModel)

# 3.2
Predict2 = predict(intModel, newdata=test, type="response")
max(Predict2)
table(test$not.fully.paid, Predict2 > 0.5)

# 3.3
ROCRpred2 = prediction(Predict2, test$not.fully.paid)
as.numeric(performance(ROCRpred2, "auc")@y.values)

# 4.1
10 * exp(6/100*3)

# 5.1
test$profit = exp(test$int.rate*3) - 1
test$profit[test$not.fully.paid == 1] = -1
max(test$profit) * 10

# 6.1
highInterest = subset(test, int.rate > 0.15)
mean(highInterest$profit)
table(highInterest$not.fully.paid)
110 / (110 + 327)

# 6.2
cutoff = sort(highInterest$predicted.risk, decreasing=FALSE)[100]
selectedLoans = subset(highInterest, predicted.risk <= cutoff)
sum(selectedLoans$profit)
table(selectedLoans$not.fully.paid)