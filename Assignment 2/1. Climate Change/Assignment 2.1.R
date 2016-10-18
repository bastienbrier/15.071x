# 1.1 - 1.2
climate = read.csv("climate_change.csv")
trainingSet = subset(climate, climate$Year < 2007)
testSet = subset(climate, climate$Year > 2006)
model = lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data=trainingSet)
summary(model)

# 2.1 - 2.2
cor(trainingSet)

# 3
model2 = lm(Temp ~ N2O + MEI + TSI + Aerosols, data=trainingSet)
summary(model2)

# 4
newModel = step(model)
summary(newModel)

# 5
predictTest = predict(newModel, newdata = testSet)
SSE = sum((testSet$Temp - predictTest)^2)
SST = sum((testSet$Temp - mean(trainingSet$Temp, na.rm=TRUE))^2)
Rsquared = 1 - SSE/SST
Rsquared