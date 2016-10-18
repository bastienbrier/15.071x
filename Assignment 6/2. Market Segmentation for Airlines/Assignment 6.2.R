# 1.1
airlines = read.csv("AirlinesCluster.csv")
summary(airlines)

# 1.3
library(caret)
preproc = preProcess(airlines)
airlinesNorm = predict(preproc, airlines)
summary(airlinesNorm)

# 2.1
airlineDist = dist(airlinesNorm, method="euclidean")
airlineClust = hclust(airlineDist, method="ward.D")
plot(airlineClust)

# 2.2
airlineGroups = cutree(airlineClust, k=5)
table(airlineGroups)

# 2.3 - 2.7
tapply(airlines$Balance, airlineGroups, mean)
tapply(airlines$QualMiles, airlineGroups, mean)
tapply(airlines$BonusMiles, airlineGroups, mean)
tapply(airlines$BonusTrans, airlineGroups, mean)
tapply(airlines$FlightMiles, airlineGroups, mean)
tapply(airlines$FlightTrans, airlineGroups, mean)
tapply(airlines$DaysSinceEnroll, airlineGroups, mean)

# 3.1
set.seed(88)
airlineKmeans = kmeans(airlinesNorm, centers = 5, iter.max = 1000)
airlineKCluster = airlineKmeans$cluster
table(airlineKCluster)

# 3.2
tapply(airlines$Balance, airlineKCluster, mean)
tapply(airlines$QualMiles, airlineKCluster, mean)
tapply(airlines$BonusMiles, airlineKCluster, mean)
tapply(airlines$BonusTrans, airlineKCluster, mean)
tapply(airlines$FlightMiles, airlineKCluster, mean)
tapply(airlines$FlightTrans, airlineKCluster, mean)
tapply(airlines$DaysSinceEnroll, airlineKCluster, mean)
airlineKmeans$centers