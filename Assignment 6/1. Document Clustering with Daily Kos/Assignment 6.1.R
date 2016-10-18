# 1.1
dailyKos = read.csv("dailykos.csv")
distance = dist(dailyKos, method="euclidean")
clusterIntensity = hclust(distance, method="ward.D")

# 1.2
plot(clusterIntensity)

# 1.4
clusters = cutree(clusterIntensity, k=7)
cluster1 = subset(dailyKos, clusters == 1)
cluster2 = subset(dailyKos, clusters == 2)
cluster3 = subset(dailyKos, clusters == 3)
cluster4 = subset(dailyKos, clusters == 4)
cluster5 = subset(dailyKos, clusters == 5)
cluster6 = subset(dailyKos, clusters == 6)
cluster7 = subset(dailyKos, clusters == 7)
table(clusters)

# 1.5
tail(sort(colMeans(cluster1)))

# 1.6
tail(sort(colMeans(cluster2)))
tail(sort(colMeans(cluster3)))
tail(sort(colMeans(cluster4)))
tail(sort(colMeans(cluster5)))
tail(sort(colMeans(cluster6)))
tail(sort(colMeans(cluster7)))

# 2.1
set.seed(1000)
KMC = kmeans(dailyKos, 7)
dkCluster = KMC$cluster
table(dkCluster)

# 2.2
dkCluster1 = subset(dailyKos, dkCluster == 1)
dkCluster2 = subset(dailyKos, dkCluster == 2)
dkCluster3 = subset(dailyKos, dkCluster == 3)
dkCluster4 = subset(dailyKos, dkCluster == 4)
dkCluster5 = subset(dailyKos, dkCluster == 5)
dkCluster6 = subset(dailyKos, dkCluster == 6)
dkCluster7 = subset(dailyKos, dkCluster == 7)
tail(sort(colMeans(dkCluster1)))
tail(sort(colMeans(dkCluster2)))
tail(sort(colMeans(dkCluster3)))
tail(sort(colMeans(dkCluster4)))
tail(sort(colMeans(dkCluster5)))
tail(sort(colMeans(dkCluster6)))
tail(sort(colMeans(dkCluster7)))

# 2.3 - 2.6
table(clusters, dkCluster)