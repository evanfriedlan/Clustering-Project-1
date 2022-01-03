path = 'marketing_campaign.csv'
marketing = read.csv(path,sep="\t")

ind = c(1,3,4,6,7,8,21:29)

## this new data frame keeps only numerical columns
marketing = marketing[,-ind]
## removes the missing income rows
marketing = marketing[!is.na(marketing$Income),]


marketing.scaled = marketing

## scaling the columns based on standardization
## standardization is where we subtract by the mean and divide by the standard
## deviation
for (col in colnames(marketing.scaled)){
  x = marketing.scaled[,col]
  avg = mean(x)
  sd = sd(x)
  marketing.scaled[,col] = (x - avg)/sd
}

library(cluster)
library(factoextra)

## selecting optimal number of clusters
par(mfrow=c(1,2))
fviz_nbclust(marketing.scaled,kmeans,method="silhouette",k.max=10,n=5)
fviz_nbclust(marketing.scaled,kmeans,method="wss",k.max=10,n=5)

set.seed(425)

k = kmeans(x=marketing.scaled,centers=2,n=10)

k$centers

## check accuracy 
R.squared = 1 - k$tot.withinss/k$totss
R.squared


## cluster visualization
clusplot(marketing.scaled,k$cluster,color=T)

fviz_cluster(k, data = marketing,
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)

## outlier detection for later cleaning/refining of dataset
distances <- sqrt(rowSums((marketing.scaled - k$centers)^2))
outliers <- order(distances, decreasing=T)[1:5]
print(marketing.scaled[outliers,])
print(marketing[outliers,])

## Silhouette plot to confirm optimal clustering
windows()

d = dist(marketing.scaled)
plot(silhouette(k$cluster,d))

mean(silhouette(k$cluster,d)[,3])

marketing$cluster = k$cluster

clust1 = marketing[marketing$cluster==1,]
clust2 = marketing[marketing$cluster==2,]

## Plot average and total values for each cluster
par(mfrow=c(3,5))

for (colname in colnames(marketing)){
  print(colname)
  colaverages = c(mean(clust1[,colname]),mean(clust2[,colname]))
  xx = barplot(colaverages,names.arg=c("1","2"), col=c("blue","cyan"),main = colname)
  text(x = xx, y = round(colaverages), label = round(colaverages), pos = 3, cex = 0.8, col = "black")
}

for (colname in colnames(marketing)){
  print(colname)
  colsums = c(sum(clust1[,colname]),sum(clust2[,colname]))
  xx = barplot(colsums,names.arg=c("1","2"), col=c("blue","magenta"),main = colname)
  text(x = xx, y = round(colsums), label = round(colsums), pos = 3, cex = 0.8, col = "black")
}

## Purchasing Channels
par(mfrow=c(1,2))
pie(colMeans(clust1[,c(4:9)]), col = rainbow(length(4:9)), main="cluster 1")
pie(colMeans(clust2[,c(4:9)]), col = rainbow(length(4:9)), main="cluster 2")

## Sales by Product Category
pie(colMeans(clust1[,c(11:13)]),col=rainbow(3),main="cluster 1")
pie(colMeans(clust2[,c(11:13)]),col=rainbow(3),main="cluster 2")

