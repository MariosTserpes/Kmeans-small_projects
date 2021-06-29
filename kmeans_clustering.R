      ####Tserpes Marios####
###Research Methods in Data Science###
      ####Assignment 3####


#structure of dataset
View(USArrests)
?USArrests
str(USArrests)
sapply(USArrests, class)
sprintf('No. of rows : %d. No. of columns : %s',
        nrow(USArrests), ncol(USArrests))

summary(USArrests)
##################
####QUESTION i####
##################
set.seed(2)
km.out <- kmeans(USArrests, 3, nstart = 20)
km.out
km.out$cluster


#subset with states in cluster 1
states.cluster1 <- subset(USArrests, km.out$cluster == 1)
states.cluster1

#subset with states in cluster 2
states.cluster2 <- subset(USArrests, km.out$cluster == 2)
states.cluster2

# subset with states in cluster 3
states.cluster3 <- subset(USArrests, km.out$cluster == 3)
states.cluster3

#Value counts per cluster
sprintf('Cluster one(1) : %d. Cluster two(2) : %s. Cluster three(3) : %d', 
        nrow(states.cluster1), nrow(states.cluster2), nrow(states.cluster3))

#Some insights about states in 3 clusters
summary(states.cluster1)
summary(states.cluster2)
summary(states.cluster3)


###################
####QUESTION ii####
###################

#2Dimensional visualization
plot(USArrests, col = (km.out$cluster + 1), 
     main = "K-Means Clustering with K=3", 
      pch = 20, cex = 2)


###################
####QUESTION iii###
###################

#Repeat the above process by scaling the data.
attach(USArrests)
df.scaled <- data.frame(scale(USArrests)) #scale the data
head(df.scaled, n = 3) #View the first 3 rows

#data distribution BEFORE scaling
par(mfrow = c(2, 2))
hist(USArrests$Murder, main = 'Murder arrests',
     xlab = "Murder")
hist(USArrests$Assault, main = 'Assault arrests',
     xlab = "Assault")
hist(USArrests$Rape, main = 'Rape arrests',
     xlab = "Rape")
hist(USArrests$UrbanPop, main = 'Percent Urban Pop',
     xlab = "UrbanPop")

#data distribution AFTER scaling
par(mfrow = c(2, 2))
hist(df.scaled$Murder, main = 'Murder arrests',
     xlab = "Murder")
hist(df.scaled$Assault, main = 'Assault arrests',
     xlab = "Assault")
hist(df.scaled$Rape, main = 'Rape arrests',
     xlab = "Rape")
hist(df.scaled$UrbanPop, main = 'Percent Urban Pop',
     xlab = "UrbanPop")
summary(df.scaled) #mean equals to zero

#K-means clustering with scaled data
set.seed(4)
km.out2 <- kmeans(df.scaled, 3)
km.out2
km.out2$cluster

#subset with states in cluster 1 based on scaled data
df.scaled.cluster1 <- subset(df.scaled, km.out2$cluster == 1)
df.scaled.cluster1

#subset with states in cluster 2
df.scaled.cluster2 <- subset(df.scaled, km.out2$cluster == 2)
df.scaled.cluster2

# subset with states in cluster 3
df.scaled.cluster3 <- subset(df.scaled, km.out2$cluster == 3)
df.scaled.cluster3

#Value counts per cluster
sprintf('Cluster one(1) : %d. Cluster two(2) : %s. Cluster three(3) : %d', 
        nrow(df.scaled.cluster1), nrow(df.scaled.cluster2), nrow(df.scaled.cluster3))

#Some insights about states in 3 clusters
summary(df.scaled.cluster1)
summary(df.scaled.cluster2)
summary(df.scaled.cluster3)

plot(df.scaled, col = (km.out2$cluster + 1), 
     main = "K-Means Clustering with K=3 after scaling", 
     pch = 18, cex = 0.8)



###################
####QUESTION iv###
###################

#Repeating the process with different number of initializations
#number of initialization == 1
set.seed(6)
km.out3 <- kmeans(USArrests, 3, nstart = 1)
km.out3
km.out$cluster
km.out3$tot.withinss
km.out3$betweenss
km.out3$centers
km.out3$size
mean(km.out3$withinss)

plot(USArrests, col = (km.out3$cluster + 1), 
     main = "K-Means Clustering with K=3", 
     pch = 20, cex = 2)


#number of initializations == 10
set.seed(8)
km.out4 <- kmeans(USArrests, 3, nstart = 10)
km.out4
km.out4$cluster
km.out4$tot.withinss
km.out4$betweenss
km.out4$centers
km.out4$size
mean(km.out4$withinss)

plot(USArrests, col = (km.out4$cluster + 1), 
     main = "K-Means Clustering with K=3", 
     pch = 18, cex = 2)

#number of initializations == 50
set.seed(10)
km.out5 <- kmeans(USArrests, 3, nstart = 50)
km.out5
km.out5$cluster
km.out5$tot.withinss
km.out5$betweenss
km.out5$centers
km.out5$size
mean(km.out5$withinss)

plot(USArrests, col = (km.out5$cluster + 1), 
     main = "K-Means Clustering with K=3", 
     pch = 20, cex = 2)

#a data frame with cluster vectors with different initializations
clusters.df <- data.frame(km.out3$cluster, km.out4$cluster, km.out5$cluster)
clusters.df


#different number of initializations in SCALED data
#initilization == 1
set.seed(12)
km.out6 <- kmeans(df.scaled, 3, nstart = 1)
km.out6
km.out6$cluster
km.out6$tot.withinss
km.out6$betweenss
km.out6$centers
km.out6$size
mean(km.out6$withinss)

#initilization == 10
set.seed(14)
km.out7 <- kmeans(df.scaled, 3, nstart = 10)
km.out7
km.out7$cluster
km.out7$tot.withinss
km.out7$betweenss
km.out7$centers
km.out7$size
mean(km.out7$withinss)

#initializations == 50
set.seed(16)
km.out8 <- kmeans(df.scaled, 3, nstart = 50)
km.out8
km.out8$cluster
km.out8$tot.withinss
km.out8$betweenss
km.out8$centers
km.out8$size
mean(km.out8$withinss)

