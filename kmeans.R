install.packages("ClusterR")
install.packages("cluster")

library(ClusterR)
library(cluster)

data(iris)

str(iris)

iris_df<-iris[,-5]

set.seed(240)
kmodel=kmeans(iris_df, centers = 3, nstart = 20)
kmodel

kmodel$cluster

cf_matrix=table(iris$Species, kmodel$cluster)
cf_matrix

#plot, centers, clusplot

head(iris_df)

plot(iris_df[c("Sepal.Length","Sepal.Width")],
     col= kmodel$cluster,
     main="K-means with 3 clusters",
     pch=19)
     

kmodel$centers
kmodel$centers[,c("Sepal.Length","Sepal.Width")]


kclusters<-kmodel$cluster
clusplot(iris_df[,c("Sepal.Length","Sepal.Width")],
         kclusters,
         lines=0,
         shade=TRUE,
         color=TRUE,
         labels=2,
         plotchar=FALSE,
         span=TRUE)