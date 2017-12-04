library(tidyverse)
library(stringr)

data("iris")

iris$Species %>% levels()
head(iris)

iris_kmeans <- kmeans(iris[,1:4], centers = 3, nstart=20)
iris_kmeans_reduced <- kmeans(iris[,3:4], centers = 3, nstart=20)

summary(iris_kmeans)
summary(iris_kmeans_reduced)

iris_kmeans

plot(iris, col = iris_kmeans$cluster)
plot(iris, col = iris_kmeans_reduced$cluster)

wss <- 0

for (i in 1:15) {
  km.out <- kmeans(iris[,1:4], centers = i, nstart = 20)
  # Save total within sum of squares to wss variable
  wss[i] <- km.out$tot.withinss
}

plot(1:15, wss, type = "b", 
     xlab = "Number of Clusters", 
     ylab = "Within groups sum of squares")

table(iris_kmeans$cluster, iris$Species)
table(iris_kmeans_reduced$cluster, iris$Species)
