library(tidyverse)

calculate_wss <- function(data) {
  # calculates the within sum of squares
  wss <- 0
  for (i in 1:15) {
    wss[i] <- (kmeans(data, centers=i, nstart=20))$tot.withinss
  }
  wss
}

calculate_cluster_error <- function(clusters, classes) { 
  # calculates the error from clustering based on the clusters and
  # the original classes. Assumes the largest clusters are correct.
  decision_table <- table(clusters, classes)
  (apply(decision_table, 1, max) - apply(decision_table, 1, sum)) %>% 
    sum %>% abs  
}

data(wine, package="ggbiplot")

wine %>% View()
summary(wine.class)

pairs(wine_scaled) # suggests some correlated variables. PCA might be useful.

wine_scaled <- scale(wine) %>% as.data.frame()

wine_wss <- calculate_wss(wine_scaled)

# Elbow method suggests k = 3
ggplot(data = data.frame(wss = wine_wss, index = 1:length(wine_wss)),
       aes(index, wss)) +
  geom_line() +
  geom_point(shape = 2)

wine_kmeans <- kmeans(wine_scaled, centers = 3, nstart = 20)

ggplot(wine_scaled, 
       aes(wine.class, Color, colour = as.factor(wine_kmeans$cluster))) +
  geom_point(alpha=0.7, size=2) +
  scale_color_brewer(palette = "Set1")

ggplot(wine_scaled, 
       aes(Hue, Color, colour = as.factor(wine_kmeans$cluster))) +
  geom_point(alpha=0.7) +
  scale_color_brewer(palette = "Set1")

wine_kmeans

table(wine_kmeans$cluster, wine.class)

# How many misclassified?
calculate_cluster_error(wine_kmeans$cluster, wine.class)



