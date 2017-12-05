library(tidyverse)
library(mclust)

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

calculate_pov <- function(pca_results) {
  # Calculates the proportion of variance, given the output from prcomp()
  pr_var <- pca_results$sdev^2
  pr_var / sum(pr_var)
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

# Gaussian Mixture Methods using mclust
wine_mclust <- Mclust(wine_scaled)
wine_mclust %>% summary()

ggplot(wine_scaled, 
       aes(wine.class, Color, colour = as.factor(wine_kmeans$cluster))) +
  geom_point(alpha=0.7, size=2) +
  scale_color_brewer(palette = "Set1")

calculate_cluster_error(wine_mclust$classification, wine.class)

# Hcluster with wards method
wine_hclust <- dist(wine_scaled) %>% hclust(method="ward.D")
wine_hclust %>% summary
plot(wine_hclust)
wine_hclust_clusters <- wine_hclust %>% cutree(k=3)

wine_scaled_results <- 
  wine_scaled %>% 
  cbind(hclust_clusters = wine_hclust_clusters, 
        kmeans_clusters = wine_kmeans$cluster, 
        mclust_clusters = wine_mclust$classification,
        classes = wine.class)

ggplot(wine_hclust_results, 
       aes(wine.class, Color, colour = as.factor(wine_hclust_clusters))) +
  geom_bar(alpha=0.7, size=2, position = "stack") +
  scale_color_brewer(palette = "Set1")

# Cluster means for each variable
wine_mclust$parameters$mean

# PCA with mclust
wine_pc <- prcomp(wine, scale = TRUE, center = TRUE)
wine_pc
summary(wine_pc)
ggbiplot::ggbiplot(wine_pc, choices = c(1,3))
wine_pc_pov <- calculate_pov(wine_pc)

ggplot(data=data.frame(proportion_of_var = wine_pc_pov,
                       principle_component = 1:length(wine_pc_pov)),
       aes( principle_component, cumsum(proportion_of_var))) +
  geom_point() +
  geom_line() +
  ggtitle("Scree plot for wine PCA")+
  theme_bw() # 7 maybe?

wine_BIC = mclustBIC(wine_pc$x[ ,1:6])
plot(wine_BIC)
summary(wine_BIC)
wine_mclust_pca <- Mclust(wine_pc$x[ ,1:6], x=wine_BIC)

wine_scaled_results$mclust_pca_clusters <-
  wine_mclust_pca$classification

# Comparing methods
table(wine_scaled_results$kmeans_clusters, wine_scaled_results$classes)
table(wine_scaled_results$hclust_clusters, wine_scaled_results$classes)
table(wine_scaled_results$mclust_clusters, wine_scaled_results$classes)
table(wine_scaled_results$mclust_pca_clusters, wine_scaled_results$classes)

calculate_cluster_error(wine_scaled_results$kmeans_clusters, wine_scaled_results$classes)
calculate_cluster_error(wine_scaled_results$hclust_clusters, wine_scaled_results$classes)
calculate_cluster_error(wine_scaled_results$mclust_clusters, wine_scaled_results$classes)
calculate_cluster_error(wine_scaled_results$mclust_pca_clusters, wine_scaled_results$classes)

ggplot(wine_scaled_results, 
       aes(classes, Flav, colour = as.factor(mclust_pca_clusters))) +
  geom_point(alpha=0.7, size=2) +
  scale_color_brewer(palette = "Set1")