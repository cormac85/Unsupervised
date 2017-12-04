library(tidyverse)
library(datakindr)

fit_kmeans <- function(dataset){
  # Initialise within sum of squares
  wss <- 0
  
  for (i in 1:15) {
    # Fit the model: km.out
    km.out <- kmeans(dataset, centers = i, nstart = 20, iter.max = 50)
    # Save the within cluster sum of squares
    wss[i] <- km.out$tot.withinss
  }
  wss
}

url <- "http://s3.amazonaws.com/assets.datacamp.com/production/course_1903/datasets/WisconsinCancer.csv"

wisc_cancer <- read_csv(url)
summary(wisc_cancer)

wisc_cancer_matrix <- as.matrix(wisc_cancer[, 3:32])
row.names(wisc_cancer_matrix) <- wisc_cancer$id
head(wisc_cancer_matrix)

diagnosis <- as.numeric(wisc_cancer$diagnosis == "M")

summary(diagnosis)
wisc_cancer %>% group_by(diagnosis) %>% tally()
table(diagnosis)

# Data summarise
wisc_cancer_matrix %>% 
  as.data.frame() %>% 
  summarise_all(funs(mean, sd)) %>% 
  t %>% 
  as.data.frame %>% 
  rownames_to_column() %>%
  mutate(V1 = round(V1, 2)) %>% 
  arrange(rowname)

# PCA
wisc_cancer_pcs <- prcomp(wisc_cancer_matrix, scale = TRUE, center = TRUE)

summary(wisc_cancer_pcs)

ggbiplot::ggscreeplot(wisc_cancer_pcs)
ggbiplot::ggbiplot(wisc_cancer_pcs)

# First few PC's plotted against each other.
# Note that PC1 and PC2 are quite different as they are created 
# orthogonally to each other. 
gridExtra::grid.arrange(
wisc_cancer_pcs$x %>% as.data.frame() %>% 
  ggplot(aes(PC1, PC2, colour = as.factor(diagnosis))) +
  geom_point(alpha = 0.75) +
  dk_theme,
wisc_cancer_pcs$x %>% as.data.frame() %>% 
  ggplot(aes(PC1, PC3, colour = as.factor(diagnosis))) +
  geom_point(alpha = 0.75) +
  dk_theme,
wisc_cancer_pcs$x %>% as.data.frame() %>% 
  ggplot(aes(PC2, PC3, colour = as.factor(diagnosis))) +
  geom_point(alpha = 0.75) +
  dk_theme
)

# Explained Variance
# Calculate variability of each component
# Variance explained by each principal component: pve
pve <- wisc_cancer_pcs$sdev^2 / sum(wisc_cancer_pcs$sdev^2)


gridExtra::grid.arrange(
ggplot(data=data.frame(pve=pve, index=1:length(pve)),
       aes(index, pve)) +
  geom_point(shape=2) +
  geom_line() +
  ylim(0,1) +
  dk_theme ,
ggplot(data=data.frame(cumulative_pve = cumsum(pve), index=1:length(pve)),
       aes(index, cumulative_pve)) +
  geom_point(shape=2) +
  geom_line() +
  ylim(0,1) +
  dk_theme 
)

wisc_cancer_pcs$rotation %>% head()

# H-Clustering
wisc_cancer_scaled <- scale(wisc_cancer_matrix)
wisc_cancer_scaled %>% head()

wisc_cancer_hclust <- hclust(dist(wisc_cancer_scaled), method= "complete")

plot(wisc_cancer_hclust)

wisc_cancer_clusters <- cutree(wisc_cancer_hclust, k = 4)

table(wisc_cancer_clusters, diagnosis)

apply(table(wisc_cancer_clusters, diagnosis), 1, min)

ggplot(data = wisc_cancer[ ,2:32] %>% cbind(wisc_cancer_clusters),
       aes(radius_mean, texture_mean, 
           colour = as.factor(wisc_cancer_clusters))) +
  geom_point(size = 2, alpha = 0.7) +
  dk_theme +
  scale_color_brewer(palette = "Set1")

ggplot(data = wisc_cancer[ ,2:32] %>% cbind(wisc_cancer_clusters),
       aes(diagnosis, radius_mean, 
           colour = as.factor(wisc_cancer_clusters))) +
  geom_point(size = 2, alpha = 0.7) +
  dk_theme +
  scale_color_brewer(palette = "Set1")


# Kmeans clustering
wss_wisc <- fit_kmeans(scale(wisc_cancer_matrix))

# Scree
ggplot(data=data.frame(wss_wisc, index = 1:length(wss_wisc)),
       aes(index, wss_wisc)) +
  geom_line() +
  geom_point(shape=2)

wisc_kmeans <- kmeans(scale(wisc_cancer_matrix), 2, nstart=20)

# Evaluate
ggplot(data = wisc_cancer[ ,2:32] %>% cbind(wisc_kmeans$cluster),
       aes(diagnosis, radius_mean, 
           colour = as.factor(wisc_kmeans$cluster))) +
  geom_point(size = 2, alpha = 0.7) +
  dk_theme +
  scale_color_brewer(palette = "Set1")

table(wisc_kmeans$cluster, diagnosis) %>% 
  apply(1, min) %>% sum 
# 51 are in the other cluster.

table(wisc_kmeans$cluster, wisc_cancer_clusters) 

# 37 are in disagreement
table(wisc_kmeans$cluster, wisc_cancer_clusters) %>% 
  apply(2, min) %>% sum 

################################
# using PCA with hclust
################################

wisc_pca_hclust <- hclust(dist(wisc_cancer_pcs$x[, 1:7]), method = "complete")
wisc_pca_hclust_clusters <- cutree(wisc_pca_hclust, k=4)

# Evaluate
table(wisc_pca_hclust_clusters, diagnosis)
# Compare 3 approaches: Hclust, kmeans, PCA with hclust
table(wisc_pca_hclust_clusters, diagnosis) %>% apply(1, min) %>% sum
table(wisc_kmeans$cluster, diagnosis) %>% apply(1, min) %>% sum
table(wisc_cancer_clusters, diagnosis) %>% apply(1, min) %>% sum
