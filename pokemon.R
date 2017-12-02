library(tidyverse)

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

pok <- read_csv("data/Pokemon.csv")

head(pok)

pok_stats <- pok[,6:11]

wss <- fit_kmeans(pok_stats)

plot(1:15, wss, type = "b", 
     xlab = "Number of Clusters", 
     ylab = "Within groups sum of squares")

k <- 6

pok_kmeans <- kmeans(pok_stats,k, nstart=20)

plot(pok_stats, col=pok_kmeans$cluster)
