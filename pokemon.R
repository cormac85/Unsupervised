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

wss <- fit_kmeans(pok[ ,6:11])

plot(1:15, wss, type = "b", 
     xlab = "Number of Clusters", 
     ylab = "Within groups sum of squares")

k <- 4

pok_kmeans <- kmeans(pok[ , 6:11],k, nstart=20)


plot(pok[ ,c(6,7,8,9,10,11,13)] %>% 
       mutate(Legendary = as.factor(Legendary)),
     col=pok_kmeans$cluster)
