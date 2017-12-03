library(tidyverse)
data(iris)

hclust.out <- hclust(dist(iris[1:3]))

summary(hclust.out)

plot(hclust.out)

abline(h = 4, col = "red")

cutree(hclust.out, k = 3)

plot(iris, col=cutree(hclust.out, k = 3))