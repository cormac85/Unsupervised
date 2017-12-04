library(tidyverse)

data(wine, package="ggbiplot")

wine %>% View()
summary(wine.class)
wine_scaled <- scale(wine) %>% as.data.frame()
