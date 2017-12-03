library(tidyverse)

pok <- read_csv("data/Pokemon.csv")

pok %>% 
  select(HP, Attack, Defense,  Speed, `Sp. Atk`, `Sp. Def`) %>% 
  summarise_all(funs(mean, sd)) %>% 
  t %>% 
  as.data.frame %>% 
  rownames_to_column() %>%
  separate(rowname, into = c("feature", "fun"), sep = "_") %>% 
  spread(fun, V1)

pok.scaled <- 
  scale(pok %>% select(HP, Attack, Defense,  Speed, `Sp. Atk`, `Sp. Def`))

hclust.pok <- 
  hclust(dist(
    pok %>% select(HP, Attack, Defense,  Speed, `Sp. Atk`, `Sp. Def`)), 
    method = "single")
cut.pok <- cutree(hclust.pok, k=3)

pok_kmeans <- kmeans(pok[ , 6:11], 4, nstart=20)

table(pok_kmeans$cluster, cut.pok)


plot(pok[ ,c(6,7,8,9,10,11,13)] %>% 
       mutate(Legendary = as.factor(Legendary)),
     col=cut.pok)


plot(pok[ ,c(6,7,8,9,10,11,13)] %>% 
       mutate(Legendary = as.factor(Legendary)),
     col=pok_kmeans$cluster)