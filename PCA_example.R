library(tidyverse)


data("iris")

summary(iris)

pr.iris <- prcomp(x = iris[-5], scale = F, center = T)
summary(pr.iris)
pr.iris

# Pokemon
pok <- read_csv("data/Pokemon.csv")

pok_main_vars <- pok %>% select(HP, Attack, Defense, Speed)

pok_pc <- prcomp(pok_main_vars, scale = TRUE, center = TRUE)
summary(pok_pc)

pok_pc$center
pok_pc$scale
pok_pc$rotation

biplot(pr.iris)

# Older package, will probably break some tidyverse stuff. 
# Don't load into namespace
ggbiplot::ggbiplot(pr.iris) 

# Getting proportion of variance for a scree plot
pr_var <- pr.iris$sdev^2
proportion_of_var <- pr_var / sum(pr_var)

plot(proportion_of_var, ylim=c(0,1), type='b')


# Back to pokemon
ggbiplot::ggbiplot(pok_pc)


pr_var_pok <- pok_pc$sdev^2
proportion_of_var_pok <- pr_var_pok / sum(pr_var_pok)

plot(cumsum(proportion_of_var_pok), ylim=c(0,1), type='b', 
     main="Pokemon Proportion of Var")