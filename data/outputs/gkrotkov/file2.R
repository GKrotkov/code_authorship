## ----setup, include=FALSE--------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## --------------------------------------------------------------------------------------------------------
library(tidyverse)
library(factoextra)
coffee_ratings <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-07/coffee_ratings.csv') %>%
  filter(total_cup_points > 0)


## --------------------------------------------------------------------------------------------------------
coffee_quant <- coffee_ratings[, 21:31] %>%
    scale(center = TRUE, 
          scale = apply(., 2, sd, na.rm = TRUE))


## --------------------------------------------------------------------------------------------------------
coffee_pca <- prcomp(coffee_quant,
                     center = TRUE, scale. = TRUE)

coffee_ratings$pc1 <- coffee_pca$x[, 1]
coffee_ratings$pc2 <- coffee_pca$x[, 2]
coffee_ratings$pc3 <- coffee_pca$x[, 3]

coffee_ratings %>%
    ggplot(aes(x = pc1, y = pc2)) +
    geom_point(alpha = 0.5) +
    labs(title = "Principal Components of Coffee Dataset", 
         x = "PC 1", y = "PC 2") + 
    theme_bw()

summary(coffee_pca)


## --------------------------------------------------------------------------------------------------------
fviz_eig(coffee_pca, addlabels = TRUE, ncp = 11) +
    geom_hline(yintercept = 100 * (1 / ncol(coffee_pca$x)), 
               linetype = "dashed", color = "darkred")


## --------------------------------------------------------------------------------------------------------
ggplot(coffee_ratings, aes(x = pc1, y = pc2, color = total_cup_points)) + 
    geom_point() + 
    scale_color_gradient(low = "darkblue", high = "yellow") +
    labs(title = "PCA of Coffee Ratings", 
         x = "PC1", y = "PC2", color = "Total Cup Points") + 
    theme_bw()


## --------------------------------------------------------------------------------------------------------
ggplot(coffee_ratings, aes(x = pc1, y = pc3, color = total_cup_points)) + 
    geom_point() + 
    scale_color_gradient(low = "darkblue", high = "yellow") +
    labs(title = "PCA of Coffee Ratings", 
         x = "PC1", y = "PC3", color = "Total Cup Points") + 
    theme_bw()


## --------------------------------------------------------------------------------------------------------
fviz_pca_biplot(coffee_pca, label = "var", axes = c(1, 3),
                alpha.ind = 0.6, alpha.var = 0.5, repel = FALSE,
                col.ind = coffee_ratings$total_cup_points, col.var = "black") +
  scale_color_gradient(low = "darkblue", high = "darkorange") +
  labs(color = "Total cup points") +
  theme(legend.position = "bottom")


## --------------------------------------------------------------------------------------------------------
coffee_pca$rotation


## ---- warning = FALSE------------------------------------------------------------------------------------
load("data.rda")

ggplot(companies, aes(x = mean_salary_min)) + 
    geom_histogram(bins = 40, fill = "lightblue", color = "black") + 
    theme_bw() + 
    labs(title = "Distribution of Average Minimum Salary", 
         x = "Avg. Max Salary", y = "Frequency")

