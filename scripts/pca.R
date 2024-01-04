library(tidyverse)
library(quanteda)
library(nFactors)
library(factoextra)
source("cmu_textstats/mda_functions.R")

load("../data/pca_info.rda")

# Create pairwise_loadings dataframe
# need to find a way to tag each comparison as being individual vs group, etc
pairwise_loadings <- pca_info
rm(pca_info)
comparisons <- gsub("[0-9_]", "", rownames(pairwise_loadings))
comparison_type <- ifelse(comparisons == "group group", "group to group", 
                          ifelse(comparisons == "individual individual", 
                                 "individual to individual", 
                                 "group to individual"))

# Screeplot
# use screeplot_mda()
screeplot_mda(pairwise_loadings)

# PCA Plot
authorship_pca <- prcomp(pairwise_loadings, center = TRUE, scale. = TRUE)
theme_set(theme_bw())
ggplot(data.frame(pc1 = authorship_pca$x[, 1], 
                  pc2 = authorship_pca$x[, 2], 
                  comp_type = comparison_type), 
       aes(x = pc1, y = pc2, color = comp_type)) + 
    geom_point(alpha = 0.6) + 
    labs(x = "PC1", y = "PC2", 
         title = "PCA of Pairwise Comparisons")

ggplot(data.frame(pc1 = authorship_pca$x[, 3], 
                  pc2 = authorship_pca$x[, 4], 
                  comp_type = comparison_type), 
       aes(x = pc1, y = pc2, color = comp_type)) + 
    geom_point(alpha = 0.6) + 
    labs(x = "PC3", y = "PC4", 
         title = "PCA of Pairwise Comparisons")

ggplot(data.frame(pc1 = authorship_pca$x[, 5], 
                  pc2 = authorship_pca$x[, 6], 
                  comp_type = comparison_type), 
       aes(x = pc1, y = pc2, color = comp_type)) + 
    geom_point(alpha = 0.6) + 
    labs(x = "PC5", y = "PC6", 
         title = "PCA of Pairwise Comparisons")

ggplot(data.frame(pc1 = authorship_pca$x[, 7], 
                  pc2 = authorship_pca$x[, 8], 
                  comp_type = comparison_type), 
       aes(x = pc1, y = pc2, color = comp_type)) + 
    geom_point(alpha = 0.6) + 
    labs(x = "PC7", y = "PC8", 
         title = "PCA of Pairwise Comparisons")

ggplot(data.frame(pc1 = authorship_pca$x[, 4], 
                  pc2 = authorship_pca$x[, 6], 
                  comp_type = comparison_type), 
       aes(x = pc1, y = pc2, color = comp_type)) + 
    geom_point(alpha = 0.6) + 
    labs(x = "PC4", y = "PC6", 
         title = "PCA of Pairwise Comparisons")

# Biplot

# this biplot is too busy, we'll try to simulate it with pca_var
fviz_pca_biplot(authorship_pca, label = "var", 
                alpha.ind = 0.4, alpha.var = 0.75, labelsize = 3, 
                col.var = "darkblue", repel = TRUE) + 
    scale_color_gradient2(low = "white", mid = "blue", high = "red")

fviz_contrib(authorship_pca, choice = "var", axes = 1:8, top = 50)

fviz_pca_var(authorship_pca, select.var = list(contrib = 35), axes = c(4, 6),
             label = "var", col.var = "contrib", repel = TRUE) + 
    scale_color_gradient2(low = "blue", high = "red")
