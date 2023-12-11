library(tidyverse)
library(quanteda)
library(nFactors)
library(factoextra)
source("cmu_textstats/mda_functions.R")

load("../data/inputs/pca_test_chunk.rda")

# Create pairwise_loadings dataframe
# need to find a way to tag each comparison as being individual vs group, etc
pairwise_loadings <- df_biber

# Screeplot
# use screeplot_mda()
screeplot_mda(pairwise_loadings)

# PCA Plot
authorship_pca <- prcomp(pairwise_loadings, center = TRUE, scale. = TRUE)

ggplot(data.frame(pc1 = authorship_pca$x[, 1], pc2 = authorship_pca$x[, 2], 
                  text_type = pairwise_loadings$text_type), 
       aes(x = pc1, y = pc2, color = text_type)) + 
    geom_point(alpha = 0.6) + 
    labs(x = "PC1", y = "PC2", 
         title = "PCA on Biber-tagged loadings")

# Biplot

# this biplot is too busy, we'll try to simulate it with pca_var
fviz_pca_biplot(authorship_pca, label = "var", 
                alpha.ind = 0.4, alpha.var = 0.75, labelsize = 3, 
                col.var = "darkblue", repel = TRUE) + 
    scale_color_gradient2(low = "white", mid = "blue", high = "red")

fviz_contrib(authorship_pca, choice = "var", axes = 1, top = 20)

fviz_pca_var(authorship_pca, select.var = list(contrib = 10), label = "var", 
             col.var = "contrib", repel = TRUE) + 
    scale_color_gradient2(low = "blue", high = "red")
