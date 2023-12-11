## --------------------------------------------------------------------------------------------------------
# PUT YOUR CODE AND PLOT HERE


## --------------------------------------------------------------------------------------------------------
library(tidyverse)
spotify <- read_csv("https://raw.githubusercontent.com/FSKoerner/Fall23-36315-data/main/spotify.csv", 
                    col_types = cols())


## --------------------------------------------------------------------------------------------------------
spotify[1,]


## --------------------------------------------------------------------------------------------------------
spotify_quant = spotify %>% dplyr::select("acousticness", "danceability", "duration_ms", "energy", "instrumentalness", "liveness", "speechiness", "tempo", "valence", "popularity")
spotify_scale = scale(spotify_quant, center=TRUE, scale=TRUE)
spotify_dist = dist(spotify_scale)
spotify_cl <- hclust(spotify_dist, method = 'complete')
spotify_sl <- hclust(spotify_dist, method = "single")
spotify_cl_dend <- as.dendrogram(spotify_cl)
spotify_sl_dend <- as.dendrogram(spotify_sl)
plot(spotify_sl_dend, main = "Single Linkage Dendrogram")
plot(spotify_cl_dend, main = "Complete Linkage Dendrogram")


## --------------------------------------------------------------------------------------------------------
 library(dendextend)
 hc_spotify_cl_dend <- set(spotify_cl_dend, "branches_k_color", k = 3)
 plot(hc_spotify_cl_dend)


## --------------------------------------------------------------------------------------------------------
hc_spotify_sl_dend <- set(spotify_sl_dend, "branches_k_color", k = 3)
 plot(hc_spotify_sl_dend)


## --------------------------------------------------------------------------------------------------------
#cutree(hc_spotify_cl_dend, k=3)
table(cutree(hc_spotify_cl_dend, k=3))
table(cutree(hc_spotify_sl_dend, k=3))


## --------------------------------------------------------------------------------------------------------
spotify_decades_colors = ifelse(spotify$decades == "1920s-30s", "pink", 
                                ifelse(spotify$decades == "1940s-50s", "orange", 
                                       ifelse(spotify$decades == "1960s-1970s", "yellow", 
                                              ifelse(spotify$decades == "1980s-1990s", "lightblue", "purple"))))
                                                     


## --------------------------------------------------------------------------------------------------------
hc_spotify_cl_dend <- set(hc_spotify_cl_dend, "labels_colors", order_value = TRUE, spotify_decades_colors)
plot(hc_spotify_cl_dend)


## --------------------------------------------------------------------------------------------------------
spotify_quant = spotify %>% dplyr::select("acousticness", "danceability", "duration_ms", "energy", "instrumentalness", "liveness", "speechiness", "tempo", "valence", "popularity")
spotify_scale = scale(spotify_quant, center=TRUE, scale=TRUE)


## --------------------------------------------------------------------------------------------------------
spotify_pca <- prcomp(spotify_scale)
summary(spotify_pca)


## --------------------------------------------------------------------------------------------------------
library(factoextra)
fviz_eig(spotify_pca)
fviz_eig(spotify_pca) + geom_hline(yintercept = 100*(1/ncol(spotify_quant)))


## --------------------------------------------------------------------------------------------------------
spotify_pc_matrix <- spotify_pca$x
spotify$pca1 = spotify_pc_matrix[,1]
spotify$pca2 = spotify_pc_matrix[,2]
spotify %>% ggplot(aes(x=pca1, y=pca2)) + geom_point(aes(color = decades))


## --------------------------------------------------------------------------------------------------------
spotify$pca3 = spotify_pc_matrix[,3]
spotify %>% ggplot(aes(x=pca1, y=pca3)) + geom_point(aes(color = decades))


## --------------------------------------------------------------------------------------------------------
fviz_pca_biplot(spotify_pca,
                # Set the color of the points to decades variable:
                habillage = spotify$decades, pointshape = 19)


## --------------------------------------------------------------------------------------------------------
spotify_pca$rotation

