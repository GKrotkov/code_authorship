ud_model <- udpipe_load_model("/Users/zachstrennen/hw4app/english-ewt-ud-2.5-191206.udpipe")
ud_model <- udpipe_load_model("../models/english-ewt-ud-2.5-191206.udpipe")
annotation <- udpipe_annotate(ud_model, x = df$text, doc_id = df$doc_id, parser = "none")

anno_edit <- annotation %>%
  as_tibble() %>%
  unite("upos", upos:xpos)

ud_parse <- data.frame(udpipe_annotate(ud_model, 
                                       x = df$text, doc_id = df$doc_id), 
                       stringsAsFactors = FALSE)

df_biber <- biber_udpipe(ud_parse)




df_biber <- df_biber %>%
  column_to_rownames(var = "doc_id")

constant_cols <- which(colMeans(df_biber) == 0)
df_biber <- df_biber[, -constant_cols]

biber_pca <- prcomp(df_biber,
                    center = TRUE, scale. = TRUE)

df_biber$pc1 <- biber_pca$x[, 1]
df_biber$pc2 <- biber_pca$x[, 2]
df_biber$text_type <- as.factor(c(rep('Original',9),rep('Reboot',9)))

df_biber
ggplot(df_biber, aes(x = pc1, y = pc2, color = text_type)) + 
  geom_point(alpha = 0.6) + 
  geom_vline(xintercept = mean(filter(df_biber, text_type == "Reboot")$pc1), linetype="dotted", 
               color = "blue", size=1.5) +
  geom_vline(xintercept = mean(filter(df_biber, text_type == "Original")$pc1), linetype="dotted", 
             color = "red", size=1.5) +
  labs(x = "PC1", y = "PC2", 
       title = "PCA on Biber-tagged loadings for Reboots",
       color = "Movie Type")

fviz_pca_var(biber_pca, select.var = list(contrib = 10),labelsize = 3,repel=TRUE,col.var="contrib") +
  scale_color_gradient2(low="white", mid="blue",
                        high="red",midpoint = 2.5) +
  labs(title = "Reboot Comparison - PCA",
       color = "Contribution")
