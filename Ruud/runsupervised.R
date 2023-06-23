library(tidyverse)
library(factoextra)



df = read.csv('../data/ruud_fdf.csv')
summary(df)




sdf = df[, !(names(df) %in% c('Odd_Ruud', 'Odd_Opp', 'Odd_Pred', 'Odd_Acc', 'Win', 'Rank_Ruud', 'Rank_Opp'))]
summary(sdf)

pc_all = prcomp(sdf, scale = TRUE)
pc_all


summary(pc_all)


fviz_eig(pc_all, addlabels = TRUE, ylim = c(0, 50), main = "Scree Plot of PCA")



fviz_pca_var(pc_all, col.var = "blue", col.quanti.sup = "red", 
             addlabels = TRUE, repel = TRUE)


plot(pc_all$x[, 1], pc_all$x[, 2], type = "n", xlab = "PC1", ylab = "PC2") 
points(pc_all$x[, 1], pc_all$x[, 2], col = rgb(1, 0, 0, alpha = 0.5), pch = 16)  
arrows(0, 0, pc_all$rotation[, 1]*5, pc_all$rotation[, 2]*5, length = 0.1, angle = 30)
text(pc_all$rotation[, 1]*3, pc_all$rotation[, 2]*3, labels = rownames(pc_all[[2]]), pos = 3)





# PCA WITH ONLY SELECTED VARIABLES 

ssdf = df[, !(names(df) %in% c('Match_Importance', 'Hard', 'Grass', 'Opp_Set_Perc', 
                              'Odd_Ruud', 'Pre_Ruud', 'Pre5_Ruud', 'Five_Set', 'Pre_Opp', 'Pre5_Opp',
                              'Odd_Opp', 'Odd_Pred', 'Odd_Acc', 'Win', 'Rank_Ruud', 'Rank_Opp'))]
summary(ssdf)



pc_six = prcomp(ssdf, scale = TRUE)
pc_six


summary(pc_six)


fviz_eig(pc_six, addlabels = TRUE, ylim = c(0, 50), main = "Scree Plot of PCA")



fviz_pca_var(pc_six, col.var = "blue", col.quanti.sup = "red", 
             addlabels = TRUE, repel = TRUE)


plot(pc_six$x[, 1], pc_six$x[, 2], type = "n", xlab = "PC1", ylab = "PC2") 
points(pc_six$x[, 1], pc_six$x[, 2], col = rgb(1, 0, 0, alpha = 0.5), pch = 16)  
arrows(0, 0, pc_six$rotation[, 1]*3, pc_six$rotation[, 2]*3, length = 0.1, angle = 30)
text(pc_six$rotation[, 1]*4, pc_six$rotation[, 2]*4, labels = rownames(pc_six[[2]]), pos = 3)







# CLUSTERING

set.seed(1)

clusters = kmeans(sdf, 2)

clusters


plot(sdf, col = adjustcolor(clusters$cluster + 1, alpha.f = 0.1),
      main = "K- Means Clustering Results with K = 2", pch = 20)



df$cluster = unlist(clusters[1])
head(df)


count01 = sum((df$Win == 0) & (df$cluster == 1))
count02 = sum((df$Win == 0) & (df$cluster == 2))
count11 = sum((df$Win == 1) & (df$cluster == 1))
count12 = sum((df$Win == 1) & (df$cluster == 2))

count01
count02
count11
count12



# CLUSTERING (6 VAR)


clusters_six = kmeans(ssdf, 2)

clusters_six


plot (ssdf, col = adjustcolor(clusters_six$cluster + 1, alpha.f = 0.9),
      main = "K- Means Clustering Results with K = 2", pch = 20)


# CLUSTERING PCA (FIRST TWO WHEN nPC = 6)

pcadf = data.frame(PC1 = as.data.frame(pc_six$x)$PC1, PC2 = as.data.frame(pc_six$x)$PC2)



clusters_pca = kmeans(pcadf, 2)
clusters_pca

plot (pcadf, col = adjustcolor(clusters_pca$cluster + 1, alpha.f = 0.9),
      main = "K- Means Clustering Results with K = 2", pch = 20)




# CLUSTERING WITH ALL PCs WHEN nPC = 15



pcadf_all = as.data.frame(pc_all$x)



clusters_pca_all = kmeans(pcadf_all, 2)
clusters_pca_all

plot (pcadf_all, col = adjustcolor(clusters_pca_all$cluster + 1, alpha.f = 0.9),
      main = "K- Means Clustering Results with K = 2", pch = 20)





# CLUSTERING WITH FIRST TWO PCs WHEN nPC = 15

pcadf_all2 = data.frame(PC1 = as.data.frame(pc_all$x)$PC1, PC2 = as.data.frame(pc_all$x)$PC2)



clusters_pca_all2 = kmeans(pcadf_all2, 2)
clusters_pca_all2

plot (pcadf_all2, col = adjustcolor(clusters_pca_all2$cluster + 1, alpha.f = 0.9),
      main = "K- Means Clustering Results with K = 2", pch = 20)
