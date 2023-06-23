library(tidyverse)
library(factoextra)



df = read.csv('../data/djo_fdf.csv')
summary(df)




sdf = df[, !(names(df) %in% c('Odd_Djo', 'Odd_Opp', 'Odd_Pred', 'Odd_Acc', 'Win', 'Rank_Djo', 'Rank_Opp'))]
summary(sdf)

pc_all = prcomp(sdf, scale = TRUE)
pc_all


summary(pc_all)


fviz_eig(pc_all, addlabels = TRUE, ylim = c(0, 50), main = "Scree Plot of PCA")



fviz_pca_var(pc_all, col.var = "black", col.quanti.sup = "red", 
             addlabels = TRUE, repel = TRUE, title = '')





plot(pc_all$x[, 1], pc_all$x[, 2], type = "n", xlab = "PC1", ylab = "PC2") 
points(pc_all$x[, 1], pc_all$x[, 2], col = rgb(1, 0, 0, alpha = 0.5), pch = 16)  
arrows(0, 0, pc_all$rotation[, 1]*5, pc_all$rotation[, 2]*5, length = 0.1, angle = 30)
text(pc_all$rotation[, 1]*5, pc_all$rotation[, 2]*5, labels = rownames(pc_all[[2]]), pos = 3)





# PCA WITH ONLY SELECTED VARIABLES 

ssdf = df[, !(names(df) %in% c('Match_Importance', 'Hard', 'Grass', 'Indoor', 'Opp_Set_Perc', 
                              'Odd_Djo', 'H2H_Djo', 'Pre_Djo', 'Pre5_Djo',
                              'Odd_Opp', 'Odd_Pred', 'Odd_Acc', 'Win', 'Rank_Djo', 'Rank_Opp'))]
summary(ssdf)



pc_seven = prcomp(ssdf, scale = TRUE)
pc_seven


summary(pc_seven)


fviz_eig(pc_seven, addlabels = TRUE, ylim = c(0, 50), main = "Scree Plot of PCA")



fviz_pca_var(pc_seven, col.var = "blue", col.quanti.sup = "red", 
             addlabels = TRUE, repel = TRUE)


plot(pc_seven$x[, 1], pc_seven$x[, 2], type = "n", xlab = "PC1", ylab = "PC2") 
points(pc_seven$x[, 1], pc_seven$x[, 2], col = rgb(1, 0, 0, alpha = 0.5), pch = 16)  
arrows(0, 0, pc_seven$rotation[, 1]*3, pc_seven$rotation[, 2]*3, length = 0.1, angle = 30)
text(pc_seven$rotation[, 1]*2, pc_seven$rotation[, 2]*2, labels = rownames(pc_seven[[2]]), pos = 3)







# CLUSTERING

set.seed(1)

clusters = kmeans(sdf, 2)

clusters


plot (sdf, col = adjustcolor(clusters$cluster + 1, alpha.f = 0.1), main = '', pch = 20)



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


pseudoacc = (count02 + count11) / (count01 + count02 + count11 + count12)
pseudoacc


# CLUSTERING (7 VAR)


clusters_seven = kmeans(ssdf, 2)

clusters_seven


plot (ssdf, col = adjustcolor(clusters_seven$cluster + 1, alpha.f = 0.1),
      main = "K- Means Clustering Results with K = 2", pch = 20)


# CLUSTERING PCA

pcadf = data.frame(PC1 = as.data.frame(pc_seven$x)$PC1, PC2 = as.data.frame(pc_seven$x)$PC2)



clusters_pca = kmeans(pcadf, 2)
clusters_pca

plot (pcadf, col = adjustcolor(clusters_pca$cluster + 1, alpha.f = 0.9),
      main = "K- Means Clustering Results with K = 2", pch = 20)


clusters_pca_six = kmeans(pcadf, 6)
clusters_pca_six

plot (pcadf, col = adjustcolor(clusters_pca_six$cluster + 1, alpha.f = 0.9),
      main = "K- Means Clustering Results with K = 6", pch = 20)






