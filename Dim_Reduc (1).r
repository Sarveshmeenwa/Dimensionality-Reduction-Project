library('car')
library("readr")
library("ggpubr")
library("corrplot")
library("FactoMineR")
library("vcd")
library("moments")
library("ggplot2")
library("FactoMineR")
library("factoextra")
library("tidyverse")
library("ClustOfVar")
library("ClusterR")
library("cluster")
library("RColorBrewer")
library("scales")
library("ggdendro")
library("NbClust")

#install.packages('car')

#install.packages('ggpubr')
#install.packages("ggdendro")

df = read_csv("H:/Downloads/Datatsets/users.db.csv")
head(df)

colnames(df)

df_cont <- df[c('score', 'n.matches', 'n.updates.photo', 'n.photos')]

# mpg
ggqqplot(df$score, ylab = "score")
# wt
ggqqplot(df$n.matches, ylab = "n.matches")

res <- cor.test(df$score, df$n.matches, method = "spearman")
res

df$gender[df$gender == 0] <- "Male"
df$gender[df$gender == 1] <- "Female"
df$gender[df$gender == 2] <- "Other"

ggplot(df) +
  aes(x = gender, y = n.matches) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()

shapiro.test(subset(df, gender == "Male")$n.matches)

shapiro.test(subset(df, gender == "Female")$n.matches)

test <- wilcox.test(subset(df, gender != "Other")$n.matches ~ subset(df, gender != "Other")$gender)
test

lm(n.matches ~ score, df)

lm(n.matches ~ gender, df)

skewness(df$score, na.rm = TRUE)

df$score_log <- log10(df$score)
df$n.matches_log <- log10(df$n.matches)
df$n.photos_log <- log10(df$n.photos)

hist(df$score)

hist(df$score_log)

lm(score_log ~ gender+n.matches_log+n.photos_log, df)

lm(score_log ~ gender+n.matches_log+n.photos_log+gender*photo.keke+gender*photo.beach, df)

lm(score_log ~ gender*photo.keke+gender*photo.beach, df)

anova(lm(score_log ~ gender*photo.keke, df))

summary(lm(score_log ~ gender*photo.keke, df))

df$photo.keke <- as.factor(df$photo.keke)

ggplot(df, aes(x=photo.keke, y=score_log, fill=gender)) + geom_boxplot()

# corr_var(df, score_log, method = "spearman", plot = TRUE, top = 10)

mod_1 <- lm(score_log ~ gender+n.matches_log+n.photos_log+gender*photo.keke+gender*photo.beach, df)
anova(mod_1)

summary(mod_1)

.

summary(lm(score_log ~ gender+photo.keke+gender:photo.keke, df))



names(df)

cont_df <- df[c('score', 'score_log', 'n.matches', 'n.updates.photo', 'n.photos', 'sent.ana', 'length.prof')]
# cols <- unlist(lapply(df, is.<numeric))
# cont_df <- df[, cols]
head(cont_df)

results <- prcomp(cont_df, scale = TRUE)
results$rotation

results$x <- -1*results$x
head(results$x)

biplot(results, scale = 0)

cont_df.pca <- prcomp(cont_df, center = TRUE,scale. = TRUE)
summary(cont_df.pca)

fviz_pca_var(cont_df.pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE)

var <- get_pca_var(cont_df.pca)
corrplot(var$cos2, is.corr = FALSE)

fviz_pca_ind(cont_df.pca,
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = df$gender, # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, # Concentration ellipses
             legend.title = "Groups")

ind.coord <- as.data.frame(get_pca_ind(cont_df.pca)$coord)
km.res <- kmeans(as.data.frame(get_pca_ind(cont_df.pca)$coord), 3)
ind.coord$cluster <- factor(km.res$cluster)
ind.coord$gender <- df$gender


eigenvalue <- round(get_eigenvalue(cont_df.pca), 1)
variance.percent <- eigenvalue$variance.percent

ggscatter(
  ind.coord, x = "Dim.1", y = "Dim.2", 
  color = "cluster", palette = "npg", ellipse = TRUE, ellipse.type = "convex",
  shape = "gender", size = 1.5,  legend = "right", ggtheme = theme_bw(),
  xlab = paste0("Dim 1 (", variance.percent[1], "% )" ),
  ylab = paste0("Dim 2 (", variance.percent[2], "% )" )
) +
  stat_mean(aes(color = cluster), size = 4)

fviz_eig(cont_df.pca)

discrete_df <- df[c('gender', 'voyage', 'laugh', 'photo.keke', 'photo.beach')]

discrete_df$gender <- as.factor(discrete_df$gender)
discrete_df$voyage <- as.factor(discrete_df$voyage)
discrete_df$laugh <- as.factor(discrete_df$laugh)
discrete_df$photo.keke <- as.factor(discrete_df$photo.keke)
discrete_df$photo.beach <- as.factor(discrete_df$photo.beach)

head(discrete_df)
dim(discrete_df)

discrete_df.mca <- MCA(discrete_df, graph = FALSE)
print(discrete_df.mca)

eig.val <- get_eigenvalue(discrete_df.mca)
eig.val

fviz_screeplot(discrete_df.mca, addlabels = TRUE, ylim = c(0, 45))


fviz_mca_biplot(discrete_df.mca, 
               repel = TRUE, # Avoid text overlapping (slow if many point)
               addEllipses = TRUE,
               habillage=colnames(discrete_df),
            
            ggtheme = theme_minimal())

fviz_mca_biplot(discrete_df.mca, 
                repel = TRUE, # Avoid text overlapping (slow if many point)
                addEllipses = TRUE,
              
                title=""       ,         
                ggtheme = theme_minimal())

fviz_mca_var(discrete_df.mca, choice = "mca.cor", 
            repel = TRUE, # Avoid text overlapping (slow)
            ggtheme = theme_minimal())

fviz_mca_var(discrete_df.mca, 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())

# Color by cos2 values: quality on the factor map
fviz_mca_var(discrete_df.mca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE, # Avoid text overlapping
             ggtheme = theme_minimal())

fviz_mca_var(discrete_df.mca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE, # Avoid text overlapping
             ggtheme = theme_minimal())

fviz_cos2(discrete_df.mca, choice = "var", axes = 1:2)

fviz_contrib(discrete_df.mca, choice = "var", axes = 1)

fviz_contrib(discrete_df.mca, choice = "var", axes = 2)

fviz_mca_ind(discrete_df.mca,
             label = "none", # hide individual labels
             habillage = "gender", # color by groups,
             addEllipses = TRUE, ellipse.type = "confidence",
             ggtheme = theme_minimal())

fviz_mca_ind(discrete_df.mca, col.ind = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE, # Avoid text overlapping (slow if many points)
             ggtheme = theme_minimal())


fviz_mca_ind(discrete_df.mca, col.ind = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE, # Avoid text overlapping (slow if many points)
             ggtheme = theme_minimal())


plot.MCA(discrete_df.mca,
           axes=c(1,2),
           #invisible="ind",
           autoLab="yes",
           shadowtext=TRUE,
           habillage="quali",
           title="")
 
plot.MCA(discrete_df.mca,
          axes=c(3,4),
          invisible="ind",
          autoLab="yes",
          shadowtext=TRUE,
          habillage="quali",
          title="")
 
 

# run variable clustering excluding the target variable (churn) 
variable_tree <- hclustvar(X.quali = cont_df)
#plot the dendrogram of variable groups
plot(variable_tree)

km.res <- kmeans(cont_df, 3)

summary(cont_df.pca)

fviz_cluster(km.res, data = cont_df,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
             )


# Dimension reduction using PCA
res.pca <- prcomp(cont_df,  scale = TRUE)
# Coordinates of individuals
ind.coord <- as.data.frame(get_pca_ind(res.pca)$coord)
# Add clusters obtained using the K-means algorithm
ind.coord$cluster <- factor(km.res$cluster)
# Add Species groups from the original data sett
ind.coord$gender <- df$gender
# Data inspection
head(ind.coord)

# Percentage of variance explained by dimensions
eigenvalue <- round(get_eigenvalue(res.pca), 1)
variance.percent <- eigenvalue$variance.percent
head(eigenvalue)

ggscatter(
  ind.coord, x = "Dim.1", y = "Dim.2", 
  color = "cluster", palette = "npg", ellipse = TRUE, ellipse.type = "convex",
  shape = "gender", size = 1.5,  legend = "right", ggtheme = theme_bw(),
  xlab = paste0("Dim 1 (", variance.percent[1], "% )" ),
  ylab = paste0("Dim 2 (", variance.percent[2], "% )" )
) +
  stat_mean(aes(color = cluster), size = 4)

comp <- data.frame(cont_df.pca$x[, 1:4])
palette(alpha(brewer.pal(9, 'Set1'), 0.5))
plot(comp, col=km.res$cluster, pch = 16)

comp <- data.frame(cont_df.pca$x[, 1:2])
palette(alpha(brewer.pal(9, 'Set1'), 0.5))
plot(comp, col=km.res$cluster, pch = 16)

plot(comp, pch=16, col=rgb(0,0,0,0.5))

wss <- (nrow(cont_df)-1)*sum(apply(cont_df,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(cont_df,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

seeds_df_sc <- as.data.frame(scale(seeds_df))
d <- dist(cont_df, method = "euclidean")

# Hierarchical clustering using Complete Linkage
hc1 <- hclust(d, method = "complete" )

# Plot the obtained dendrogram
plot(hc1, cex = 0.6, hang = -1)

hc3 <- agnes(cont_df, method = "ward")
pltree(hc3, cex = 0.6, hang = -1, main = "Dendrogram of agnes") 

dist = dist(res.pca, diag=TRUE)
hc <- hclust(dist)
dhc <- as.dendrogram(hc)
plot(dhc[[2]] , main= "zoom on a part of the dendrogram")

mygraph <- graph_from_data_frame(cont_df)
ggraph(mygraph, layout = 'dendrogram', circular = FALSE) + 
  geom_edge_diagonal() +
  geom_node_point() +
  theme_void()

dd <- dist(scale(cont_df), method = "euclidean")
hc <- hclust(dd, method = "ward.D2")

# Convert hclust into a dendrogram and plot
hcd <- as.dendrogram(hc)
# Default plot
plot(hcd, type = "rectangle", ylab = "Height")

plot(hcd, ylim = c(20, 100))

# Define nodePar
nodePar <- list(lab.cex = 0.6, pch = c(NA, 19), 
                cex = 0.7, col = "blue")
# Customized plot; remove labels
plot(hcd, ylab = "Height", nodePar = nodePar, leaflab = "none")

# Build dendrogram object from hclust results
dend <- as.dendrogram(hc)
# Extract the data (for rectangular lines)
# Type can be "rectangle" or "triangle"
dend_data <- dendro_data(dend, type = "rectangle")
# What contains dend_data
names(dend_data)

# Plot line segments and add labels
p <- ggplot(dend_data$segments) + 
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend))+
  geom_text(data = dend_data$labels, aes(x, y, label = label),
            hjust = 1, angle = 90, size = 3)+
  ylim(5, 100)
print(p)



res.pca <- PCA(cont_df, graph = FALSE)
hcpc <- HCPC(res.pca, min = 3, max = NULL, nb.clust = -1)
plot(hc)

plot(hcpc, choice="map")

plot(hcpc, choice="tree")

plot(hcpc, choice="bar")

fviz_dend(hcpc, show_labels = FALSE)

###########################










########################
#testing k-means on pca#

# Calculer k-means avec k = 3
set.seed(123)
res.km <- kmeans(scale(cont_df), 2, nstart = 25)

# Réduction de dimension en utilisant l'ACP
res.pca <- prcomp(cont_df,  scale = TRUE)
# Coordonnées des individus
ind.coord <- as.data.frame(get_pca_ind(res.pca)$coord)
# Ajouter les clusters obtenus à l'aide de l'algorithme k-means
ind.coord$cluster <- factor(res.km$cluster)
# Ajouter les groupes d'espèces issues du jeu de données initial
ind.coord$gender <- df$gender
# Inspection des données
head(ind.coord)



ggscatter(
  ind.coord, x = "Dim.1", y = "Dim.2", 
  color = "cluster", palette = "npg", ellipse = TRUE, ellipse.type = "convex",
  shape = "gender", size = 1.5,  legend = "right", ggtheme = theme_bw(),
  xlab = paste0("Dim 1 (", variance.percent[1], "% )" ),
  ylab = paste0("Dim 2 (", variance.percent[2], "% )" )
) +
  stat_mean(aes(color = cluster), size = 4)

#check optimal number of clusters
#scale df
scaled_contdf <- scale(cont_df)

# Elbow method
fviz_nbclust(scaled_contdf, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")
# Silhouette method
fviz_nbclust(scaled_contdf, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")
# Gap statistic
# nboot = 50 to keep the function speedy.
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
set.seed(123)
fviz_nbclust(scaled_contdf, kmeans, nstart = 25, method = "gap_stat", nboot = 100,verbose=FALSE)+
  labs(subtitle = "Gap statistic method")

nb <- NbClust(scaled_contdf, distance = "euclidean", min.nc = 2,
              max.nc = 7, method = "kmeans")

fviz_nbclust(nb)
#check optimal number of clusters
library(cluster)

set.seed(42)
km_res <- kmeans(scale(cont_df), centers = 2, nstart = 25)

sil <- silhouette(km_res$cluster, dist(scale(cont_df)))
fviz_silhouette(sil)


################
####HCPC########
# Compute PCA with ncp = 3(principal components)
res.pca <- PCA(scaled_contdf, ncp = 3, graph = FALSE)
# Compute hierarchical clustering on principal components
res.hcpc <- HCPC(res.pca, graph = FALSE)
#dendogram
fviz_dend(res.hcpc,
          cex = 0.5, # Label size
          palette = "jco", # Color palette see ?ggpubr::ggpar
          rect = TRUE, rect_fill = TRUE, # Add rectangle around groups
          rect_border = "jco", # Rectangle color
          labels_track_height = 0.8 # Augment the room for labels
)

fviz_cluster(res.hcpc,
             repel = TRUE, # Avoid label overlapping
             show.clust.cent = TRUE, # Show cluster centers
             palette = "jco", # Color palette see ?ggpubr::ggpar
             ggtheme = theme_minimal(),
             main = "Factor map"
)



res.pca <- PCA(cont_df, graph = FALSE)
hcpc <- HCPC(res.pca, min = 3, max = NULL, nb.clust = -1)
fviz_dend(res.hcpc,
          cex = 0.5, # Label size
          palette = "jco", # Color palette see ?ggpubr::ggpar
          rect = TRUE, rect_fill = TRUE, # Add rectangle around groups
          rect_border = "jco", # Rectangle color
          labels_track_height = 0.8 # Augment the room for labels
)

# Principal components + tree
plot(res.hcpc, choice = "3D.map")
plot(res.hcpc,choice='choice')
plot(res.hcpc,choice='tree')


