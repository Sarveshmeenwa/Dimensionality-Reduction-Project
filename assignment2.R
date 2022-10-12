library('plyr')
library('dplyr')
library(Hmisc)
library("viridis")  
library('naniar')
library(xtable)
library(schoRsch)
library(gmodels)
library(readr)
#interpreting results 
#https://blog.bioturing.com/2018/06/18/how-to-read-pca-biplots-and-scree-plots/
df  <- read.csv('users.db.csv')
head(df)
summary(df)

#does not have a normal distribution 
ggqqplot(df$score,ylab="score")
ggqqplot(df$n.matches,ylab='matches')
cor.test(df$score,df$n.matches, method = 'spearman')
lm(n.matches ~ score, data = df)
hist(df$score)
hist(log(df$score))
hist(df$n.matches)
hist(log(df$n.matches))
#when it's not normally distributed we have to use log so that we can use lm
mod <- lm(log(n.matches) ~ log(df$score)+gender+photo.keke+gender*photo.beach, data = df)
summary(mod)
#AIC plus bas plus mieux 

#make the pca analysis
install.packages(c("FactoMineR", "factoextra"))
library("FactoMineR")
library("factoextra")
#keep only the nuemrical variables for scaling
df2=df[,-c(9,10,13,14,15,16,2,7,8)]
#inertia drops when you add more variables 
#when scaling we only keep the numeric variables
PCA(df2, scale.unit = TRUE, graph = TRUE)

#using MCA for discrete variables
df3=df[,c(13,14,15,16,10)]
for (i in 1:5) {
  plot(df3[,i], main=colnames(df3)[i],
       ylab = "Count", col="steelblue", las = 2)
}
library(magrittr)
library(dplyr)


df3[sapply(df3, is.numeric)] <- lapply(df3[sapply(df3, is.numeric)], 
                                       as.character)
str(df3)
df3[sapply(df3, is.character)] <- lapply(df3[sapply(df3, is.character)], 
                                       as.factor)
str(df3)

MCA(df3)
res.mca <- MCA(df3, graph = FALSE)
print(res.mca)
fviz_screeplot(res.mca, addlabels = TRUE, ylim = c(0, 45))
fviz_mca_biplot(res.mca, 
                repel = TRUE, # Avoid text overlapping (slow if many point)
                ggtheme = theme_minimal())

#individuals_MCA
fviz_mca_ind(res.mca, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE, # Avoid text overlapping (slow if many points)
             ggtheme = theme_minimal())
             

fviz_mca_ind(res.mca, 
             label = "none", # hide individual labels
             habillage = "gender", # color by groups 
             palette = c("#00AFBB", "#E7B800"),
             addEllipses = TRUE, ellipse.type = "confidence",
             ggtheme = theme_minimal()) 
#small arrows are a bad sign


#Apply pca and kmean
#standarization
#We correct for this by scaling the data using the scale() function. We can then verify that the variances across the different variables are equal so that when we apply principal components one variable does not dominate.
# Scale
df2 <- data.frame(scale(df2))
# Verify variance is uniform
plot(sapply(df2, var))
# Proceed with principal components
#https://www.r-bloggers.com/2014/06/pca-and-k-means-clustering-of-delta-aircraft/
pc <- princomp(df2)
plot(pc)
plot(pc, type='l')
summary(pc) # 2 components is both 'elbow' and explains >85% variance
pc <- prcomp(df2)
# First for principal components
comp <- data.frame(pc$x[,1:2])
# Plot
plot(comp, pch=16, col=rgb(0,0,0,0.5))

wss <- (nrow(df2)-1)*sum(apply(df2,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(df2,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")
#So here we can see that the "elbow" in the scree plot is at k=4, so we apply the k-means clustering function with k = 4 and plot.
# From scree plot elbow occurs at k = 4
# Apply k-means with k=4
k <- kmeans(comp, 4, nstart=25, iter.max=1000)


library(RColorBrewer)
library(scales)
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot(comp, col=k$clust, pch=16)

#HCPC 
#http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/117-hcpc-hierarchical-clustering-on-principal-components-essentials/
#Case of continuous variables
res.pca <- PCA(df2, graph = FALSE)
res.hcpc <- HCPC(res.pca, graph = FALSE)
plot(res.hcpc,choice='tree')
plot(res.hcpc,choice='choice')
plot(res.hcpc)




