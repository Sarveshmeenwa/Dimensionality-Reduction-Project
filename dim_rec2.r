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


df = read_csv("H:/Downloads/Datatsets/users.db.csv")
head(df)

colnames(df)

df_cont <- df[c('score', 'n.matches', 'n.updates.photo', 'n.photos')]

# mpg
#ggqqplot(df$score, ylab = "score")
# wt
#ggqqplot(df$n.matches, ylab = "n.matches")

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


######################
######PCA#############

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



fviz_pca_var(cont_df.pca, col.var = "cos2",
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

fviz_pca_ind(cont_df.pca, col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE ,# Avoid text overlapping (slow if many points),
             select.ind = list(contrib = 250)
)

fviz_pca_biplot(cont_df.pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969" # Individuals color
)
fviz_pca_biplot(cont_df.pca,
                col.ind = df$gender, palette = "jco",
                addEllipses =FALSE, label = "var",
                col.var = "black", repel = TRUE,
                legend.title = "Gender",
                select.ind = list(contrib = 250))

#select top 30 individuals
fviz_pca_biplot(cont_df.pca, label="var",
                select.ind = list(contrib = 500))



#scree plot
fviz_eig(cont_df.pca, addlabels = TRUE, ylim = c(0, 50))
fviz_eig(cont_df.pca,choice="eigenvalue", addlabels = TRUE, ylim = c(0, 50))
fviz_eig(cont_df.pca, choice = "eigenvalue", 
         addlabels=TRUE)

library('xtable')
print(xtable(results$rotation))
