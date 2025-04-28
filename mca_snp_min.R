install.packages("FactoMineR")
install.packages("vcd")
install.packages("factoextra")

library(FactoMineR)
library(vcd)
library(factoextra)
library(tidyverse)
library(svglite)
library(corrplot) #visualize the cos2 of row categories on all the dimensions 

#wtvs.snp
data1 <- read.csv("pca_wt_snp.csv")
data1 <- as_tibble(data1)

#select data
data2 <- data1[ , 3:11]

#mca
data2_mca <- MCA(data2, 
                     ncp = 5, 
                     graph = FALSE)
print(data2_mca)

## visualize

#eigenvalues
eig_1<- fviz_screeplot(data2_mca, addlabels = TRUE, ylim = c(0, 45)
               , ggtheme = theme_classic())

#biplot
fviz_mca_biplot(data2_mca, 
                repel = TRUE, 
                ggtheme = theme_classic())

#graph of variables
#results
var <- get_mca_var(data2_mca)
var

#Correlation between variables and principal dimensions
cor_1<- fviz_mca_var(data2_mca, choice = "mca.cor", 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_classic())

#Coordinates of variable categories
head(round(var$coord, 2), 10)
coor_1<- fviz_mca_var(data2_mca, 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_classic())

#Quality of representation of variable categories
head(var$cos2, 4)
# Color by cos2 values: quality on the factor map
fviz_mca_var(data2_mca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # Avoid text overlapping
             ggtheme = theme_classic())
#corrplot
corrplot(var$cos2, is.corr=FALSE)
# Cos2 of variable categories on Dim.1 and Dim.2
cos2_1<- fviz_cos2(data2_mca, choice = "var", axes = 1:2, ggtheme = theme_classic())

cos2_1
#Contribution of variable categories to the dimensions
head(round(var$contrib,2), 4)
# Contributions of rows to dimension 1
con_d1_1 <-fviz_contrib(data2_mca, choice = "var", axes = 1, top = 5,  ggtheme = theme_classic())
# Contributions of rows to dimension 2
con_d2_1 <- fviz_contrib(data2_mca, choice = "var", axes = 2, top = 5,  ggtheme = theme_classic())
con_d2_1
# Total contribution to dimension 1 and 2
fviz_contrib(data2_mca, choice = "var", axes = 1:2, top = 15)
#scatterplot
fviz_mca_var(data2_mca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # avoid text overlapping (slow)
             ggtheme = theme_minimal()
)

##graph of individuals
ind <- get_mca_ind(data2_mca)
ind
# Coordinates of column points
head(ind$coord)
# Quality of representation
head(ind$cos2)
# Contributions
head(ind$contrib)

#Plots: quality and contribution
fviz_mca_ind(data2_mca, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE, # Avoid text overlapping (slow if many points)
             ggtheme = theme_minimal())
# Cos2 of individuals
fviz_cos2(data2_mca, choice = "ind", axes = 1:2, top = 20)
# Contribution of individuals to the dimensions
fviz_contrib(data2_mca, choice = "ind", axes = 1:2, top = 20)

#MCA plot with grouping
groups <- as.factor(data1$type[1:90]) #identify groups
pca_plot1 <- fviz_mca_ind(data2_mca,
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = groups, # color by groups
             addEllipses = TRUE, # Concentration ellipses
             ellipse.type = "confidence",
             palette = c("#F8766D","#A3A500", "#00BF7D"),
             legend.title = "Groups",
             repel = TRUE,
             ggtheme = theme_classic()
             )

pca_plot1

##################
#wtvs.homo/hetero
data3 <- read.csv("pca_homo_hetero.csv")
data3 <- as_tibble(data3)

#select data
data4 <- data3[ , 3:11]

#mca
data4_mca <- MCA(data4, 
                 ncp = 5, 
                 graph = FALSE)
print(data4_mca)

## visualize

#eigenvalues
eig_2 <- fviz_screeplot(data4_mca, addlabels = TRUE, ylim = c(0, 45),
               ggtheme = theme_classic())

#biplot
fviz_mca_biplot(data4_mca, 
                repel = TRUE, 
                ggtheme = theme_classic())

#graph of variables
#results
var2 <- get_mca_var(data4_mca)
var2

#Correlation between variables and principal dimensions
cor_2<- fviz_mca_var(data4_mca, choice = "mca.cor", 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_classic())

#Coordinates of variable categories
head(round(var2$coord, 2), 10)
coor_2 <- fviz_mca_var(data4_mca, 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_classic())

#Quality of representation of variable categories
head(var2$cos2, 4)
# Color by cos2 values: quality on the factor map
fviz_mca_var(data4_mca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # Avoid text overlapping
             ggtheme = theme_classic())
#corrplot
corrplot(var2$cos2, is.corr=FALSE)
# Cos2 of variable categories on Dim.1 and Dim.2
cos2_2<- fviz_cos2(data4_mca, choice = "var", axes = 1:2, ggtheme = theme_classic())
cos2_2
#Contribution of variable categories to the dimensions
head(round(var2$contrib,2), 4)
# Contributions of rows to dimension 1
con_d1_2<- fviz_contrib(data4_mca, choice = "var", axes = 1, top = 5,  ggtheme = theme_classic())
# Contributions of rows to dimension 2
con_d2_2<- fviz_contrib(data4_mca, choice = "var", axes = 2, top = 5,  ggtheme = theme_classic())
# Total contribution to dimension 1 and 2
fviz_contrib(data4_mca, choice = "var", axes = 1:2, top = 15)
#scatterplot
fviz_mca_var(data4_mca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # avoid text overlapping (slow)
             ggtheme = theme_classic()
)

##graph of individuals
ind2 <- get_mca_ind(data4_mca)
ind2
# Coordinates of column points
head(ind2$coord)
# Quality of representation
head(ind2$cos2)
# Contributions
head(ind2$contrib)

#Plots: quality and contribution
fviz_mca_ind(data4_mca, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE, # Avoid text overlapping (slow if many points)
             ggtheme = theme_classic())
# Cos2 of individuals
fviz_cos2(data4_mca, choice = "ind", axes = 1:2, top = 20)
# Contribution of individuals to the dimensions
fviz_contrib(data4_mca, choice = "ind", axes = 1:2, top = 20)


#MCA plot with grouping
groups_2 <- as.factor(data3$type[1:90]) #identify groups
pca_plot2 <- fviz_pca_ind(data4_mca,
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = groups_2, # color by groups
             addEllipses = TRUE, # Concentration ellipses
             ellipse.type = "confidence",
             palette = c("#F8766D","#A3A500", "#00BF7D"),
             legend.title = "Groups",
             repel = TRUE,
             ggtheme = theme_classic()
              )
print(pca_plot2)
#eig.val <- get_eigenvalue(data4_mca)
#fviz_eig(data2_mca, addlabels = TRUE)

#export svg
svglite("eig_1.svg", width = 5, height = 5)
eig_1
dev.off()

svglite("cor_1.svg", width = 5, height = 5)
cor_1
dev.off()

svglite("coor_1.svg", width = 5, height = 5)
coor_1
dev.off()

svglite("cos2_1.svg", width = 5, height = 5)
cos2_1
dev.off()

svglite("con_d1_1.svg", width = 5, height = 5)
con_d1_1
dev.off()

svglite("con_d2_1.svg", width = 5, height = 5)
con_d2_1
dev.off()

svglite("pca_snp_v2.svg", width = 5, height = 5)
pca_plot1
dev.off()


svglite("eig_2.svg", width = 5, height = 5)
eig_2
dev.off()

svglite("cor_2.svg", width = 5, height = 5)
cor_2
dev.off()

svglite("coor_2.svg", width = 5, height = 5)
coor_2
dev.off()

svglite("cos2_2.svg", width = 5, height = 5)
cos2_2
dev.off()

svglite("con_d1_2.svg", width = 5, height = 5)
con_d1_2
dev.off()

svglite("con_d2_2.svg", width = 5, height = 5)
con_d2_2
dev.off()

svglite("pca_homo_hetero_v2.svg", width = 5, height = 5)
pca_plot2
dev.off()
