#SCRIPT3: PCA
library(factoextra)
library(missMDA)
library(corrplot)
library(ggfortify)
library(BayesFactor)
library(ggpubr)
library(dplyr)
library(vtable)
library(psych)
#import data


LES_Traits<-PlantNameData[,c(2:6,13,16:18,19,22:25)]


LES_Traits<-LES_Traits[!is.na(LES_Traits$LMAmgmm), ]
View(LES_Traits)


#PCA of LES TRaits


LES_Traits.PCA <- prcomp(LES_Traits[,c(3:6,10)], center = T, scale. = T)
PCA.IND <-
  fviz_pca_ind(
    LES_Traits.PCA,
    geom = "point",
    axes = c(1, 2),
    label = "none",
    habillage = LES_Traits$Genotype.x,
    mean.point = F
  )




#produce PCA biplot it combines the variable loading plot and individuals

#LES PCA
fviz_pca_biplot(LES_Traits.PCA, label = "var", habillage=LES_Traits$Plant.x,invisible = "quali",
                addEllipses=F, ellipse.level=0.95,repel = T,pointsize = 4, geom = "point",
                ggtheme = theme_minimal())+  scale_shape_manual(values=c(rep(16,7)))


#PCA of isolate lesion differences 

LES_Traits.PCA <- prcomp(LES_Traits[,c(7:9)], center = T, scale. = T)
PCA.IND <-
  fviz_pca_ind(
    LES_Traits.PCA,
    geom = "point",
    axes = c(1, 2),
    label = "none",
    habillage = LES_Traits$Genotype.x,
    mean.point = F
  )


fviz_pca_biplot(LES_Traits.PCA, label = "var", habillage=LES_Traits$Plant.x,invisible = "quali",
                addEllipses=F, ellipse.level=0.95,repel = T,pointsize = 4, geom = "point",
                ggtheme = theme_minimal())+  scale_shape_manual(values=c(rep(16,7)))

