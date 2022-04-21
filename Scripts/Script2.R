#Script 2 ecophys Comparison


#analyze the growth data
library("lubridate")
library(lattice)
library(ggplot2)
library(dplyr)
library(forcats)
library(ggpubr)
#importdata

#Plant id info

PlantNames<- read.csv("./InputData/PLANTID2Genotype.csv")


#read in data

PlantAreaData<-read.csv("./InputData/Masses.csv")



#merge files

PlantNameData<-merge(PlantNames,PlantAreaData,by.x = "Plant.ID",by.y = "PlantID")



PlantNameData<-merge(PlantNameData,Plant.gd,by.x = "Plant.ID",by.y = "Plant.ID")


PlantNameData<-PlantNameData[!is.na(PlantNameData$LMAmgmm), ]
PlantNameData$LMAmgmm<-as.numeric(PlantNameData$LMAmgmm)

#plot of all species by day 
xyplot(LMAmgmm ~ X944Areacm|Plant.x, data = PlantNameData,
       groups = PlantNameData$Genotype.x, pch = 1, cex = 1.5)
xyplot(DryMassg ~ NROTareapix|Plant.x, data = PlantNameData,
       groups = PlantNameData$Genotype.x, pch = 16, cex = 0.5)



str(PlantNameData)
#make sure lma is numeric
PlantNameData$LMA.gperpix.<-as.numeric(PlantNameData$LMA.gperpix.)
PlantNameData<-PlantNameData[!is.na(PlantNameData$LMA.gperpix.), ]


#change quotations per species
species<-PlantNameData[PlantNameData$Plant.x=="CommBean", ]

cor.test(species$WatercontentPercent,species$PepperAreacm)


#overall correlations 
cor.test(PlantNameData$WatercontentPercent,PlantNameData$PepperAreacm)


# Create a group-means data set
gd <- PlantNameData %>% 
  group_by(Genotype.x) %>% 
  summarise(
    MEAN_Pepper_lesionsize = mean(PepperAreacm[PepperAreacm !=0], na.rm = TRUE),
    MEAN_944__lesionsize = mean(X944Areacm[X944Areacm != 0], na.rm = TRUE),
    MEAN_NROT_lesionsize = mean(NROTareapix[NROTareapix !=0], na.rm = TRUE),
   # MEAN_AGR_numberofleaves = mean(AGR_numberofleaves[AGR_numberofleaves != 0], na.rm = TRUE)
    
  )

#get set of genotypes and species
Names4Lesionsarea<-unique(PlantNames[c("Plant", "Genotype")])

#add other plant data 
Lesionarea.gd<-merge(Names4Lesionsarea,gd,by.x = "Genotype",by.y = "Genotype.x")


#relevel factor of genotype
PlantNameData$Genotype.x <- factor(PlantNameData$Genotype.x, levels = unique(PlantNameData$Genotype.x[order(PlantNameData$Plant.x)]))



# Plot both data sets
#empasize that you just complied this data 

#NROTareacm smallest lesion on average
PlantNameData %>%
  ggplot(aes(col=Plant.x, y=NROTareacm, x=Genotype.x) )+
  geom_point(position="dodge", stat = "identity",size=3) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


#X944Areacm middle lesion on average
PlantNameData %>%
  ggplot(aes(col=Plant.x, y=X944Areacm, x=Genotype.x) )+
  geom_point(position="dodge", stat = "identity",size=3) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#PepperAreacm largest lesion on average
PlantNameData %>%
  ggplot(aes(col=Plant.x, y=PepperAreacm, x=Genotype.x) )+
  geom_point(position="dodge", stat = "identity",size=3) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))




# PCA 






