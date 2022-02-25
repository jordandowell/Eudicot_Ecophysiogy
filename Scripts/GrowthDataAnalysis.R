#analyze the growth data
library("lubridate")
library(lattice)
library(ggplot2)
library(dplyr)
library(forcats)
#importdata

#Plant id info

PlantNames<- read.csv("./InputData/PLANTID2Genotype.csv")


#read in data

PlantData<-read.csv("./InputData/GrowthData.csv")



#merge files

PlantNameData<-merge(PlantNames,PlantData,by.x = "Plant.ID",by.y = "PlantID")

#make dates true dates

PlantNameData$Date<-as.Date(PlantNameData$Date, format = "%d-%b")




#plot of all species by day 
xyplot(numberofleaves ~ Day|Plant, data = PlantNameData,
       groups = PlantNameData$Genotype, pch = 16, cex = 0.5)
xyplot(AGR_plantheightcm ~ Day|Plant, data = PlantNameData,
       groups = PlantNameData$Genotype, pch = 16, cex = 0.5)





# Create a group-means data set
gd <- PlantNameData %>% 
  group_by(Plant.ID) %>% 
  summarise(
    MEAN_plantheightcm = mean(plantheightcm[plantheightcm !=0], na.rm = TRUE),
    MEAN_AGR_plantheightcm = mean(AGR_plantheightcm[AGR_plantheightcm != 0], na.rm = TRUE),
    MEAN_numberofleaves = mean(numberofleaves[numberofleaves !=0], na.rm = TRUE),
    MEAN_AGR_numberofleaves = mean(AGR_numberofleaves[AGR_numberofleaves != 0], na.rm = TRUE)
    
  )

#add other plant data 
Plant.gd<-merge(PlantNames,gd,by.x = "Plant.ID",by.y = "Plant.ID")


#relevel factor of genotype
Plant.gd$Genotype <- factor(Plant.gd$Genotype, levels = unique(Plant.gd$Genotype[order(Plant.gd$Plant)]))



# Plot both data sets



Plant.gd %>%
  ggplot(aes(col=Plant, y=MEAN_plantheightcm, x=Genotype) )+ 
  geom_point(position="dodge", stat = "identity") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


Plant.gd %>%
  ggplot(aes(col=Plant, y=MEAN_AGR_numberofleaves, x=Genotype) )+ 
  geom_point(position="dodge", stat = "identity") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))




