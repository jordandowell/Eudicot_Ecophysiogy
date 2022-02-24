#analyze the growth data
library("lubridate")
library(lattice)
library(ggplot2)
library(dplyr)
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
xyplot(RGR_numberofleaves ~ Day|Plant, data = PlantNameData,
       groups = Genotype, pch = 16, cex = 0.5)


#plot summary of relative growth rate across species 
plot(PlantNameData$Day,PlantNameData$plantheightcm)



# Create a group-means data set
gd <- PlantNameData %>% 
  group_by(Genotype) %>% 
  summarise(
    VAR1 = mean(plantheightcm),
    VAR2 = mean(RGR_plantheightcm)
  )

# Plot both data sets
ggplot(PlantNameData) +
  geom_*() +
  geom_*(data = gd)


ggplot(gd, aes(x = Genotype, y = VAR2))# +
  geom_bar(stat = "identity")





