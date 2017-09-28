#load libraries
library(plyr)
library(dplyr)
library(reshape2)
library(tidyr)

library(ggplot2)

#BIOTRAITS ANALYSIS

setwd("C:\\Users\\lbuckley\\Documents\\ClimateBiology\\data")
traits= read.csv("Delletal2013.csv")

#temp distribution
hist(traits$AmbientTemp, n=30)

#FIG A
ggplot(traits, aes(x=AmbientTemp, color=TraitOrg)) +
  geom_histogram(fill="white", alpha=0.5, position="identity")+theme_bw()

#stats by series
traits2 = traits %>% group_by(DataSeriesID) %>% summarise(TempMean= mean(AmbientTemp),TempN= length(AmbientTemp),TempRange= range(AmbientTemp)[2]-range(AmbientTemp)[1], TimeMean=mean(ObsTimeValueSI),TimeUnit=ObsTimeUnitSI[1], LabField=Labfield[1] )
traits3 = traits %>% group_by(DataSeriesID, TraitOrg) %>% summarise(TempMean= mean(AmbientTemp),TempN= length(AmbientTemp),TempRange= range(AmbientTemp)[2]-range(AmbientTemp)[1], TimeMean=mean(ObsTimeValueSI),TimeUnit=ObsTimeUnitSI[1], LabField=Labfield[1] )

#Temp
hist1= hist(traits2$TempN,n=250)
plot(hist1, xlim=c(0,10))

hist(traits2$TempRange)

#FIG B
ggplot(traits3, aes(x=TempN, color=TraitOrg)) +
  geom_histogram(fill="white", alpha=0.5, position="identity")+xlim(0,10)+theme_bw()

#Time
traits4= subset(traits2, traits2$TimeUnit=="second")
hist1= hist(traits4$TimeMean/60, n=10000)
plot(hist1, xlim=c(0,10))

#STATS IN TEXT?
ggplot(traits4, aes(x=TimeMean/60)) +
  geom_histogram(fill="gray", alpha=0.5, position="identity")+theme_bw()
