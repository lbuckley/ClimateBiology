#load libraries
library(plyr)
library(dplyr)
library(reshape2)
library(tidyr)
library(grid)

library(ggplot2)

#BIOTRAITS ANALYSIS

setwd("C:\\Users\\lbuckley\\Documents\\ClimateBiology\\data")
traits= read.csv("Delletal2013.csv")

#temp distribution
hist(traits$AmbientTemp, n=30)

#FIG A
#fig3a<- ggplot(traits, aes(x=AmbientTemp, color=TraitOrg)) + geom_histogram(fill="white", alpha=0.5, position="identity")+theme_bw()+ theme(legend.position="none")

fig3a<- ggplot(traits, aes(x=AmbientTemp, fill=TraitOrg, color=TraitOrg)) +
  geom_density(alpha=0.1)+theme_classic()+ theme(legend.position="none")+ labs(x = "Measurement temperature (°C)")
# , size=0.5

#stats by series
traits2 = traits %>% group_by(DataSeriesID) %>% summarise(TempMean= mean(AmbientTemp),TempN= length(AmbientTemp),TempRange= range(AmbientTemp)[2]-range(AmbientTemp)[1], TimeMean=mean(ObsTimeValueSI),TimeUnit=ObsTimeUnitSI[1], LabField=Labfield[1] )
traits3 = traits %>% group_by(DataSeriesID, TraitOrg) %>% summarise(TempMean= mean(AmbientTemp),TempN= length(AmbientTemp),TempRange= range(AmbientTemp)[2]-range(AmbientTemp)[1], TimeMean=mean(ObsTimeValueSI),TimeUnit=ObsTimeUnitSI[1], LabField=Labfield[1] )

#Temp
hist1= hist(traits2$TempN,n=250)
plot(hist1, xlim=c(0,10))

hist(traits2$TempRange)
#make integer
traits3$TempN= as.integer(traits3$TempN)

#FIG B
fig3b<- ggplot(traits3, aes(x=TempN, fill=TraitOrg, color=TraitOrg)) +
  geom_density(alpha = 0.1, adjust=1)+theme_classic()+ labs(x = "Number of measurement temperatures", colour="trait type",fill="trait type", y="")+ guides(size=FALSE)+
  scale_x_continuous(breaks=seq(0,12,2), lim=c(0,12))
#aes(size=1)

#Time
traits4= subset(traits2, traits2$TimeUnit=="second")
hist1= hist(traits4$TimeMean/60, n=10000)
plot(hist1, xlim=c(0,10))

#STATS IN TEXT?
ggplot(traits4, aes(x=TimeMean/60)) +
  geom_histogram(fill="gray", alpha=0.5, position="identity")+theme_bw()

#-----------------------
#FIG 3

multiplot(fig3a, fig3b, cols=2)
cbind(fig3a, fig3b)

#=========================================

setwd("C:\\Users\\lbuckley\\Desktop\\Fall2017\\ICBClimBio\\")
pdf("Fig5.pdf", height=4, width=8)
#tiff("Fig5.tiff", res = 300) #height=400, width=800, 

#plot
grid.newpage()
pushViewport(viewport(layout=grid.layout(1,2, widths=c(0.5,0.7) )))
vplayout<-function(x,y)
  viewport(layout.pos.row=x,layout.pos.col=y)
print(fig3a,vp=vplayout(1,1))
print(fig3b,vp=vplayout(1,2))

dev.off()

