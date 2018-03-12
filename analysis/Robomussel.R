#load libraries
library(plyr)
library(dplyr)
library(reshape2)
library(tidyr)

library(ggplot2)

source("analysis\\TempcyclesAnalysis.R")

#ROBOMUSSEL ANALYSIS

#SITES
# WaOr Tatoosh Island, WA 1660.1 48.39 124.74
# WaOr Boiler Bay, OR 1260.7 44.83 124.05
# WaOr Strawberry Hill, OR 1196 44.25 124.12
# CenCal Hopkins, CA 327.1 36.62 121.90
# CenCal Piedras Blancas, CA 208.11 35.66 121.28
# CenCal Cambria, CA 185.66 35.54 121.10
# SoCal Lompoc, CA 84.175 34.72 120.61
# SoCal Jalama, CA 57.722 34.50 120.50
# SoCal Alegria, CA 37.284 34.47 120.28
# SoCal Coal Oil Point (COP), CA 0 34.41 119.88

#-----------------
#Site data
setwd( "C:\\Users\\Buckley\\Documents\\ClimateBiology\\")

site.dat= read.csv("data\\musselREADME.csv")

#Load robomussel data
te.max <- readRDS("data\\tedat.rds")
#te.max= read.csv("tedat.csv")

#Fix duplicate CP in WA
te.max$lat= as.character(te.max$lat)
te.max$site= as.character(te.max$site)
te.max[which(te.max$lat==48.45135),"site"]<-"CPWA"
te.max$site= as.factor(te.max$site)

#drop 1 of two close sites
te.max<- te.max[-which(te.max$site=="LB"),]

#----------------------
#PLOTS

#Time series
#clim2 = clim2 %>% group_by(Year,Site) %>% summarise(Min= mean(Min, na.rm=TRUE),Max= mean(Max, na.rm=TRUE),Mean= mean(Mean, na.rm=TRUE) )

#count by site, subsite, year
te.count = te.max %>% group_by(year,site, subsite) %>% summarise( count=length(MaxTemp_C)  )
te.count= as.data.frame(te.count)

#subset sites
te.max2= subset(te.max, te.max$site %in% c("SD","BB","PD") ) # "HS",
#te.max2= subset(te.max, te.max$lat %in% c(48.39137,44.83064,35.66582,34.46717) )

# USWACC	48.5494	-123.0059667	Colins Cove
# USWACP	48.45135	-122.9617833	Cattle Point
#* USWASD	48.39136667	-124.7383667	Strawberry Point

#* USORBB	44.83064	-124.06005	Boiler Bay

#* USCAPD	35.66581667	-121.2867167	Piedras
# USCAAG	34.46716667	-120.2770333	Alegria


#time series
te.max1= subset(te.max2, te.max2$year==2002)

#restrict to summer
#May 1 through September: 121:273 
te.max1= subset(te.max1, te.max1$doy>120 & te.max1$doy<274)

#ggplot(data=te.max1, aes(x=doy, y = MaxTemp_C, color=subsite ))+geom_line() +theme_bw()+facet_wrap(~site)
#by tidal height
#ggplot(data=te.max1, aes(x=doy, y = MaxTemp_C, color=height ))+geom_line() +theme_bw()+facet_wrap(~site)

#update labels
te.max1$labs= as.character(te.max1$site)
te.max1$labs[which(te.max1$labs=="BB")]<- "44.8° Boiler Bay, OR"
te.max1$labs[which(te.max1$labs=="PD")]<- "35.7° Piedras Blancas, CA"
te.max1$labs[which(te.max1$labs=="SD")]<- "48.4° Strawberry Point, WA"

#FIG 1A
#by lat
fig2a<- ggplot(data=te.max1, aes(x=doy, y = MaxTemp_C, color=subsite ))+geom_line(alpha=0.8) +theme_classic()+
  facet_wrap(~labs, nrow=1)+ guides(color=FALSE)+labs(x = "Day of year",y="Maximum daily temperature (°C)")

#------------------
#FREQUENCY
# https://github.com/georgebiogeekwang/tempcycles/

#power spectrum
#x: frequency (1/days)
#y: log amplitude

fseq= exp(seq(log(0.001), log(1), length.out = 200))

sites= c("SD","BB","PD") #levels(te.max2$site)
subsites=  levels(te.max2$subsite)

pow.out= array(NA, dim=c(length(sites),length(subsites),length(fseq) ) )

for(site.k in 1:length(sites))
{
  te.dat= te.max2[which(te.max2$site==sites[site.k]),]
  subsites1= levels(te.dat$subsite)
  
  for(subsite.k in  1:length(subsites)) {
    te.dat1= te.dat[which(te.dat$subsite==subsites1[subsite.k]),]
    
    pow.out[site.k, subsite.k,] <- spec_lomb_phase(te.dat1$MaxTemp_C, te.dat1$j, freq=fseq)$cyc_range
      }
}

dimnames(pow.out)[[1]]<- sites
dimnames(pow.out)[[2]]<- 1:75

#to long format
for(site.k in 1:length(sites)){
  pow1= pow.out[site.k,,]
  pow1= na.omit(pow1)
  pow1m= melt(pow1)
  pow1m$site= sites[site.k]

  if(site.k==1)pow=pow1m
  if(site.k>1)pow=rbind(pow,pow1m)
}

colnames(pow)[1:3]=c("subsite","freq","cyc_range")

#correct freq values
pow$freq= fseq[pow$freq]

#sort by frequency
pow= pow[order(pow$site, pow$subsite, pow$freq),]
pow$subsite= factor(pow$subsite)

#freq, amp plot

#add latitude
site.dat1=  te.max %>% group_by(site) %>% summarise( lat=lat[1],zone=zone[1],tidal.height..m.=tidal.height..m.[1],substrate=substrate[1] )
match1= match(pow$site, site.dat1$site)
pow$lat= site.dat1$lat[match1]

#update labels
pow$labs= as.character(pow$site)
pow$labs[which(pow$labs=="BB")]<- "44.8° Boiler Bay, OR"
pow$labs[which(pow$labs=="PD")]<- "35.7° Piedras Blancas, CA"
pow$labs[which(pow$labs=="SD")]<- "48.4° Strawberry Point, WA"

fig2b<- ggplot(data=pow, aes(x=log(freq), y = log(cyc_range/2) ))+geom_line(alpha=0.8, aes(color=subsite)) +theme_classic()+facet_wrap(~labs, nrow=1)+ guides(color=FALSE, size=FALSE)+
 geom_vline(xintercept=-2.639, color="gray")+geom_vline(xintercept=-1.946, color="gray")+geom_vline(xintercept=-3.40, color="gray")+geom_vline(xintercept=-5.9, color="gray")+
labs(x = "log (frequency) (1/days)",y="log (amplitude)")+
  annotate(geom="text", x=-2.1, y=-5.5, label="1 week", size=3, color="black",angle=90)+ 
  annotate(geom="text", x=-2.8, y=-5.5, label="2 weeks", size=3, color="black",angle=90)+ 
  annotate(geom="text", x=-3.6, y=-5.5, label="1 month", size=3, color="black",angle=90)+ 
  annotate(geom="text", x=-6.2, y=-5.5, label="1 year", size=3, color="black",angle=90)+ylim(range(-5.8,1.2))
  #add lines for 1 week, 2 week, month, year

# #save plots
# pdf("Fig2a.pdf",height = 4, width = 8)
# fig2a
# dev.off()
# 
# pdf("Fig2b.pdf",height = 4, width = 8)
# fig2b
# dev.off()

#----
setwd("C:\\Users\\Buckley\\Google Drive\\Buckley\\Work\\ICBClimateBiology\\figures\\") 
pdf("Fig2.pdf",height = 8, width = 10)
grid.newpage()
pushViewport(viewport(layout=grid.layout(2,1)))
vplayout<-function(x,y)
  viewport(layout.pos.row=x,layout.pos.col=y)
print(fig2a,vp=vplayout(1,1))
print(fig2b,vp=vplayout(2,1))
dev.off()

#===================================================
#Quilt plot

#round lat
te.max$lat= as.numeric(as.character(te.max$lat))
te.max$lat.lab= round(te.max$lat,2)

#mean daily maximum by month
te.month = te.max %>% group_by(lat, month, lat.lab) %>% summarise( max=max(MaxTemp_C), mean.max=mean(MaxTemp_C), q75= quantile(MaxTemp_C, 0.75), q95= quantile(MaxTemp_C, 0.95) ) 

fig2<- ggplot(te.month) + 
  aes(x = month, y = as.factor(lat.lab) ) + 
  geom_tile(aes(fill = mean.max)) + 
  coord_equal()+
  scale_fill_gradientn(colours = rev(heat.colors(10)), name="temperature (°C)" )+
  #scale_fill_distiller(palette="Spectral", na.value="white", name="max temperature (°C)") + 
  theme_classic(base_size = 18)+xlab("month")+ylab("latitude (°)")+ theme(legend.position="bottom")+ #+ coord_fixed(ratio = 4)
  geom_hline(yintercept = 7.5, color="white", lwd=2) +
  scale_x_continuous(breaks=seq(1,12,2))

pdf("Fig3.pdf",height = 8, width = 8)
fig2
dev.off()

#==================================================
# EXTREMES

library(ismev) #for gev
library(reshape)
library(maptools) #for mapping
library(evd) #for extremes value distributions
library(extRemes)
library(fExtremes) # generate gev

sites= levels(te.max$site) #c("SD","BB","PD")
sites= sites[-which(sites=="LB")]
subsites=  levels(te.max$subsite)

gev.out= array(NA, dim=c(length(sites),length(subsites),13 ) )

for(site.k in 1:length(sites))
{
  te.dat= te.max[which(te.max$site==sites[site.k]),]
  subsites1= levels(te.dat$subsite)
  
  for(subsite.k in  1:length(subsites)) {
    te.dat1= te.dat[which(te.dat$subsite==subsites1[subsite.k]),]
    
    #add site data
    gev.out[site.k, subsite.k,12]= te.dat1$lat[1]
    #gev.out[site.k, subsite.k,13]= te.dat1$height[1]
    
    #Generalized extreme value distribution
      dat1= na.omit(te.dat1$MaxTemp_C)  ##CHECK na.omit appropraite?
      
    if(length(dat1)>365){
        
    mod.gev<- try(gev.fit(dat1, show=FALSE) ) #stationary
    if(class(mod.gev)!="try-error") gev.out[site.k, subsite.k,1]<-mod.gev$nllh
    if(class(mod.gev)!="try-error") gev.out[site.k, subsite.k,2:4]<-mod.gev$mle #add another for non-stat
    if(class(mod.gev)!="try-error") gev.out[site.k, subsite.k,5]<-mod.gev$conv #add another for non-stat
    
    #Generalized pareto distribution, for number of times exceeds threshold
    thresh= 35
    
    #stationary
    mod.gpd <- try(gpd.fit(dat1, thresh, npy=365)) #stationary 
    if(class(mod.gpd)!="try-error") gev.out[site.k, subsite.k,6]<-mod.gpd$rate
    
   ## nonstationary 
   # try(mod.gpd<-gpd.fit(dat1, 40, npy=92, ydat=as.matrix(te.dat1$year), sigl=1),silent = FALSE) 
    
    #RETURN LEVELS:  MLE Fitting of GPD - package extRemes
    mpers= c(10,20,50,100)
    for(m in 1:length(mpers)){
      pot.day<- try( fpot(dat1, threshold=35, npp=365.25, mper=mpers[m], std.err = FALSE) )
    
      if(class(pot.day)!="try-error") gev.out[site.k, subsite.k,6+m]=pot.day$estimate[1]
    }
    
    #proportion above threshold
    if(class(pot.day)!="try-error") gev.out[site.k, subsite.k,11]=pot.day$pat
    
    } #end check time series
  } #end subsites
} #end sites

#-------------------------
#PLOT
pow.out=gev.out

dimnames(pow.out)[[1]]<- sites
dimnames(pow.out)[[2]]<- 1:19
dimnames(pow.out)[[3]]<- c("gev.nllh", "gev.loc", "gev.scale", "gev.shape", "conv", "rate", "return10", "return20", "return50", "return100","pat","lat","height")

#to long format
for(site.k in 1:length(sites)){
  pow1= pow.out[site.k,,]
  #pow1= na.omit(pow1)
  pow1m= melt(pow1)
  pow1m$site= sites[site.k]
  
  if(site.k==1)pow=pow1m
  if(site.k>1)pow=rbind(pow,pow1m)
}

#--------------------
# ADD SITE INFO
names(pow)[1:2]=c("subsite","var")

pow$ssite= paste(pow$site,pow$subsite, sep=".")

pow.site= subset(pow, pow$var=="lat")
pow.site= pow.site[!duplicated(pow.site$ssite),]

match1= match(pow$ssite, pow.site$ssite)
pow$lat= pow.site$value[match1]

##fix duplicate site in WA
#te.max[which(te.max$lat==48.45135),"site"]<-"CPWA"


#====================
## PLOT

#dimnames(pow.out)[[3]]<- c("gev.nllh", "gev.loc", "gev.scale", "gev.shape", "conv", "rate", "return10", "return20", "return50", "return100","pat", "lat","height")

pow1= pow[pow$var %in% c("gev.loc", "gev.scale", "gev.shape", "pat", "return100"),]

#get rid of return100 outlier for ploting purposes
pow1[which(pow1$value>300),"value"]<- NA

#revise labels
pow1$var <- factor(pow1$var, labels = c("location", "scale", "shape", "percent above threshold", "100 year return"))
#round lat
pow1$lat= as.numeric(as.character(pow1$lat))
pow1$lat.lab= round(pow1$lat,2)

#fix value
pow1$value= as.numeric(as.character(pow1$value))

#drop NAs
pow1= pow1[which(!is.na(pow1$lat.lab)),]

#ggplot(data=pow1, aes(x=site, y = value, color=subsite))+geom_point()+theme_bw()+facet_wrap(~var, scales="free_y")
fig4= ggplot(data=pow1, aes(x=as.factor(lat.lab), y = value, color=subsite))+geom_point()+
  theme_bw()+facet_wrap(~var, scales="free_y")+ guides(color=FALSE)+xlab("latitude (°)")+ylab("metric value")+ theme(axis.text.x = element_text(size=8, angle=90))+
  geom_vline(xintercept = 7.5, color="black", lwd=1) 


#as factor not latitude
#+theme(axis.text.x=element_blank())

pdf("Fig4.pdf",height = 4, width = 8)
fig4
dev.off()








