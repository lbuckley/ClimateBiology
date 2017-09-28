#load libraries
library(plyr)
library(dplyr)
library(reshape2)
library(tidyr)

library(ggplot2)

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
setwd("C:\\Users\\lbuckley\\Documents\\ClimateBiology\\data\\robomussel\\")
site.dat= read.csv("README.csv")

my.read.table= function(x) {
  dat= read.table(x, row.names=NULL)
  dat$id= gsub(".txt","",x) 
  return(dat)}

#WA
#CC
setwd("C:\\Users\\lbuckley\\Documents\\ClimateBiology\\data\\robomussel\\WA (Washington)\\CC (Colins Cove)\\")
file_names <- dir() #where you have your files
te.cc <- do.call(rbind,lapply(file_names,my.read.table))

#LB
setwd("C:\\Users\\lbuckley\\Documents\\ClimateBiology\\data\\robomussel\\WA (Washington)\\LB (Landing Beach)\\")
file_names <- dir() #where you have your files
te.lb <- do.call(rbind,lapply(file_names,my.read.table))

#CP
setwd("C:\\Users\\lbuckley\\Documents\\ClimateBiology\\data\\robomussel\\WA (Washington)\\CP (Cattle Point)\\")
file_names <- dir() #where you have your files
te.cp <- do.call(rbind,lapply(file_names,my.read.table))

#SD
setwd("C:\\Users\\lbuckley\\Documents\\ClimateBiology\\data\\robomussel\\WA (Washington)\\SD (Strawberry Point)\\")
file_names <- dir() #where you have your files
te.sd <- do.call(rbind,lapply(file_names,my.read.table))

#combine
te.wa= rbind(te.cc, te.lb, te.cp,te.sd)

#----
#OR
#BB
setwd("C:\\Users\\lbuckley\\Documents\\ClimateBiology\\data\\robomussel\\OR (Oregon)\\BB (Boiler Bay)\\")
file_names <- dir() #where you have your files
te.bb <- do.call(rbind,lapply(file_names,my.read.table))

#SH
setwd("C:\\Users\\lbuckley\\Documents\\ClimateBiology\\data\\robomussel\\OR (Oregon)\\SH (Strawberry Hill)\\")
file_names <- dir() #where you have your files
te.sh <- do.call(rbind,lapply(file_names,my.read.table))

#combine
te.or= rbind(te.bb, te.sh)

#---
#CA

#HS
setwd("C:\\Users\\lbuckley\\Documents\\ClimateBiology\\data\\robomussel\\CA (California)\\HS (Hopkins)\\")
file_names <- dir() #where you have your files
te.hs <- do.call(rbind,lapply(file_names,my.read.table))

#PD
setwd("C:\\Users\\lbuckley\\Documents\\ClimateBiology\\data\\robomussel\\CA (California)\\PD (Piedras)\\")
file_names <- dir() #where you have your files
te.pd <- do.call(rbind,lapply(file_names,my.read.table))

#CA
setwd("C:\\Users\\lbuckley\\Documents\\ClimateBiology\\data\\robomussel\\CA (California)\\CA (Cambria)\\")
file_names <- dir() #where you have your files
te.ca <- do.call(rbind,lapply(file_names,my.read.table))

#LL (or LS?)
setwd("C:\\Users\\lbuckley\\Documents\\ClimateBiology\\data\\robomussel\\CA (California)\\LL (Lompoc Landing)\\")
file_names <- dir() #where you have your files
te.ll <- do.call(rbind,lapply(file_names,my.read.table))

#JA
setwd("C:\\Users\\lbuckley\\Documents\\ClimateBiology\\data\\robomussel\\CA (California)\\JA (Jalama)\\")
file_names <- dir() #where you have your files
te.ja <- do.call(rbind,lapply(file_names,my.read.table))

#AG
setwd("C:\\Users\\lbuckley\\Documents\\ClimateBiology\\data\\robomussel\\CA (California)\\AG (Alegria)\\")
file_names <- dir() #where you have your files
te.ag <- do.call(rbind,lapply(file_names,my.read.table))

#CP
setwd("C:\\Users\\lbuckley\\Documents\\ClimateBiology\\data\\robomussel\\CA (California)\\CP (Coal oil point)\\")
file_names <- dir() #where you have your files
te.cp <- do.call(rbind,lapply(file_names,my.read.table))

#combine
te.ca= rbind(te.hs, te.pd, te.ca, te.ll, te.ja, te.ag, te.cp)

#combine all
te.wa$state="WA"
te.or$state="OR"
te.ca$state="CA"

te.wa= rbind(te.wa,te.or,te.ca)

#----------------------------

#extract sites and numbers
te.wa$id1= gsub("BMRMUS","",te.wa$id)
te.wa$site= as.factor( substr(te.wa$id1, 3, 4) )

#extract subsite
te.wa$subsite=  substr(te.wa$id1, 5, 6)
te.wa$subsite= gsub("_","",te.wa$subsite)
te.wa$subsite= as.factor(te.wa$subsite)

te.wa$date= te.wa$row.names

#find daily max
te.max= te.wa %>% group_by(date, site, subsite) %>% summarise(id=id[1],MaxTemp_C= max(Temp_C) ) #, lat=lat[1], height=tidal.height..m.[1]

day=  as.POSIXlt(te.max$date, format="%m/%d/%Y")
te.max$doy=as.numeric(strftime(day, format = "%j"))
te.max$year=as.numeric(strftime(day, format = "%Y"))
te.max$month=as.numeric(strftime(day, format = "%m"))
te.max$j=julian(day)

#add latitude
site.match= vapply(strsplit(te.max$id,"_"), `[`, 1, FUN.VALUE=character(1))
 
match1= match(site.match, site.dat$microsite.id) #site.dat$site
te.max$lat= site.dat$latitude[match1]
te.max$zone= site.dat$zone[match1]
te.max$tidal.height..m.= site.dat$tidal.height..m.[match1]
te.max$substrate= site.dat$substrate[match1]

#----------------------
#PLOTS

#Time series
#clim2 = clim2 %>% group_by(Year,Site) %>% summarise(Min= mean(Min, na.rm=TRUE),Max= mean(Max, na.rm=TRUE),Mean= mean(Mean, na.rm=TRUE) )

#count by site, subsite, year
te.count = te.max %>% group_by(year,site, subsite) %>% summarise( count=length(MaxTemp_C)  )
te.count= as.data.frame(te.count)

#time series
te.max1= subset(te.max, te.max$year==2002)
ggplot(data=te.max1, aes(x=doy, y = MaxTemp_C, color=subsite ))+geom_line() +theme_bw()+facet_wrap(~site)
#by tidal height
ggplot(data=te.max1, aes(x=doy, y = MaxTemp_C, color=height ))+geom_line() +theme_bw()+facet_wrap(~site)
#by lat
ggplot(data=te.max1, aes(x=doy, y = MaxTemp_C, color=subsite ))+geom_line() +theme_bw()+facet_wrap(~lat)


#------------------
#FREQUENCY
# https://github.com/georgebiogeekwang/tempcycles/

#power spectrum
#x: frequency (1/days)
#y: log amplitude

fseq= exp(seq(log(0.001), log(1), length.out = 400))

#need to incorporate within site variation
te.dat= te.max[which(te.max$site=="SD" & te.max$subsite=="2"),]

#te.max.lomb <- spec_lomb_phase(te.dat$MaxTemp_C, te.dat$doy, freq=seq(0,1,0.01))
te.max.lomb <- spec_lomb_phase(te.dat$MaxTemp_C, te.dat$j, freq=fseq)

#power series by site and subsite
#te.lomb = te.max %>% group_by(site, subsite) %>% summarise( freq=spec_lomb_phase(te.max$MaxTemp_C, te.max$j, freq=fseq)  )

sites= levels(te.max$site)
subsites=  levels(te.max$subsite)

pow.out= array(NA, dim=c(length(sites),length(subsites),length(fseq) ) )

for(site.k in 1:length(sites))
{
  te.dat= te.max[which(te.max$site==sites[site.k]),]
  subsites1= levels(te.dat$subsite)
  
  for(subsite.k in  1:length(subsites)) {
    te.dat1= te.dat[which(te.dat$subsite==subsites1[subsite.k]),]
    
    pow.out[site.k, subsite.k,] <- spec_lomb_phase(te.dat1$MaxTemp_C, te.dat1$j, freq=fseq)$cyc_range
      }
}

dimnames(pow.out)[[1]]<- sites
dimnames(pow.out)[[2]]<- 1:75

#failed attempt to use plyr or apply
#make wrapper function
#dat= te.max[,c("MaxTemp_C","j", "site","subsite")]
#lomb= function(dat){ spec_lomb_phase(dat[,1], dat[,2], freq=fseq) }

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
plot(te.max.lomb$freq, te.max.lomb$cyc_range/2, type="l", log="xy")

ggplot(data=pow, aes(x=log(freq), y = log(cyc_range/2), color=subsite))+geom_line() +theme_bw()+facet_wrap(~site)

#add latitude
site.dat1=  te.max %>% group_by(site) %>% summarise( lat=lat[1],zone=zone[1],tidal.height..m.=tidal.height..m.[1],substrate=substrate[1] )
match1= match(pow$site, site.dat1$site)
pow$lat= site.dat1$lat[match1]

ggplot(data=pow, aes(x=log(freq), y = log(cyc_range/2), color=subsite))+geom_line() +theme_bw()+facet_wrap(~lat)

#===================================================
#Quilt plot

#mean daily maximum by month
te.month = te.max %>% group_by(site, month) %>% summarise( max=max(MaxTemp_C), mean.max=mean(MaxTemp_C), q75= quantile(MaxTemp_C, 0.75), q95= quantile(MaxTemp_C, 0.95) ) 

ggplot(te.month) + 
  aes(x = month, y = site, z = mean.max, fill = mean.max) + 
  geom_tile() + 
  coord_equal() +
  scale_fill_distiller(palette="Spectral", na.value="white", name="temperature (°C)") + 
  theme_bw(base_size = 18)+xlab("month")+ylab("site")+ theme(legend.position="right")+ theme(legend.position="right")+ coord_fixed(ratio = 4)

#==================================================
# EXTREMES

library(ismev) #for gev
library(reshape)
library(maptools) #for mapping
library(evd) #for extremes value distributions
library(extRemes)
library(fExtremes) # generate gev

#From PTRS Fig 5

sites= levels(te.max$site)
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
    gev.out[site.k, subsite.k,13]= te.dat1$height[1]
    
    #Generalized extreme value distribution
      dat1= na.omit(te.dat1$MaxTemp_C)  ##CHECK na.omit appropraite?
      
    if(length(dat1)>365){
        
    try(mod.gev<- gev.fit(dat1, show=FALSE) ) #stationary
    if(class(mod.gev)!="try-error") gev.out[site.k, subsite.k,1]<-mod.gev$nllh
    if(class(mod.gev)!="try-error") gev.out[site.k, subsite.k,2:4]<-mod.gev$mle #add another for non-stat
    if(class(mod.gev)!="try-error") gev.out[site.k, subsite.k,5]<-mod.gev$conv #add another for non-stat
    
    #Generalized pareto distribution, for number of times exceeds threshold
    thresh= 35
    
    #stationary
    try(mod.gpd <-gpd.fit(dat1, thresh, npy=365)) #stationary 
    if(class(mod.gpd)!="try-error") gev.out[site.k, subsite.k,6]<-mod.gpd$rate
    
   ## nonstationary 
   # try(mod.gpd<-gpd.fit(dat1, 40, npy=92, ydat=as.matrix(te.dat1$year), sigl=1),silent = FALSE) 
    
    #RETURN LEVELS:  MLE Fitting of GPD - package extRemes
    mpers= c(10,20,50,100)
    for(m in 1:length(mpers)){
      try( pot.day<- fpot(dat1, threshold=35, npp=365.25, mper=mpers[m], std.err = FALSE) )
    
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

#to long format crudely
pow1= pow.out[1,,]
pow1m= melt(pow1)
pow1m$site= "CC"

pow2= pow.out[2,,]
pow2m= melt(pow2)
pow2m$site= "CP"

pow3= pow.out[3,,]
pow3m= melt(pow3)
pow3m$site= "LB"

pow4= pow.out[4,,]
pow4m= melt(pow4)
pow4m$site= "SD"

pow= rbind(pow1m, pow2m, pow3m, pow4m)

#--------------------
# ADD SITE INFO
names(pow)[1:2]=c("subsite","var")

pow$ssite= paste(pow$site,pow$subsite, sep=".")

pow.site= subset(pow, pow$var=="lat")
pow.site= pow.site[!duplicated(pow.site$ssite),]

match1= match(pow$ssite, pow.site$ssite)
pow$lat= pow.site$value[match1]

#====================
## PLOT

#setwd("C:\\Users\\Buckley\\Google Drive\\Buckley\\Work\\ExtremesPhilTrans\\figures\\")

#file<-paste("AustGEV.pdf" ,sep="", collapse=NULL)
#pdf(file,height = 8, width = 11)

dimnames(pow.out)[[3]]<- c("gev.nllh", "gev.loc", "gev.scale", "gev.shape", "conv", "rate", "return10", "return20", "return50", "return100","pat", "lat","height")

pow1= pow[pow$var %in% c("loc", "scale", "shape", "pat", "return100"),]

#ggplot(data=pow1, aes(x=site, y = value, color=subsite))+geom_point()+theme_bw()+facet_wrap(~var, scales="free_y")
ggplot(data=pow1, aes(x=lat, y = value, color=subsite))+geom_point()+theme_bw()+facet_wrap(~var, scales="free_y")

#dev.off()





