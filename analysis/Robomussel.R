#load libraries
library(plyr)
library(dplyr)
library(reshape2)
library(tidyr)

library(ggplot2)

#ROBOMUSSEL ANALYSIS
my.read.table= function(x) {
  dat= read.table(x, row.names=NULL)
  dat$id= gsub(".txt","",x) 
  return(dat)}

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

#extract sites and numbers
te.wa$id1= gsub("BMRMUSWA","",te.wa$id)
te.wa$site= as.factor( substr(te.wa$id1, 1, 2) )

#extract subsite
te.wa$subsite=  substr(te.wa$id1, 3, 4)
te.wa$subsite= gsub("_","",te.wa$subsite)
te.wa$subsite= as.factor(te.wa$subsite)

te.wa$date= te.wa$row.names

#find daily max
te.max= te.wa %>% group_by(date, site, subsite) %>% summarise(id1=id1[1], MaxTemp_C= max(Temp_C))

day=  as.POSIXlt(te.max$date, format="%m/%d/%Y")
te.max$doy=as.numeric(strftime(day, format = "%j"))
te.max$year=as.numeric(strftime(day, format = "%Y"))
te.max$month=as.numeric(strftime(day, format = "%m"))
te.max$j=julian(day)

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
te.lomb = te.max %>% group_by(site, subsite) %>% summarise( freq=spec_lomb_phase(te.max$MaxTemp_C, te.max$j, freq=fseq)  )

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
dimnames(pow.out)[[2]]<- 1:19

#failed attempt to use plyr or apply
#make wrapper function
#dat= te.max[,c("MaxTemp_C","j", "site","subsite")]
#lomb= function(dat){ spec_lomb_phase(dat[,1], dat[,2], freq=fseq) }

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
colnames(pow)[1:3]=c("subsite","freq","cyc_range")

#correct freq values
pow$freq= fseq[pow$freq]

#sort by frequency
pow= pow[order(pow$site, pow$subsite, pow$freq),]
pow$subsite= factor(pow$subsite)

#freq, amp plot
plot(te.max.lomb$freq, te.max.lomb$cyc_range/2, type="l", log="xy")

ggplot(data=pow, aes(x=log(freq), y = log(cyc_range/2), color=subsite))+geom_line() +theme_bw()+facet_wrap(~site)

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
    if(class(pot.day)!="try-error") gev.out[site.k, subsite.k,13]=pot.day$pat
    
    } #end check time series
  } #end subsites
} #end sites

#-------------------------
#PLOT

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
colnames(pow)[1:3]=c("subsite","freq","cyc_range")



#====================
## PLOT

#setwd("C:\\Users\\Buckley\\Google Drive\\Buckley\\Work\\ExtremesPhilTrans\\figures\\")

#file<-paste("AustGEV.pdf" ,sep="", collapse=NULL)
#pdf(file,height = 8, width = 11)

sites=cbind(acorn,temp[,1,])
colnames(sites)[18:24]= c("gev.nllh", "gev.loc", "gev.scale", "gev.shape", "gev.mle4", "conv", "rate")


#CLIMATE
plot(temps.coast$Latitude, temps.coast$gev.loc, col="black", type="b", xlim= range(sites.temps$Latitude), ylim= range(sites$gev.loc), ylab="GEV location", xlab="Latitude (?S)" )
points(temps.cont$Latitude, temps.cont$gev.loc, lty="dashed", col="grey", type="b")

legend("bottomright", legend=c("Coastal","Continental"), lty="solid", col=c("black","grey"), bty='n', cex=1)

plot(temps.coast$Latitude, temps.coast$gev.scale, col="black", type="b", xlim= range(sites.temps$Latitude), ylim= range(sites$gev.scale), ylab="GEV scale")
points(temps.cont$Latitude, temps.cont$gev.scale, lty="dashed", col="grey", type="b")

plot(temps.coast$Latitude, temps.coast$gev.shape, col="black", type="b", xlim= range(sites.temps$Latitude), ylim= range(sites$gev.shape), ylab="GEV shape")
points(temps.cont$Latitude, temps.cont$gev.shape, lty="dashed", col="grey", type="b")

plot(temps.coast$Latitude, temps.coast$rate, col="black", type="b", xlim= range(sites.temps$Latitude), ylim= range(sites$rate), ylab="Annual rate of exceeding 40?C")
points(temps.cont$Latitude, temps.cont$rate, lty="dashed", col="grey", type="b")

mtext("Latitude (?)", side=1, line = 0, cex=1.3, outer=TRUE)

dev.off()





