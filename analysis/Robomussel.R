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


#shifts in temp and phenology
fld <- with(phen.slopes, interp(x = phen.m*10, y = dtemp.m*10, z = ngen.m*10, duplicate=TRUE))

gdat <- interp2xyz(fld, data.frame=TRUE)

p3d= ggplot(gdat) + 
  aes(x = x, y = y, z = z, fill = z) + 
  geom_tile() + 
  coord_equal() +
  geom_contour(color = "white", alpha = 0.5) + 
  scale_fill_distiller(palette="Spectral", na.value="white", name="number generations\n (1/decade)") + 
  theme_bw(base_size = 18)+xlab("phenology (days/decade)")+ylab("temperature (°C/decade)")+ theme(legend.position="right")+ theme(legend.position="right")+ coord_fixed(ratio = 4)




#==================================================
# EXTREMES

#PTRS
#Fig 5

#STORE TEMPERATURE DATA
temp.dist.cut= dat[dat$Year %in% 1991:2013,"Tmax"]
temp[stat.k,1,1:8]= c(mean(temp.dist.cut, na.rm=TRUE), sd(temp.dist.cut, na.rm=TRUE), median(temp.dist.cut, na.rm=TRUE), quantile(temp.dist.cut, probs=c(0.05, 0.95), na.rm=TRUE), range(dat[,"Year"]), length(unique(dat$Year)) )

#Generalized extreme value distribution
dat1= na.omit(temp.dist.cut)
try(mod.gev<- gev.fit(dat1, show=FALSE) ) #stationary
if(class(mod.gev)!="try-error") temp[stat.k,1, 9]<-mod.gev$nllh
if(class(mod.gev)!="try-error") temp[stat.k,1, 10:12]<-mod.gev$mle #add another for non-stat
if(class(mod.gev)!="try-error") temp[stat.k,1, 14]<-mod.gev$conv #add another for non-stat

#Generalized pareto distribution, for number of times exceeds threshold
thresh= 35
mod.gpd <-gpd.fit(dat1, thresh, npy=365) #stationary
temp[stat.k,1, 15]<-mod.gpd$rate

#---------------------------------------
temp.dist.cut= dat[dat$Year %in% 1962:1990,"Tmax"]
temp.baseline[stat.k,1,1:8]= c(mean(temp.dist.cut, na.rm=TRUE), sd(temp.dist.cut, na.rm=TRUE), median(temp.dist.cut, na.rm=TRUE), quantile(temp.dist.cut, probs=c(0.05, 0.95), na.rm=TRUE), range(dat[,"Year"]), length(unique(dat$Year)) )

#Generalized extreme value distribution
dat1= na.omit(temp.dist.cut)
try(mod.gev<- gev.fit(dat1, show=FALSE) ) #stationary
if(class(mod.gev)!="try-error") temp.baseline[stat.k,1, 9]<-mod.gev$nllh
if(class(mod.gev)!="try-error") temp.baseline[stat.k,1, 10:12]<-mod.gev$mle #add another for non-stat
if(class(mod.gev)!="try-error") temp.baseline[stat.k,1, 14]<-mod.gev$conv #add another for non-stat

#Generalized pareto distribution, for number of times exceeds threshold
thresh= 40
try(mod.gpd <-gpd.fit(dat1, thresh, npy=365) ) #stationary
if(class(mod.gpd)!="try-error") temp.baseline[stat.k,1, 15]<-mod.gpd$rate

} #end loop station

#=========================
## PLOT TOGETHER
setwd("C:\\Users\\Buckley\\Google Drive\\Buckley\\Work\\ExtremesPhilTrans\\figures\\")

file<-paste("AustGEV.pdf" ,sep="", collapse=NULL)
pdf(file,height = 8, width = 11)

sites=cbind(acorn,temp[,1,])
colnames(sites)[18:24]= c("gev.nllh", "gev.loc", "gev.scale", "gev.shape", "gev.mle4", "conv", "rate")

## PLOT SITE CLIMATE INFO
par(mfrow=c(2,2), cex=1.4, mar=c(2, 3, 1, 1), mgp=c(2, 1, 0), oma=c(2,0,0,0), lwd=2)

sites$st.lat= sites$Latitude
sites.coast= sites[sites$cline=="Coast",]
sites.cont= sites[sites$cline=="Cont",]

sites.temps= sites[order(sites$Latitude),]
temps.coast= sites.temps[sites.temps$cline=="Coast",]
temps.cont= sites.temps[sites.temps$cline=="Cont",]

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

#Fig 3

#Return times

dat0=dat[!is.na(dat$var),]
dat1=dat0$var

#Generalized extreme value distribution
try(mod.gev<- gev.fit(dat1, show=FALSE) ) #stationary
if(class(mod.gev)!="try-error") ns.ext.stat[stat.k, 1]<-mod.gev$nllh
if(class(mod.gev)!="try-error") ns.ext.stat[stat.k, 2:4]<-mod.gev$mle #add another for non-stat
if(class(mod.gev)!="try-error") ns.ext.stat[stat.k, 6]<-mod.gev$conv #add another for non-stat

#Generalized pareto distribution, for number of times exceeds threshold
#mod.gpd <-gpd.fit(Ta$value, 40, npy=92) #stationary
## nonstationary 
try(mod.gpd<-gpd.fit(dat1, 40, npy=92, ydat=as.matrix(dat0$year), sigl=1),silent = FALSE) 

#RETURN LEVELS:  MLE Fitting of GPD - package extRemes
thresh= quantile(dat.month$var, 0.9, na.rm=TRUE)
mpers= c(2,5,10,20,50,100)
for(m in 1:length(mpers)){
  pot.day= fpot(dat0$var, threshold=30, npp=365.25, mper=mpers[m] )
  pot.week= fpot(dat.week, threshold=30, npp=365.25, mper=mpers[m] )
  if(m>4)pot.month= fpot(dat.month$var, threshold=30, npp=3, mper=mpers[m] )
  
  rl[stat.k, m, 1]=pot.day$estimate[1]
  rl[stat.k, m, 2]=pot.week$estimate[1]
  if(m>4)rl[stat.k, m, 3]=pot.month$estimate[1]
}
rl[stat.k, 7, 1]=pot.day$pat
rl[stat.k, 7, 2]=pot.week$pat
rl[stat.k, 7, 3]=pot.month$pat

#PLOT RETURN LEVELS
rp= c(2,5,10,20,50,100)

for(k in 1:2){
  labs= "Temperature (?C)"
  
  plot(rp, rl[k, 1:6, 1], type="l", ylim=range(na.omit(rl[k,1:6,])), col=cols[1], ylab=labs, xlab="Return Period (years)", main="", xlim=c(0,80))
  points(rp, rl[k, 1:6, 2], type="l", col=cols[2] )
  points(rp[4:6], rl[k, 4:6, 3], type="l", col=cols[3] )
}




