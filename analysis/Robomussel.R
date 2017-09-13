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

#failed attempt to use plyr or apply
#make wrapper function
#dat= te.max[,c("MaxTemp_C","j", "site","subsite")]
#lomb= function(dat){ spec_lomb_phase(dat[,1], dat[,2], freq=fseq) }

#to long format


#freq, amp plot
plot(te.max.lomb$freq, te.max.lomb$cyc_range/2, type="l", log="xy")