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
te.wa$site= substr(te.wa$id1, 1, 2)

#extract subsite
te.wa$subsite=  substr(te.wa$id1, 3, 4)
te.wa$subsite= as.numeric(gsub("_","",te.wa$subsite))

te.wa$date= te.wa$row.names

#find daily max
te.max= te.wa %>% group_by(date, site, subsite) %>% summarise(id1=id1[1], MaxTemp_C= max(Temp_C))

day=  as.POSIXlt(te.max$date, format="%m/%d/%Y")
te.max$doy=as.numeric(strftime(day, format = "%j"))
te.max$j=julian(day)

#----------------------
#PLOTS

#FREQUENCY
# https://github.com/georgebiogeekwang/tempcycles/

#power spectrum
#x: frequency (1/days)
#y: log amplitude

#need to incorporate within site variation
te.dat= subset(te.max, te.max$site=="SD")
te.max.lomb <- spec_lomb_phase(te.dat$MaxTemp_C, te.dat$doy, freq=seq(0,1,0.01))
te.max.lomb <- spec_lomb_phase(te.dat$MaxTemp_C, te.dat$j, freq=seq(0,1,0.01))


#freq, amp plot
plot(te.max.lomb$freq, te.max.lomb$cyc_range/2, type="l", log="y")