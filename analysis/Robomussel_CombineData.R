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

#SAVE
setwd("C:\\Users\\lbuckley\\Desktop\\Fall2017\\ICBClimBio\\")
saveRDS(te.max, "tedat.rds")
#te.max <- readRDS("tedat.rds")

write.csv(te.max, "tedat.csv")


