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
#te.wa$id= gsub("BMRMUSWA","",te.wa$id)
#te.wa$site= substr(te.wa$id, 1, 2)

#----------------------
#PLOTS

BMRMUSWASD



#-------------------------
#FREQUENCY
# https://github.com/georgebiogeekwang/tempcycles/
library("tempcycles")