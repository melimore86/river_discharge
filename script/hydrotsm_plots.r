################################################################################
#       HydroTSM Package Graphs           
############################################################################### 
#setwd('C:\\Users\\Mike Dodrill\\Desktop\\Flow Summaries\\DATA')
setwd('I:/Oyster/Apalachicola oyster/Apalach flow data/Flow_sum')
rm(list=ls(all=TRUE))
library(hydroTSM)

d1=read.csv("Apalach_flow_1950_2012.csv")                             
str(d1)                                                               
head(d1)                                                              
                                                                                                               
class(d1$Date)                                                        
d1$Date = as.POSIXct(strptime(as.character(d1$Date),"%m/%d/%Y"))     
                          
# ---------------------------------------------------------------------------- #
d2 = zoo(d1$Discharge,d1$Date)
d2 = vector2zoo(d1$Discharge,d1$Date,date.fmt="%m/%d/%Y")

# ---------------------------- Plots ----------------------------------------- #
# Large summary plot
hydroplot(d2, FUN=mean, ylab= "Q", var.unit = "m3/s")

# Subset of the 9 plots, See hydroplot in package manual 
# can change the pfreq and ptype arguments...
hydroplot(d2, FUN=mean, ylab = 'Q',pfreq="ma",ptype='ts+boxplot',var.unit = "m3/s")


### END ###### END ###### END ###### END ###### END ###### END ###### END ###### END ###