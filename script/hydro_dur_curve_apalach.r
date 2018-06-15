################################################################################
#       Flow Duration Curve Comparison - Pre-2000 with 2007-2012          
############################################################################### 
setwd('C:\\Users\\Mike Dodrill\\Desktop\\Flow Summaries\\DATA')
rm(list=ls(all=TRUE))
library(hydroTSM)
library(plyr)

d1=read.csv("Apalach_flow_1950_2012.csv")                             
str(d1)                                                               
head(d1)                                                              
                                                        
d1$Date = as.POSIXct(strptime(as.character(d1$Date),"%m/%d/%Y"))     
class(d1$Date)                            
# ---------------------------------------------------------------------------- #
test = which(substr(d1$Date,1,4)=='2007')[1]   # find which row a specific year starts

d2 = vector2zoo(d1[1:17989,4],d1[1:17989,3],date.fmt="%m/%d/%Y")     # Pre - 2000
head(d2)
length(d2)
d3 = vector2zoo(d1[20547:22647,4],d1[20547:22647,3],date.fmt="%m/%d/%Y") # 2007-2012
head(d3)
length(d3) 

# Since pre-2000 and 07-12 are different lengths, you can't just bind them together, 
# because the shorter vector's values will be recycled to match the length of the
# longer vector. So...

Pre <- d1[1:17989,4]
D_07on <- d1[20547:22647,4]
n <- max(length(Pre), length(D_07on))
length(Pre) <- n                      
length(D_07on) <- n
d4 = cbind(Pre,D_07on)
head(d4)

# -------------------------- Plots  ------------------------------------------ #
# Flow Duration Curves 
fdc(d2,thr.shw=FALSE, ylab= expression(paste('Q','  ',(ft^3 / sec))), line=2.5)   # years individually
fdc(d3,thr.shw=FALSE, ylab= expression(paste('Q','  ',(ft^3 / sec))), line=2.5)

# Comparison Plot
fdc(d4, thr.shw=FALSE,leg.txt=c('Pre 2000', '2007-2012'),
main= 'Apalachacola River Flow Duration Curves', ylab='', log='y',yat=c(5000, 10000, 20000))
mtext(expression(paste('Discharge','  ',(ft^3 / sec))),side=2, line=2.5,cex=1.2)

### END ###### END ###### END ###### END ###### END ###### END ###### END ###### END ###### END ###