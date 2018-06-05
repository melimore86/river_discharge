
#Run it line by line. Note in the read and prepare data section it 
#will take a few seconds for each line to load the data from the server. 
#Don't run the code for temperature as your gauge doesn't have temperature. 
#It might have rainfall however.




################################################################################
#
# this code reads daily water guage data from a USGS site, computes the weekly
# composite mean discharge, the weekly climatology over the entire time frame,
# and anomolies based on the the time span of climatoloty.  Requires the
# waterData package.  Visit the following pages for site numbers and metadata
#
# sites:       http://maps.waterdata.usgs.gov/mapper/
# stat codes:  http://nwis.waterdata.usgs.gov/nwis/help/?read_file=stat&format=table
# code codes:  http://nwis.waterdata.usgs.gov/usa/nwis/pmcodes     (choose physical)
#
# Wilcox Suwannee  gauge 02323500
#
################################################################################
#SETUP
rm(list=ls());gc();
graphics.off();windows(record=T)
library("waterData")
library("hydroTSM")
setwd("~/Proposals/Oyster/Oyster AMP")

#station to analyze
station = '02323500'   

################################################################################ 
#
#                     READ AND PREPARE DATA
#
################################################################################ 
#get site name to use in plot titles and such
stinfo  = siteInfo(station)

#read entire time series
dis   = importDVs(staid=station,code='00060',stat='00003', sdate= "1950-01-01") 
#ok looks like temp code is wrong for this station, maybe rainfall is available?

#get some date components
dis$year    = as.numeric(strftime(dis$dates,format="%Y"))
dis$month   = as.numeric(strftime(dis$dates,format="%m")) 

#make dataset from epochs, 
disE  = dis[dis$dates>='1950-01-01' & dis$dates<='2017-10-01',]  

#get monthly sum, mean, sd, and var
#discharge
disE.mo  = aggregate(val~month+year,data=disE,FUN = function(x) c(mean(x,na.rm=T),sd(x,na.rm=T),var(x,na.rm=T),sum(x)))
disE.mo  = do.call('data.frame',disE.mo)
names(disE.mo)[3:6] = c('avg','sd','var','sumflow') 
disE.mo$yrmo = disE.mo$year+(disE.mo$month-0.5)/12       


#get yearly mean, sd, and var
#discharge
disE.yr  = aggregate(val~year,data=disE,FUN = function(x) c(mean(x,na.rm=T),sd(x,na.rm=T),var(x,na.rm=T)))
disE.yr  = do.call('data.frame',disE.yr)
names(disE.yr)[2:4] = c('avg','sd','var')                      


#make some time series objects
disE.zoo    = zoo(disE$val,disE$dates)  

disE.mo.ts  = ts(disE.mo$avg,start=c(1950,1),end=c(2017,10),frequency=12)
disE.mo.sum.ts  = ts(disE.mo$sumflow,start=c(1950,1),end=c(2017,10),frequency=12)
disE.yr.ts  = ts(disE.yr$avg,start=1950,end=2017,frequency=1)

# #fill the gaps in monthly temperature time series - need this for some of the time series analysis
# tempE.mo.zoo = na.approx(tempE.mo.ts)                     #this doeas linear interpolates over gaps
# tempE.seasonal = decompose(tempE.mo.zoo)$seasonal         #this gets the seasonal component
# tempE.mo.filled = ifelse(is.na(tempE.mo.ts),tempE.mo.zoo+tempE.seasonal,tempE.mo.ts)  #now add the seasonal component to interpolated
# tempE.mo.filled  = ts(tempE.mo.filled,start=c(1950,1),end=c(2016,12),frequency=12)
# plot(tempE.mo.filled,col='green',main='Filled Temperature Trend',ylab='Temperature (deg C)')
# lines(tempE.mo.zoo,col='red')  
# lines(tempE.mo.ts,col='black')
# legend('topleft',legend=c('available data','linear interpolation','interpolation + seasonal component'),bty='n',
#        text.col=c('black','red','green'))
# plot(tempE.mo.filled)
# 





################################################################################ 
#
#                     PLOT DATA
#
################################################################################ 
#---------------------------- basic time series plots --------------------------------

#plot all available daily data, discharge and temp
par(mfrow=c(2,1),oma=c(0,0,1,0),mar=c(4,4,4,1))
plot(dis$dates,dis$val,type='l',main='Discharge',xlab='Date',ylab='cf/s')
#abline(v=as.Date('1963-01-01'),lty=2,col='red')

title(stinfo[1,2],outer=T)

#plot daily data from epochs, discharge and temp
par(mfrow=c(2,1),oma=c(0,0,1,0),mar=c(4,4,4,1))
plot(disE$dates,disE$val,type='l',main='Discharge',xlab='Date',ylab='cf/s')
title(stinfo[1,2],outer=T)

#--------------------summary plots from hydroTSM package------------------------
#discharge data, 1950-2016
windows()
hydroplot(disE.zoo,FUN=mean,ylab='Q',var.unit='cf/s')
title(paste(stinfo[1,2],': 2000-2017',sep=''),outer=T)

#--------------------------- duration curves -----------------------------------
#simply converting each vector to 'ts' allows to cbind without recycling values in shorter vector

dis.fdc = as.data.frame(cbind(ts(disE$val[disE$year<1981]),ts(disE$val[disE$year>=1981])))
par(mfrow=c(1,1))
fdc(dis.fdc,ylab='Flow (cf/s)',leg.txt=c('1950-1980','1981-2017'),cex.axis=1)
title(stinfo[1,2],outer=T)


#---------------------------- quartile plots -----------------------------------

################################################################################ 
#
#                     LOESS - local regression, smoothing
#
################################################################################ 
#this is used to graphically evaluate whether the time series data show monotonic
#or piecewise trends

#Discharge data - cf/s
par(mfrow=c(2,2),mar=c(4,4,2,2),oma=c(0,0,3,0))
plot(disE$dates,disE$val,type='l',xlab='Date',ylab='Discharge (cf/s)',main='daily data')
lines(lowess(disE$dates,disE$val),col='red')  
plot(disE.mo.ts,xlab='Date',ylab='Discharge (cf/s)',main='monthly means')
lines(lowess(disE.mo.ts),col='red')   
plot(disE.mo.sum.ts,xlab='Date',ylab='Discharge (cf/s)',main='monthly sum')
lines(lowess(disE.mo.sum.ts),col='red')   
plot(disE.yr$year,disE.yr$avg,type='p',xlab='Date',ylab='Discharge (cf/s)',main='annual means')
lines(lowess(disE.yr$year,disE.yr$avg),col='red')
abline(lm(disE.yr$avg~seq(1955,2013,1)))
title(stinfo[1,2],outer=T)



########

CV  = disE.yr$sd/disE.yr$avg



#par(mfrow=c(2,2))

tiff(filename="Figure1.tiff", height = 22.7, width = 17.3, units = 'cm', compression = "lzw", res = 500)


windows(record=T)

layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))

plot(disE.yr$year,disE.yr$avg,type='p',xlab='Date',ylab='Discharge (cf/s)',pch=19,cex=1.5,main='Annual means')
lines(lowess(disE.yr$year,disE.yr$avg),col='red', lwd=4)
#abline(lm(disE.yr$avg~seq(1950,2016,1)))
title(stinfo[1,2],outer=T, line = -1.0)

plot(disE.yr$year,disE.yr$var,type='p',xlab='Date',ylab='Discharge (cf/s)',pch=19,cex=1.5,main='Annual variance')
lines(lowess(disE.yr$year,disE.yr$var),col='red', lwd=4)
#abline(lm(disE.yr$avg~seq(1950,2016,1)))
title(outer=T)

plot(disE.yr$year,CV,type='p',xlab='Date',ylab='Discharge (cf/s)',pch=19,cex=1.5,main='Annual CV')
lines(lowess(disE.yr$year,CV),col='red', lwd=4)
title(outer=T)

dev.off()



########



################################################################################ 
#
#                     PIECEWISE REGRESSION
#
################################################################################ 
#------------------- manually looping through breakpoints ----------------------
#this assumes that the segments are discontinuous
par(mfrow=c(2,2))
#Annual Mean Discharge..........................................................
Break = disE.yr$year
rse   = numeric(length(Break))
for(i in 1:length(Break)){
  mod = lm(avg ~ year*(year<Break[i]) + year*(year>=Break[i]),data=disE.yr)
  rse[i] = summary(mod)[[6]]
}
#model with optimum breakpoint
optbk   = Break[which.min(rse)]
mod.opt = lm(avg ~ year*(year<optbk) + year*(year>=optbk),data=disE.yr)
coefs   = coef(mod.opt)
mod.lm  = lm(avg~year,data=disE.yr)
anova(mod.lm,mod.opt)
#plot
plot(avg~year,data=disE.yr,type='b',main='annual',xlab='Year',ylab='Mean Discharge (cf/s)')
curve(coefs[1]+coefs[2]*x,add=T,from=optbk,to=tail(Break,1),col='red')
curve((coefs[1]+coefs[3])+(coefs[2]+coefs[5])*x,add=T,from=head(Break,1),to=optbk,col='red')
abline(v=optbk,col='gray')
minor.tick(5)
plot(Break,rse,type='l',main='annual',xlab='Breakpoint')
minor.tick(5)    

#Monthly Mean Discharge......................................................... 
Break = disE.mo$yrmo
rse   = numeric(length(Break))
for(i in 1:length(Break)){
  mod = lm(avg ~ yrmo*(yrmo<Break[i]) + yrmo*(yrmo>=Break[i]),data=disE.mo)
  rse[i] = summary(mod)[[6]]
}
#model with optimum breakpoint
optbk   = Break[which.min(rse)]
mod.opt = lm(avg ~ yrmo*(yrmo<optbk) + yrmo*(yrmo>=optbk),data=disE.mo)
coefs   = coef(mod.opt)
mod.lm  = lm(avg~yrmo,data=disE.mo)
anova(mod.lm,mod.opt)
#plot
plot(avg~yrmo,data=disE.mo,type='b',main='monthly',xlab='Month',ylab='Mean Discharge (cf/s)')
curve(coefs[1]+coefs[2]*x,add=T,from=optbk,to=tail(Break,1),col='red')
curve((coefs[1]+coefs[3])+(coefs[2]+coefs[5])*x,add=T,from=head(Break,1),to=optbk,col='red')
abline(v=optbk,col='gray')  
minor.tick(5)
plot(Break,rse,type='l',main='monthly',xlab='Breakpoint')
minor.tick(5)  
title(stinfo[1,2],outer=T)


#---------------------- using segmented package --------------------------------
#this method assumes that segments are continuous
library(segmented)
par(mfrow=c(2,1))

#Annual Mean Discharge..........................................................
mod.lm   = lm(avg~year,data=disE.yr)
mod.seg  = segmented(mod.lm,seg.Z=~year,psi=2000)
summary(mod.seg)
plot(avg~year,data=disE.yr,type='b',main='annual',xlab='Year',ylab='Mean Discharge (cf/s)')
plot(mod.seg,add=T,col='red')
lines(mod.seg,col='blue') 
minor.tick(5)
anova(mod.lm,mod.seg)   

#Monthly Mean Discharge.........................................................
mod.lm   = lm(avg~yrmo,data=disE.mo)
mod.seg  = segmented(mod.lm,seg.Z=~yrmo,psi=2000)
summary(mod.seg)
plot(avg~yrmo,data=disE.mo,type='b',main='monthly',xlab='Month',ylab='Mean Discharge (cf/s)')
plot(mod.seg,add=T,col='red')
lines(mod.seg,col='blue')
minor.tick(5)
anova(mod.lm,mod.seg)

title(stinfo[1,2],outer=T)



################################################################################ 
#
#                     TIME SERIES DECOMPOSITION & ANALYSIS
#
################################################################################ 
#--------------------------- DISCHARGE -----------------------------------------
par(mfrow=c(1,1))
plot(disE.mo.ts)
frequency(disE.mo.ts)

disE.mo.stl = stl(disE.mo.ts,"periodic")
plot(disE.mo.stl,main='Monthly Mean Discharge Data')
ts.plot(disE.mo.stl$time.series,col=c('black','blue','red'))
legend('bottomright',legend=attributes(disE.mo.stl$time.series)$dimnames[[2]],
       bty='n',text.col=c('black','blue','red'))

plot(diff(disE.mo.ts))

