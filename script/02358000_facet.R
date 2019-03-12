library(waterData)
library(lubridate)
library(ggplot2)
library(tidyverse)
library(scales)
library(dplyr)
library(reshape2)

#station to analyze
station = '02358000'   

#get site name to use in plot titles and such
stinfo  = siteInfo(station)

#read entire time series
dis   = importDVs(staid=station,code='00060',stat='00003', sdate= "1950-01-01") 

dis$year    = as.numeric(strftime(dis$dates,format="%Y"))
#dis$month   = as.numeric(strftime(dis$dates,format="%m")) 

#Naming columns, using the Diver sensors, collects date, pressure, temp, conductivity
colnames(dis) <- c("StaID", "Discharge", "oldDate", "QualCode", "Year")


#Changing the format of the dates to be able to plot against time
dis$Date <- as.Date(dis$oldDate)

ggplot() +
  geom_line(data=dis, aes(x=Date, y=Discharge/1000)) +
  #stat_summary(data=dis_mean_year,aes(x=Date, y=Discharge/1000,xmin=1950, xmax=2017) , fun.y= "mean", color="red", geom = "line") +
  #geom_line(data=dis_mean_year,  aes(x=Date, y=Discharge/1000), color="red") +
  labs(x="Month", y="River Discharge (1,000 cfs)") +
  scale_x_date(
    breaks = date_breaks("1 month") ,
    labels = date_format("%b")) +
  theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid"),axis.text.x = element_text(angle = 90, hjust = 1)) + 
  facet_wrap(~Year, ncol=10,scale = "free_x") 
