library(waterData)
library(lubridate)
library(ggplot2)
library(tidyverse)
library(scales)
library(dplyr)
library(reshape2)

#station to analyze
station = '02323500'   

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

dis$Month <- month(dis$Date, label=TRUE)
dis$Month2 <- month(dis$Date, label=FALSE)


dis_mean_year<-  aggregate( Discharge ~ Year, dis, mean )
dis_mean_month<-  aggregate( Discharge ~ Month, dis, mean )
dis_mean_date<-  aggregate( Discharge ~ Date, dis, mean )
dis_mean_date$Month<- month(dis_mean_date$Date, label=TRUE)




summary(dis$Discharge)


Jan<- dis_mean_date[ which(dis_mean_date$Month=='Jan'), ]
Feb<- dis_mean_date[ which(dis_mean_date$Month=='Feb'), ]
Mar<- dis_mean_date[ which(dis_mean_date$Month=='Mar'), ]
Apr<- dis_mean_date[ which(dis_mean_date$Month=='Apr'), ]
May<- dis_mean_date[ which(dis_mean_date$Month=='May'), ]
Jun<- dis_mean_date[ which(dis_mean_date$Month=='Jun'), ]
Jul<- dis_mean_date[ which(dis_mean_date$Month=='Jul'), ]
Aug<- dis_mean_date[ which(dis_mean_date$Month=='Aug'), ]
Sep<- dis_mean_date[ which(dis_mean_date$Month=='Sep'), ]
Oct<- dis_mean_date[ which(dis_mean_date$Month=='Oct'), ]
Nov <- dis_mean_date[ which(dis_mean_date$Month=='Nov'), ]
Dec <- dis_mean_date[ which(dis_mean_date$Month=='Dec'), ]



#Jan$Mean<- mean (Jan$Discharge)
#Feb$Mean<- mean (Feb$Discharge)
#Mar$Mean<- mean (Mar$Discharge)
#Apr$Mean<- mean (Apr$Discharge)
#May$Mean<- mean (May$Discharge)
#Jun$Mean<- mean (Jun$Discharge)
#Jul$Mean<- mean (Jul$Discharge)
#Aug$Mean<- mean (Aug$Discharge)
#Sep$Mean<- mean (Sep$Discharge)
#Oct$Mean<- mean (Oct$Discharge)
#Nov$Mean<- mean (Nov$Discharge)
#Dec$Mean<- mean (Dec$Discharge)

Jan$Season<- (Jan$Season = "Winter")
Feb$Season<- (Feb$Season = "Winter")
Mar$Season<- (Mar$Season = "Spring")
Apr$Season<- (Apr$Season = "Spring")
May$Season<- (May$Season = "Spring")
Jun$Season<- (Jun$Season = "Summer")
Jul$Season<- (Jul$Season = "Summer")
Aug$Season<- (Aug$Season = "Summer")
Sep$Season<- (Sep$Season = "Fall")
Oct$Season<- (Oct$Season = "Fall")
Nov$Season<- (Nov$Season = "Fall")
Dec$Season<- (Dec$Season = "Winter")


dis_season<- rbind(Jan, Feb,Mar, May, Jun, Jul, Aug, Sep, Oct, Nov,Dec)

spring<- rbind(Mar,Apr,May)
summer<- rbind(Jun,Jul, Aug)
fall<- rbind( Sep, Oct, Nov)
winter<- rbind(Dec,Jan,Feb)


# 1998
rd_1998<-ggplot(data=dis, aes(x=Date, y=Discharge/1000)) +
  geom_line() +
  labs(x="Date", y="River Discharge (1,000 cfs)", title ="1998 Suwanee River Discharge") +
  scale_x_date(
    limits = c(
      as.Date("1998-01-01"),
      as.Date("1998-12-031")))+ 
  theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid"),
        axis.text.x = element_text(angle = 90, hjust = 1),legend.position = "right") 
ggsave("rd_1998.jpg")




# 2012
rd_2012<-ggplot(data=dis, aes(x=Date, y=Discharge/1000)) +
  geom_line() +
  labs(x="Date", y="River Discharge (1,000 cfs)", title ="2012 Suwanee River Discharge") +
  scale_x_date(
    limits = c(
      as.Date("2012-01-01"),
      as.Date("2012-12-031")))+ 
  theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid"),
        axis.text.x = element_text(angle = 90, hjust = 1),legend.position = "right") 
ggsave("rd_2012.jpg")



ggplot ()+
  geom_line(data=spring, aes(x=Date, y= Discharge/1000, color="Spring"),size=1.5)+
  geom_line(data=summer, aes(x=Date, y= Discharge/1000, color="Summer"), size=1.5)+
  geom_line(data=fall, aes(x=Date, y= Discharge/1000, color="Fall"),  size=1.5)+
  geom_line(data=winter, aes(x=Date, y= Discharge/1000, color="Winter"), size=1.5) +
  labs(x="Date", y="River Discharge (1,000 cfs)", title ="1949-1970 Mean Seasonal Water Discharge", color= "Season") +
  scale_color_manual(values = c("#D55E00", "#009E73", "#E69F00",  "#0072B2"))+
  scale_x_date(
    limits = c(
      as.Date("1949-01-01"),
      as.Date("1970-01-01")))+ 
  theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid"),
        axis.text.x = element_text(angle = 90, hjust = 1),legend.position = "right") 



ggplot ()+
  geom_line(data=spring, aes(x=Date, y= Discharge/1000, color="Spring"),size=1.5)+
  geom_line(data=summer, aes(x=Date, y= Discharge/1000, color="Summer"), size=1.5)+
  geom_line(data=fall, aes(x=Date, y= Discharge/1000, color="Fall"),  size=1.5)+
  geom_line(data=winter, aes(x=Date, y= Discharge/1000, color="Winter"), size=1.5) +
  labs(x="Date", y="River Discharge (1,000 cfs)", title ="1970-1990 Mean Seasonal Water Discharge", color= "Season") +
  scale_color_manual(values = c("#D55E00", "#009E73", "#E69F00",  "#0072B2"))+
  scale_x_date(
    limits = c(
      as.Date("1970-01-01"),
      as.Date("1990-01-01")))+ 
  theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid"),
        axis.text.x = element_text(angle = 90, hjust = 1),legend.position = "right") 


ggplot ()+
  geom_line(data=spring, aes(x=Date, y= Discharge/1000, color="Spring"),size=1.5)+
  geom_line(data=summer, aes(x=Date, y= Discharge/1000, color="Summer"), size=1.5)+
  geom_line(data=fall, aes(x=Date, y= Discharge/1000, color="Fall"),  size=1.5)+
  geom_line(data=winter, aes(x=Date, y= Discharge/1000, color="Winter"), size=1.5) +
  labs(x="Date", y="River Discharge (1,000 cfs)", title ="1990-2010 Mean Seasonal Water Discharge", color= "Season") +
  scale_color_manual(values = c("#D55E00", "#009E73", "#E69F00",  "#0072B2"))+
  scale_x_date(
    limits = c(
      as.Date("1990-01-01"),
      as.Date("2010-01-01")))+ 
  theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid"),
        axis.text.x = element_text(angle = 90, hjust = 1),legend.position = "right") 

ggplot ()+
  geom_line(data=spring, aes(x=Date, y= Discharge/1000, color="Spring"),size=1.5)+
  geom_line(data=summer, aes(x=Date, y= Discharge/1000, color="Summer"), size=1.5)+
  geom_line(data=fall, aes(x=Date, y= Discharge/1000, color="Fall"),  size=1.5)+
  geom_line(data=winter, aes(x=Date, y= Discharge/1000, color="Winter"), size=1.5) +
  labs(x="Date", y="River Discharge (1,000 cfs)", title ="2010-2018 Mean Seasonal Water Discharge", color= "Season") +
  scale_color_manual(values = c("#D55E00", "#009E73", "#E69F00",  "#0072B2"))+
  scale_x_date(
    limits = c(
      as.Date("2010-01-01"),
      as.Date("2018-01-01")))+ 
  theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid"),
        axis.text.x = element_text(angle = 90, hjust = 1),legend.position = "right") 


# Plotting all years by season

ggplot(dis_season, aes(x= Date, y=Discharge/1000, color= Season))+
  geom_point (size= 1)+
  labs(x="Date", y="River Discharge (1,000 cfs)", color= "Season", title ="1946-1970 Mean Seasonal Water Discharge") +
  scale_color_manual(values = c("#D55E00", "#009E73", "#E69F00",  "#0072B2"))+
  scale_x_date(
    limits = c(
      as.Date("1949-01-01"),
      as.Date("1970-01-01")))+ 
  theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid"),
        axis.text.x = element_text(angle = 90, hjust = 1)) 


ggplot(dis_season, aes(x= Date, y= Discharge/1000, color= Season))+
  geom_point (size= 1)+
  labs(x="Date", y="River Discharge (1,000 cfs)", color= "Season", title ="1970-1990 Mean Seasonal Water Discharge") +
  scale_color_manual(values = c("#D55E00", "#009E73", "#E69F00",  "#0072B2"))+
  scale_x_date(
    limits = c(
      as.Date("1970-01-01"),
      as.Date("1990-01-01")))+ 
  theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid"),
        axis.text.x = element_text(angle = 90, hjust = 1)) 


ggplot(dis_season, aes(x= Date, y= Discharge/1000, color= Season))+
  geom_point (size= 1)+
  labs(x="Date", y="River Discharge (1,000 cfs)", color= "Season", title ="1990-2010 Mean Seasonal Water Discharge") +
  scale_color_manual(values = c("#D55E00", "#009E73", "#E69F00",  "#0072B2"))+
  scale_x_date(
    limits = c(
      as.Date("1990-01-01"),
      as.Date("2010-01-01")))+ 
  theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid"),
        axis.text.x = element_text(angle = 90, hjust = 1)) 


ggplot(dis_season, aes(x= Date, y= Discharge/1000, color= Season))+
  geom_point (size= 1)+
  labs(x="Date", y="River Discharge (1,000 cfs)", color= "Season", title ="2010-2018 Mean Seasonal Water Discharge") +
  scale_color_manual(values = c("#D55E00", "#009E73", "#E69F00",  "#0072B2"))+
  scale_x_date(
    limits = c(
      as.Date("2010-01-01"),
      as.Date("2018-12-01")))+ 
  theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid"),
        axis.text.x = element_text(angle = 90, hjust = 1)) 



#Boxplot of every year per month
#river_box<-
  ggplot(data=dis, aes(x= Month, y=Discharge/1000)) + 
  geom_boxplot(position = 'identity') +
  theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid"),axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x="Month", y="River Discharge (1,000 cfs)") +
  facet_wrap(~Year, ncol=8) 
ggsave('river_box.png', height = 12, width = 15, dpi=300)

  
#Linear river discharge  
 # river_linear<- 
    ggplot(data=dis, aes(x= Date, y=Discharge/1000)) + 
    geom_line() + 
      scale_x_date(
        breaks = date_breaks("1 month") ,
        labels = date_format("%b")) +
      facet_wrap(~year(Date), ncol=8, scale = "free_x") +
    theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid"),axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(x="Month", y="River Discharge (1,000 cfs)") 
  ggsave('river_linear.png', height = 12, width = 15, dpi=300)

  
 #Average per year and then line per year 
#river_average<-
  ggplot(data=dis, aes(x= Date, y=Discharge)) + 
  geom_point(color= "black", aes(x=Year)) +
  stat_summary(aes(x=Month),fun.y ="mean", geom="line", color= "red", size= 3, alpha= 0.6) +
  facet_wrap(~year(Date), ncol=8, scale = "free_x") +
  #scale_x_date(
   ## breaks = date_breaks("1 month") ,
    #labels = date_format("%b"),
    #limits = c(
      #as.Date("1950-01-01"),
      #as.Date("1950-12-31"))) +
  theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid"),axis.text.x = element_text(angle = 90, hjust = 1))
 
  ##Percentage of occurence of river discharge
  
  #river_histo<- 
  ggplot(data=dis, aes(dis$Discharge/1000)) + 
    labs(x="Discharge (1,000 cfs)", y="Percentage of frequency", title="Suwannee River Discharge") +  geom_histogram(aes(y = ((..count..)/sum(..count..)*10000))) +
    theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid"),axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_y_continuous(limits = c(0,100)) +
    scale_x_continuous(breaks = c(0,5,10,15,20,25,30,35,40,45,50)) +
    facet_wrap(~Year, ncol=10)
  ggsave('river_histo.png', height = 12, width = 15, dpi=300)
  
#Creating a plot with average of all years and then every year 
 
library(lubridate)

dis_mean_year$Date <- as.Date(as.character(dis_mean_year$Year), format = "%Y")
y <- year(yr)
class(dis_mean_year$Date)



#p<- 
ggplot() +
  geom_line(data=dis, aes(x=Date, y=Discharge/1000)) +
  #stat_summary(data=dis_mean_year,aes(x=Date, y=Discharge/1000,xmin=1950, xmax=2017) , fun.y= "mean", color="red", geom = "line") +
  geom_line(data=dis_mean_year,  aes(x=Date, y=Discharge/1000), color="red") +
labs(x="Month", y="River Discharge (1,000 cfs)") +
  scale_x_date(
    breaks = date_breaks("1 month") ,
    labels = date_format("%b")) +
  theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid"),axis.text.x = element_text(angle = 90, hjust = 1)) + 
  facet_wrap(~Year, ncol=10,scale = "free_x") 




 
