################################################################################
#        Quartile Plot Comparison         
############################################################################### 
setwd('C:\\Users\\Mike Dodrill\\Desktop\\Flow Summaries\\DATA')
rm(list=ls(all=TRUE))
library(reshape)
library(ggplot2)  

d1=read.csv("Apalach_flow_1950_2012.csv")                             
str(d1)                                                               
head(d1)                                                              
                                                                                                              
class(d1$Date)                                                        
d1$Date = as.POSIXct(strptime(as.character(d1$Date),"%m/%d/%Y"))   # Change the class of the date column  

d2 = subset(d1, substr(d1$Date,6,10)!='02-29')     # Take out Feb. 29th
# -------------------- Calculate the daily Quantile -------------------------- #   
# you can define the specific percentage you want by changing the probs argument
# examp = quantile(d1$Discharge, probs = seq(0, 1, 0.25))   

out = matrix(NA,365,5)   # Object to fill with the loop - quantiles for every day of the year
day = unique(paste(substr(d2$Date[],6,7),substr(d2$Date[],9,10),sep='-'))  # Unique days of the year
o=order(day,decreasing = FALSE)				# index for reordering day 
days=day[o]                           # order days  

length(days)   # Check 

for(i in 1:length(days)){
    ind = which(substr(d2$Date[],6,10)==days[i])
    out[i,] = quantile(d2[ind,][,4])
    }

head(out)    
# ----------------------- Reformat the quartile data ------------------------- #   
# Reformat the data into a form that ggplot likes
out2 = melt(out)
head(out2)
colnames(out2) = list("id","variable","value")
out2$variable = factor(out2$variable, levels=c("1","2","3","4","5"))
str(out2)

# ------------------ Format the individual years of data --------------------- #
# Subset years for plotting 
d_07 =subset(d2, substr(d2$Date,1,4)=='2007')
d_08 =subset(d2, substr(d2$Date,1,4)=='2008')
d_09 =subset(d2, substr(d2$Date,1,4)=='2009')
d_10 =subset(d2, substr(d2$Date,1,4)=='2010')
d_11 =subset(d2, substr(d2$Date,1,4)=='2011')

length(d_07$Date)    # Check 
length(d_08$Date)     
length(d_09$Date)
length(d_10$Date)
length(d_11$Date)

d3 = as.data.frame(cbind(d_07$Discharge,d_08$Discharge,d_09$Discharge,d_10$Discharge,d_11$Discharge)) 
class(d3)
head(d3)
colnames(d3) = list("D2007","D2008","D2009","D2010","D2011")
# Combine discharge with quantile data
d4 = cbind(d3,out2)
str(d4)
  
# ------------------------ Plots --------------------------------------------- #

p = ggplot(d4) + 
geom_area(aes(id, value, fill=variable)) + 
geom_line(aes(id,D2008)) +                       # Change what year to plot in black 
ylab(expression(paste('Mean Daily Discharge','  ',(ft^3 / sec)))) + 
xlab('Day of the Year') + 
ggtitle("2008 Apalachacola River Comparison") +  # Change title 
scale_fill_discrete(guide = guide_legend(title = "Quantiles", reverse=TRUE),
labels=c('0%','25%','50%','75%','100%'))
p
 
p + theme_bw()   # or with a white background 

# p + scale_fill_discrete(guide = guide_legend(title = "Quantiles")) 
# p + guides(fill=guide_legend(reverse=TRUE))
# guide_legend(fill=FALSE)  
### END ###### END ###### END ###### END ###### END ###### END ###### END ###    