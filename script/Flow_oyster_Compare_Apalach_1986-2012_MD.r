################################################################################
#       Total Monthly Flow and Total Monthly Oyster Harvest Comparison
#           
############################################################################### 
#setwd('C:\\Users\\Mike Dodrill\\Documents\\Dodrill\\Projects\\Bill_Pine\\Osyster')
setwd('F:/Oyster/Apalachicola oyster/Apalach flow data/Flow_sum')
library(reshape)
rm(list=ls(all=TRUE))

d1=read.csv("Apalach_flow_1950_2012.csv")                             
str(d1)                                                               
head(d1)                                                              
                                                                                                                 
class(d1$Date)                                                        
d1$Date = as.POSIXct(strptime(as.character(d1$Date),"%m/%d/%Y"))     
# ---------------------------------------------------------------------------- #
test = which(substr(d1$Date,1,4)=='1986')[1]     # find which row a specific year starts
flow_86_12 = d1[12877:22647,]       # 1986-2012   
head(flow_86_12)

month = as.character(c('01','02','03','04','05','06','07','08','09','10','11','12'))
year = unique(substr(flow_86_12$Date,1,4))         # Unique years for 1986-2012 data
m_sum_flow_86_12 = matrix(NA,length(year),12)      # object to fill with loop 

# Calculate total monthy discharge 
for(i in 1:length(year)){                        
    yr = subset(d1, substr(d1$Date,1,4)==year[i])       # loop across years
        for(j in 1:length(month)){                      # loop across months
        ind = which(substr(yr$Date[],6,7)==month[j])
        m_sum_flow_86_12[i,j] = sum(yr[ind,][,4])
        }
    }  
      
m_sum_flow_86_12                            # Monthly average for each year
colnames(m_sum_flow_86_12) = c(1:12)        # Name columns
rownames(m_sum_flow_86_12) = year           # Name Rows

m = melt(t(m_sum_flow_86_12))               # Reshape the data

flow_86_12 = m[1:320,]    # Exclude the last couple of months where we don't have harvest info 

dim(flow_86_12)      # check the dimentions

plot(flow_86_12[,3])

# ---------------------------------------------------------------------------- #
##ok so above you have it plotting the monthly sum of flow from 1986 to 2012
##so now you need to use the trick from the water_q_plots_1986_2012 code
##to create an index vector from 1 to however long the number of monthly sums
##is and plot that as the X
##you would then read in the landings data by month and then make sure that is the
##same length as the monthly flow sums and plot those together

doyster=read.csv("FL_oysters_month.csv",header=T)
AB_oyster = subset(doyster, doyster$Region=='APALACHICOLA BAY AREA')
AB_oyster_1986_2012 = subset(AB_oyster, AB_oyster$Year>'1985')
str( AB_oyster_1986_2012)
# 1986 3, 2012 9, 2012 10, 2012 11, 2012 12,  -- data missing from oyster that is in the flow data
# added 1986 3 to oyster, and deleted the others from the flow data 
# to make a matching set of flow and oyster data 

insert = c(1986,3,NA,NA,NA) # create missing row     

# insert a row where there is missing info to match up with the flow data
AB_oyster_1986_2012_new = rbind(AB_oyster_1986_2012[1:2,],insert,AB_oyster_1986_2012[3:319,])

oyster_86_12=(AB_oyster_1986_2012_new$Pounds/1000)


length(oyster_86_12)                   

#Create a dataframe that has the flow and oyster data for plotting 
plot_data = cbind(flow_86_12,oyster_86_12) 
colnames(plot_data) = c('Month','Year','Flow','Pounds') 
head(plot_data)   
    
# ----------------------------Plots------------------------------------------- #
par(mar=c(5, 4, 4, 4) + 0.1,xpd=T)

plot(plot_data$Flow, type='l', ylab='Total Monthly Flow Volume', main='Monthly Flow Volume and Harvests',xaxt='n',xlab='Year', col='blue')
axis(side=1,at=c(seq(1,324,24)), labels=c('`86','`88','`90','`92','`94','`96','`98','`00','`02',
'`04','`06','`08','`10','`12'),cex.axis=.9)
par(new=T)
plot(plot_data$Pounds, type='p', col='black',yaxt='n',ylab='',xaxt='n',xlab='')
axis(side=4)
mtext('Monthly Oyster Harvest Pounds x 1000',side=4,line=2.5)
legend(250,470, legend=c('River Flow','Harvest'),bty='n', fill=c('blue','black'))

# -----------------------Off-Set Plot----------------------------------------- #
# added the xlim to keep the axis from adjusting when you add values to the index below
index = c(1:length(plot_data[,1]))

par(mar=c(5, 4, 4, 4) + 0.1,xpd=T)
       # add months to the index to shift data
plot(index+24,plot_data$Flow, type='l', ylab='Total Monthly River Flow', main='Monthly Discharge and Lag Harvests',xaxt='n',xlab='Year',xlim=c(0,350))
axis(side=1,at=c(seq(1,324,24)), labels=c('`86','`88','`90','`92','`94','`96','`98','`00','`02',
'`04','`06','`08','`10','`12'),cex.axis=.8)
par(new=T)
plot(index, plot_data$Pounds, type='p', col='blue',yaxt='n',ylab='',xaxt='n',xlab='', xlim=c(0,350))
axis(side=4)
mtext('Total Monthly Pounds of Oyster Harvest',side=4,line=2.5)
legend(250,470, legend=c('River Flow','Harvest'),bty='n', fill=c('black','blue'))




data_check_station=subset(plot_data,plot_data$Year>1987 & plot_data$Year<1991)



### END ###### END ###### END ###### END ###### END ###### END ###### END ###### END ###