################################################################################
#    Boxplots of mean monthly discharge for pre-2000 data with
#    2008-2012 mean monthly points overlayed            
############################################################################### 
#setwd('C:\\Users\\Mike Dodrill\\Desktop\\Flow Summaries\\DATA')
setwd('F:/Oyster/Apalachicola oyster/Apalach flow data/Flow_sum')

rm(list=ls(all=TRUE))

d1=read.csv("Apalach_flow_1950_2012.csv")                             
str(d1)                                                               
head(d1)                                                              
                                                                                                                 
class(d1$Date)                                                        
d1$Date = as.POSIXct(strptime(as.character(d1$Date),"%m/%d/%Y"))     
# -------------------------- pre 2000 ---------------------------------------- #
test = which(substr(d1$Date,1,4)=='2008')[1]     # find which row a specific year starts
pre_2000 = d1[1:17989,]                          # Pre 2000 data
head(pre_2000)

month = as.character(c('01','02','03','04','05','06','07','08','09','10','11','12'))
year = unique(substr(pre_2000$Date,1,4))         # Unique years for pre 2000 data
m_avg_pre_2000 = matrix(NA,length(year),12)      # object to fill with loop 

# Calculate mean monthy discharge 
for(i in 1:length(year)){                        
    yr = subset(d1, substr(d1$Date,1,4)==year[i])       # loop across years
        for(j in 1:length(month)){                      # loop across months
        ind = which(substr(yr$Date[],6,7)==month[j])
        m_avg_pre_2000[i,j] = mean(yr[ind,][,4])
        }
    }  
      
m_avg_pre_2000                            # Monthly average for each year

# boxplot(m_avg_pre_2000[,1:12])    #  , col='lightblue'
# ---------------------------- 2008-2012 ------------------------------------- #
d07_12 = d1[20547:22647,]                  # 2008-2012 data 
head(d07_12)

# Calculate mean monthy discharge 
year2 = unique(substr(d07_12$Date,1,4))
m_avg_07_12 = matrix(NA,length(year2),12)

for(i in 1:length(year2)){
    yr = subset(d1, substr(d1$Date,1,4)==year2[i])
        for(j in 1:length(month)){
        ind = which(substr(yr$Date[],6,7)==month[j])
        m_avg_07_12[i,j] = mean(yr[ind,][,4])
        }
    }  

m_avg_07_12                              # Monthly average for each year

#boxplot(m_avg_08_12[,1:12], col='lightblue')

# ----------------------------- Plot ----------------------------------------- #
# need to ad x and y labels, legend 

boxplot(m_avg_pre_2000[,1:12], main='Apalachacola River', xlab=c('Month'))
mtext(expression(paste('Mean Monthly Discharge','  ',(ft^3 / sec))),side=2, line=2.5)
points(jitter(c(1:12)),m_avg_07_12[1,],col='red', pch=19)
points(jitter(c(1:12)),m_avg_07_12[2,],col='orange', pch=19)
points(jitter(c(1:12)),m_avg_07_12[3,],col='blue', pch=19)
points(jitter(c(1:12)),m_avg_07_12[4,],col='green', pch=19)
points(jitter(c(1:12)),m_avg_07_12[5,],col='yellow', pch=19)
points(jitter(c(1:12)),m_avg_07_12[6,],col='purple', pch=19)
# legend(9.5,95000, legend=c('2007 - 2012'), bty='n', pch=21,pt.bg=c('red'))

# alternate legend
legend(11,95000, legend=c('2007','2008','2009','2010','2011','2012'),
bty='n', pch=21,pt.bg=c('red','orange','blue','green','yellow','purple'))
                                        
### END ###### END ###### END ###### END ###### END ###### END ###### END ###### END ### 