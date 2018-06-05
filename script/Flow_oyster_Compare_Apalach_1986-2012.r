   ################################################################################
#    Plots of monthly flow sum and landings            
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

index_flow_86_12 = c(1:length(m_sum_flow_86_12))

plot(index_flow_86_12,m_sum_flow_86_12)

##ok so above you have it plotting the monthly sum of flow from 1986 to 2012
##so now you need to use the trick from the water_q_plots_1986_2012 code
##to create an index vector from 1 to however long the number of monthly sums
##is and plot that as the X
##you would then read in the landings data by month and then make sure that is the
##same length as the monthly flow sums and plot those together

doyster=read.csv("FL_oysters_month.csv",header=T)
AB_oyster = subset(doyster, doyster$Region=='APALACHICOLA BAY AREA')
AB_oyster_1986_2012 = subset(AB_oyster, AB_oyster$Year>'1985')

oyster_landings=  (AB_oyster_1986_2012$Pounds/100000)

index_AB_oyster_1986_2012 = c(1:length(AB_oyster_1986_2012$Pounds))
plot(index_AB_oyster_1986_2012,oyster_landings)
plot(m_sum_flow_86_12,AB_oyster_1986_2012$Pounds)     


