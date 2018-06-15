################################################################################
#     Calculates yearly or montly flow volumns with plots
#
###############################################################################
#setwd('C:\\Users\\Mike Dodrill\\Desktop\\Flow Summaries\\DATA')
setwd('C:\\Users\\UF\\Documents\\Projects\\Bill_Pine\\R_Water_Q\\Summaries\\DATA')
rm(list=ls(all=TRUE))

d1=read.csv("Apalach_flow_1950_2012.csv")
str(d1)
head(d1)

class(d1$Date)
d1$Date = as.POSIXct(strptime(as.character(d1$Date),"%m/%d/%Y"))
# ---------------------------------------------------------------------------- #
year = unique(substr(d1$Date,1,4))               # Unique years for the dataset
year_flow_vol = matrix(NA,length(year),1)        # object to fill with loop

# Calculate yearly flow volumns
# Note: substr below is like right or left in excel, it subsets for characters in a string
for(i in 1:length(year)){
    yr = subset(d1, substr(d1$Date,1,4)==year[i])       # loop across years
    year_flow_vol[i,1] = sum(yr$Discharge)
    }

year_flow_vol                            # Total flow volumn for each year

# Plot
plot(c(1:length(year)),year_flow_vol,type='l',ylab='',main='Title', xlab='',xaxt='n')
mtext(expression(paste('Annual Flow Volumn ','  ',(ft^3 / sec))),side=2, line=2.5)
axis(side=1,at=seq(1,length(year),by=10),labels=year[seq(1,length(year),by=10)])

lines(lowess(year_flow_vol,f=.2),col='red',)     # Smoothed line fit, see ?lowess, can adjust f value to make
                                                 # more or less 'smooth', larger values = more smooth
                                                 
# ---------------------------------------------------------------------------- #
month = as.character(c('01','02','03','04','05','06','07','08','09','10','11','12'))
month_flow_vol = matrix(NA,length(year),12)      # object to fill with loop

# Calculate monthy discharge
for(i in 1:length(year)){
    yr = subset(d1, substr(d1$Date,1,4)==year[i])       # loop across years
        for(j in 1:length(month)){                      # loop across months
        ind = which(substr(yr$Date[],6,7)==month[j])
        month_flow_vol[i,j] = sum(yr[ind,4])
        }
    }

month_flow_vol     #Monthly flow volumn

boxplot(month_flow_vol[,1:12], main='Title', col='lightblue')
mtext(expression(paste('Monthly Flow Volumn ','  ',(ft^3 / sec))),side=2, line=2.5)

### END ###### END ###### END ###### END ###### END ###### END ###### END ###### END ###