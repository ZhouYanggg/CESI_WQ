####################################################################################################
# CESI Project Part 2  Threshold for Stations
# Overall Description:
# This portion of the code create a data frame that records the upper and lower threshold at
# 1014 reference stations implemented by part 1);
# The upper threshold is calculated by the 95th percentile of data from year 1981 to 2010;
# The lower threshold is calculated by the 5th percentile of data from year 1981 to 2010;
# Requirements:
# In order to obtain the threshold for a station, the station must satifiy the following conditions:
# 1. Contain at least 20 years of data between 1981 and 2010;
# 2. Each year has at least 150 days of data;
# If either of the conditions are not satisfied, threshold will not be calculated for that year;
# A NA will be put into the place of both 'Q95' and 'Q5'
# ***Remember to run this first before executing the main function in part 3)***
###################################################################################################

###########      Library&Source      ###########
library(tidyhydat)    # HYDAT access
library(dplyr)        # For data tidying and data tables     
library(tibble)       # For filter function
source('01_Variables_File download&Path setup.R')
source('01_Variables_BdayCalculation.R')

###########   Calculate Threshold    ###########
stations <- read.csv("../Dependencies/RHBN_U.csv", header = TRUE)
list <-as.character(stations$STATION_NUMBER)
Upper = c()
Lower = c()
for (i in 1:length(list)){
  stn.id <- list[i]
  flow.daily <- hy_daily_flows(stn.id)
  flow.daily$Year = substr(flow.daily$Date, 1, 4)
  ref <- flow.daily %>% filter(Year>=1981, Year<=2010)
  summ = ref %>% group_by(Year) %>% summarise(count=length(Value[!is.na(Value)]))
  if (length(summ$Year)<20 | length(summ$count[summ$count>=150])<length(summ$count)){
    Upper=append(Upper, NA)
    Lower=append(Lower, NA)
    print(paste0(stn.id, "---not enough data"))
  }else{
    Upper_All=ref$Value
    for (i in 1:length(Upper_All)){
      if (is.na(Upper_All[i])){
        Upper_All[i]=0
      }
    }
    yr=summ$Year
    num_leap_yrs=length(yr[as.numeric(yr)%%4==0])
    total_length=length(yr)*365+num_leap_yrs
    Upper_All=append(Upper_All, rep.int(0, total_length-length(Upper_All)))
    
    Lower_All=c()
    # First B day
    flow.daily$Month = as.numeric(format(flow.daily$Date, "%m"))
    for (i in 1:length(summ$Year)){
      year.op=summ$Year[i]
      bday_calculation(year.op)
      Value_within=(ref %>% filter(as.Date(Date)>=as.Date(lastbday.long)&as.Date(Date)<=as.Date(firstbday.long)))$Value
      Value_within=Value_within[Value_within!=0]
      Lower_All=append(Lower_All, Value_within)
    }
    Upper=append(Upper,quantile(Upper_All, probs = 0.95, names = FALSE, na.rm = TRUE))
    Lower=append(Lower,quantile(Lower_All, probs = 0.05, names = FALSE, na.rm = TRUE))
    print(stn.id)
  }
}
Threshold = data.frame(STATION_NUMBER = list, Q95=Upper, Q5=Lower)
write.csv(Threshold, file = "../Dependencies/Threshold_New.csv", row.names = FALSE)
