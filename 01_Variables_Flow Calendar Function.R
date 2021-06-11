###################################################################################################
# CESI project part 3- Main function to calculate desired variables&Loops to output the csv
# Overall Descriptions: 
# This section of the script focus on outputing the variables of desire.
# Specifically, the metrics calulated within this portion consists of:
#   1) Annual Mean flows. Annual requires continous year or that the mean of all annual modes is
#      under 0.01 cubic meters/s (then unrecorded dates are considered negligible)(ann_mean_yield);
#   2) Max flow of a station over a desired year(max.flow); 
#   3) Taking the 7-day rolling average, then identifying the minimum value within the bdays
#      (non-ice conditions) (min7summ);
#   4) The upper threshold of a station of desire, measured using the 95th percentile of all
#      the data measured over a desired reference period(pot_threshold); 
#   5) The number of days over a desired year that the flow measured at a desired station
#      exceeds the threshold(pot_days); 
#   6) The number of events over a desired year that the flow measured at a desired station
#      exceeds the threshold(pot_events);
#   7) The max duration of events over threshold at a desired station over a desired year
#      (pot_max_dur) 
#   8) The lower threshold of a station of desire, measured using the 5th percentile of all
#      the data measured over a desired reference period(dr_threshold);
#   9) The number of days over a desired year that the flow measured at a desired station
#      lies beneath the lower threshold(dr_days); 
#   10) The number of events over a desired year that the flow measured at a desired station
#      lies beneath the threshold(dr_events); 
#   11) The max duration of events beneath the lower threshold at a desired station over a 
#      desired year(dut_max_dur); 
# Note: all low flow metrics calculated over the summer period starting at the later of May 1st
# or the last ice-cover day, and ending at the earlier of October 1st and the first ice-cover day.
# After calculating the metrics of interest, a loop will be executed to output a csv for each
# stations. The csv contains all the metrics calculated above.
###################################################################################################

###########      Library&Source      ###########
library(tidyhydat) # HYDAT access
library(dplyr)     # For data tidying and data tables     
library(tidyr)     # for data tidying
library(zoo)       # For working with time series
library(evir)      # For POT
library(purrr)     # function 'possibly' is easiest way to deal with errors
library(lfstat)    # package for low flow statistics
source('01_Variables_File download&Path setup.R')  # Sourcing from file downloads
source('01_Variables_Threshold Building.R')        # Sourcing from the threshold calculation file
source('01_Variables_BdayCalculation.R')           # Sourcing from bday_calculation function

###########  Function:Flow_Calendar  ###########
# flow_calendar takes in id and year, and it will
# calculate and output the 11 metrics representing the station as indicated in stn.years;
# Inputs:
# id is a character that represent the station number. id can be one of the 1014 station 
# id's within the file;
# year is a numeric value that represent the desired year of data being calculated;
# flow_calendar: char Num -> None
# Effects: The function will record all 11 output metrics being calculated and
# write to a csv file located in the Dependecies file.
################################################

flow_calendar <- function(id, year){
  # Identify the station of desire
  #as.data.frame(stn.years)
  #station.id <- stn.years[["STATION_NUMBER"]]
  #year.op <- as.numeric(stn.years[["YEAR"]]) 
  #op <- as.character(stn.years[["OPERATION_CODE"]])
  #flow.daily$Year    <- as.numeric(format(flow.daily$Date, "%Y"))
  #flow.daily$Month   <- as.numeric(format(flow.daily$Date, "%m"))
  #flow.dates = flow.daily %>% filter(Year == year)
  print(year)  #Keep track of the process when writing output
  
  if (nrow(flow.dates%>%filter(!is.na(Value)))<150){
    cat(paste(id, year, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, sep=","), 
        file = output1, append = T, fill = T)
  }else{  
    if (nrow(flow.dates%>%filter(!is.na(Value))) > 0.9 * ifelse(flow.dates$Year[1]%%4==0, 366, 365)){
    ann_mean_yield <- flow.dates %>% mutate(mean_flow = mean(Value, na.rm = TRUE))
    ann_mean_yield <- ann_mean_yield[1, "mean_flow"] %>% round(2)
  } else {
    Mode <- flow.daily  %>% group_by(Year) %>% filter(!is.na(Value)) %>%
      summarise(MODE=names(sort(table(Value), decreasing = TRUE)[1]))
    Mode.multi <- mean(as.numeric(Mode$MODE))
    if (Mode.multi <0.01){
      ann_mean_yield <- flow.dates %>% filter(!is.na(Value)) %>% group_by(Year) %>% 
        summarise(TOTAL=sum(Value)/n())
      ann_mean_yield <- round(ann_mean_yield$TOTAL,2)
    } else {
      ann_mean_yield <- NA
    }
  }
    
    # 2) Max and min flow and months thereof
    max.flow <- flow.dates %>% arrange(desc(Value)) %>% slice(1)
    max.flow <- round(max.flow[["Value"]], digits = 2)
    
    # 3) 7 day min for between B days
    # Determine the minimum 7-day rolling sum
    # Calculate first and last bdays
    bday_calculation(year)
    # Calculating zooflow before interpolation 
    zooflow <- zoo(flow.dates$Value, flow.dates$Date)
    zooflow.summ <- window(zooflow, start = as.Date(lastbday.long),
                           end = as.Date(firstbday.long))
    daymean7summ <- rollmean(zooflow.summ, 7)
    daymin7summ <- which.min(daymean7summ)
    if (length(daymin7summ) > 0){
      min7summ <- round(daymean7summ[[daymin7summ]],2)
    } else {
      min7summ <- NA
    }
    
    # 4)-7) Peaks Over Threshold (POT) metrics using the 95th percentile of the 1981-2010 period
    #  Including pot_threshold, pot_events, pot_days, pot_max_dur
    station_row <- Threshold %>% filter(STATION_NUMBER==id)
    if (is.na(station_row$Q95)){
      cat(paste(id, year, ann_mean_yield, NA, NA, NA, NA, max.flow, NA, NA, NA, NA, min7summ, sep=","), 
          file = output1, append = T, fill = T)
    }else{
      threshold <- as.numeric(station_row$Q95)
      # POT function triggers 'computational singularity' warning or error when threshold >=10
      # This factor scales inputs and one of the outputs below
      if (max.flow<threshold){
        pot_days<-0
        pot_events<-0
        pot_max_dur<-0
        pot_threshold<-threshold
      }else{
        factor <- case_when( threshold >= 1000 ~ 1000,
                             threshold >= 100 ~ 100,
                             threshold >= 10 ~ 10,
                             threshold >= 0 ~ 1)
        pot2 <- possibly(evir::pot, otherwise = NA)
        pot <- pot2(flow.dates$Value[!is.na(flow.dates$Value)]/factor, threshold/factor) 
        # here is the singularity warning!
        
        if (!is.list(pot)) {
          pot_days <- 0
          pot_events <- 0
          pot_threshold <- threshold
          pot_max_dur <- 0
          
        } else {
          pot_days <- length(pot$data)
          pot.dates <- data.frame(dates = attr(pot$data, "times"))
          pot.dates$lag <- pot.dates$dates - lag(pot.dates$dates, 1)
          # At this point, we implement a check for the correlation between flood events
          # Referring to Lang etal (1999), we set two conditions:
          # 1) R > 5+log(A/1.609^2);
          # 2) Xmin < 0.75 * min(Xi, Xj);
          # where R is the time in days between two peaks, A is the watershed area in squared kilometers;
          # Xmin is the minimum flow between the adjacent flow peaks Xi and Xj. 
          # In order for two seperate peaks to be considered as independent events of drought or flood, 
          # both conditions need to be met. If either of the conditions fail, we consider two events
          # as the same one.   R is defined within the iteration
          pot_threshold <- threshold
          seq_blocks <- split(pot.dates$dates, cumsum(c(TRUE, diff(pot.dates$dates) != 1)))
          # We iterate within seq_blocks to find maximum flow within each events, before testing
          # their independence
          events_count <- c()
          for (i in 1:length(seq_blocks)){
            events_count <- append(events_count, length(seq_blocks[[as.character(i)]]))
          }
          events_array <- c()
          for (i in 1:length(events_count)){
            j <- events_count[i]
            while (j > 0){
              events_array<-append(events_array,i)
              j <- j-1
            }
          }
          pot_dat <- data.frame(data = pot$data, dates=attr(pot$data, "times"),event_num=events_array)
          max_peak <- pot_dat %>% group_by(event_num) %>% summarise(max=max(data))
          max_peak_central<-c()
          for (i in 1:length(max_peak$max)){
            max_peak_dates<-(pot_dat %>% filter(data==max_peak$max[i],dates>=seq_blocks[[i]][1],
                                                dates<=seq_blocks[[i]][length(seq_blocks[[i]])]))$dates
            if (length(max_peak_dates)%%2==1){
              max_peak_dates=max_peak_dates[(length(max_peak_dates)+1)/2]
            } else {
              max_peak_dates=(max_peak_dates[length(max_peak_dates)/2]+
                                max_peak_dates[length(max_peak_dates)/2+1])/2
            }
            max_peak_central=append(max_peak_central, max_peak_dates)
            max_peak_dates=c()
          }
          max_peak$central_dates<-max_peak_central
          R_vec<-c()
          for (i in 1:length(max_peak$max)){
            R_vec<-append(R_vec, R)
          }
          max_peak$R_vec<-R_vec
          Xmin_vec<-c()
          if (length(max_peak$max)==1){
            Xmin_vec<-append(Xmin_vec, NA)
          }else{ for( i in 1:(length(max_peak$max)-1)){
            Xmin_vec<-append(Xmin_vec, 
                             0.75*min(max_peak$max[i],max_peak$max[i+1])*factor)
          } 
            Xmin_vec<-append(Xmin_vec, NA)
          }
          max_peak$Xmin_vec<-Xmin_vec
          ind_events<-c()
          if (length(seq_blocks)>1){
            for (i in 1:(length(seq_blocks)-1)){
              range_flow <- flow.dates %>% filter(Date>=as.Date(max_peak$central_dates[i], origin=paste0(year, "-01-01")),
                                                  Date<=as.Date(max_peak$central_dates[i+1],origin=paste0(year, "-01-01")))
              if (all(min(range_flow$Value, na.rm=TRUE)<Xmin_vec[i],
                      ((max_peak$central_dates[i+1] - max_peak$central_dates[i])>R))){
                ind_events <- append(ind_events, 1)
              }else{
                ind_events <- append(ind_events, 0)
              }
            }
            ind_events <- append(ind_events, 1)
          } else { ind_events <- c(1)}
          max_peak$ind_events<-ind_events
          pot_events <- sum(ind_events)
          max_dur_count <- c()
          count<-0
          for (i in 1:length(ind_events)){
            if (ind_events[i]==1 | is.na(ind_events[i])){
              max_dur_count = append(max_dur_count, count + events_count[i])
              count = 0
            } else { # i.e. in_events[i] == 0
              count <- count + events_count[i]}
          }
          pot_max_dur <- max(max_dur_count)
        }
      }
      
      
      # 7)-10) Dips Under Threshold (DUT) metrics using 5th percentile of the 1981-2010 period 
      # using lfstat packagelimited to dates between last spring ice cover day and first fall ice cover day
      # Convert to zoo object (for time series)
      dates_value=flow.dates$Value
      if (is.na(flow.dates$Value[1])){
        front_ind=0
        while(is.na(dates_value[1])){
          dates_value=dates_value[2:length(dates_value)]
          front_ind=front_ind+1
        }
        date_ref_front=flow.dates$Date[front_ind+1]
        flow.dates=flow.dates %>% filter(as.Date(Date)>=as.Date(date_ref_front))
      }
      if (is.na(dates_value[length(dates_value)])){
        end_ind=0
        while(is.na(dates_value[length(dates_value)])){
          dates_value=dates_value[1:(length(dates_value)-1)]
          end_ind=end_ind+1
        }
        date_ref_end=flow.dates$Date[length(flow.dates$Value)-end_ind]
        flow.dates=flow.dates %>% filter(as.Date(Date)<=as.Date(date_ref_end))
      }
      date_vec<-flow.dates$Date
      flow.new<-flow.dates%>%mutate(Date=seq(1, n()))%>%mutate(Value=approx(Date,Value,Date)$y)
      flow.new$Date<-date_vec
      zooflow <- zoo(flow.new$Value, flow.new$Date)
      if (length(zooflow) > 60){
        low <- as.numeric(station_row$Q5)
        data.s <- data.frame(day= as.numeric(substring(flow.new[["Date"]], 9,10)),
                             month=as.numeric(substring(flow.new[["Date"]],6,7)),
                             year=year,flow=flow.new$Value, hyear=year,
                             baseflow=NA,
                             date=paste0(year,"-",substring(flow.new[["Date"]],6,11)))
        
        data.s <- data.s %>% filter(as.Date(data.s$date) >= as.Date(lastbday.long) & 
                                      as.Date(data.s$date) <= as.Date(firstbday.long))
        
        data.lf <- createlfobj(data.s)
        flowunit(data.lf) <- "m^3/s"
        drought <- find_droughts(data.lf, threshold = low)
        summary2 <- possibly(base::summary, otherwise = data.frame(event.no=NULL))
        duration<-summary2(drought)
        
        if (any(duration[1,"event.no"] == 0, nrow(duration)==0)){
          dr_days <- 0
          dr_events <- 0
          dut_max_dur <- 0
          
        } else {
          dr_days <- sum(duration$duration)
          # In order to measure the independence between drought events, we set the following
          # constraints:
          # 1. t >= 5
          # 2. the excess volume, Vabove / min(Vbelowi, Vbelowj)>=0.1
          # where t is the time between events, Vabove is the amount that exceeds the threshold,
          # Vbelowi and Vbelowj are the volumes below during events i and j.
          vol<-c()
          for (i in 1:length(drought$def.increase)){
            vol<-append(vol, drought$def.increase[[i]])
          }
          drevent_count<-c()
          zero_count<-c()
          for ( i in 1:length(drought$event.no)){
            drevent_count<-append(drevent_count, drought$event.no[[i]])
            if (drought$event.no[[i]]==0){
              zero_count<-append(zero_count, i)
            }
          }
          ind<-0
          if (length(zero_count)==0){ #i event that last for the entire period
            dr_days=duration$duration
            dr_events=1
            dur_max_dur=duration$duration
          } else if (length(zero_count)==1){
            drevent_count[zero_count[1]]<-"0_1"
          } else{
            for ( i in 1:(length(zero_count)-1)){
              if ((zero_count[i]+1)==zero_count[i+1]){
                drevent_count[zero_count[i]]<-paste0(as.character(ind), "_", as.character(ind+1))
              } else{
                drevent_count[zero_count[i]]<-paste0(as.character(ind), "_", as.character(ind+1))
                ind<-ind+1
              }
              drevent_count[zero_count[length(zero_count)]]<-paste0(as.character(ind), "_", as.character(ind+1))
            }
          }
          dr_filter<-data.frame(volume=vol, dr_event_count=drevent_count)
          dr_summary<-dr_filter %>% group_by(dr_event_count) %>% summarise(sum=sum(volume))
          dr_ind_events<-c()
          if (nrow(duration)>1){
            for ( i in 1:(nrow(duration)-1)){
              t <- as.numeric(duration$start[i+1]-duration$end[i])
              v_above<-(dr_summary %>% filter(dr_event_count==paste0(as.character(i), "_", 
                                                                     as.character(i+1))))$sum
              if (any(t>=5, abs(v_above/min(duration$volume[i], duration$volume[i+1]))>=0.1)){
                dr_ind_events<-append(dr_ind_events, 1)
              }else{
                dr_ind_events<-append(dr_ind_events, 0)
              }
            }
            dr_ind_events<-append(dr_ind_events, 1)
          }else{
            dr_ind_events<-c(1)
          }
          dr_events <- sum(dr_ind_events)
          dr_max_dur_count <- c()
          dr_count<-0
          for (i in 1:length(dr_ind_events)){
            if (dr_ind_events[i]==1){
              dr_max_dur_count <- append(dr_max_dur_count, dr_count + duration$duration[i])
              dr_count <- 0
            } else { # i.e. in_events[i] == 0
              dr_count <- dr_count + duration$duration[i]}
          }
          dut_max_dur <- max(dr_max_dur_count)              
        }    
      } else {
        dr_days <- NA
        dr_events <- NA
        dut_max_dur <- NA
        
      }
      dr_threshold <- low
      cat(paste(id, year, ann_mean_yield, pot_threshold, pot_days, 
                pot_events, pot_max_dur, max.flow, dr_threshold, dr_days,
                dr_events, dut_max_dur, min7summ, sep=","), 
          file = output1, append = T, fill = T)
    }
  }
}


#### Obtain flow data and calculate hydrological variables ####
stations <- read.csv("./Dependencies/RHBN_U.csv", header = TRUE)
list <-as.character(stations$STATION_NUMBER)
# select station from list, analyse, write to csv.
for (i in 1:length(list)){
  stn.id <- list[i]
  # 1) R > 5+log(A/1.609^2);
  stn_area <- stations$Shp_Area[stations$STATION_NUMBER == stn.id]
  R <- 5 + log(stn_area/(1.609^2))%>%round(1)
  print(paste("*************",length(list)-i, stn.id, "*************", sep = "  "))
  flow.daily <- hy_daily_flows(stn.id)
  flow.daily$Year <- as.numeric(format(flow.daily$Date, "%Y"))
  flow.daily$Month<- as.numeric(format(flow.daily$Date, "%m"))  # Change as necessary
  Years <- unique(flow.daily$Year) # All years with any data
  output1 <- paste("./Variables/", stn.id, ".csv", sep= "")
  fileupdate <- FALSE
  if (file.exists(output1)){
    # file.remove(output1) 
    # use this when making changes to past csvs
    fileupdate <- TRUE
    current_dat = read.csv(output1, header=TRUE)
    Years = Years[(length(current_dat$year)) :(length(Years))]
    current_dat = current_dat[-(nrow(current_dat)), ] #should remove the last row of data
    write.csv(current_dat, output1, row.names=FALSE)
  }
  Year_list=length(Years)
  for (i in 1:Year_list){
    flow.dates = flow.daily %>% filter(Year == Years[i])
    flow_calendar(stn.id,Years[i])
  }
}

# Add a header to the spreadsheet
if (!fileupdate){
  header1 <- read.csv(output1, header = FALSE)
  print("New file; add header")
  colnames(header1) <- c("station", "year", "ann_mean_yield", "pot_threshold",
                         "pot_days", "pot_events",  "pot_max_dur",
                         "1_day_max", "dr_threshold", "dr_days", "dr_events", "dut_max_dur",
                         "7_day_min")
  write.csv(header1, file = output1, row.names = FALSE)
}

# Zhou's comments on this section
# 1. The variable name is pot.exceed.mean, the output's name is pot_mean_exceedance(removed for now)
# 2. Moved zooflow from the annual mean flow section-better to define it afterwards?
# 3. Remove the comments on singularity warnings 
# 4. Min flow is not used;remove the min_flow metric from comment&move the code to unused
# Jenn comments
# 1. Removed dependency on stn.op.schedule, now attempts to calculate on all years with any data.