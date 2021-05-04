###################################################################################################
# CESI project part 3- Bdays Calculation
# Overall Desciption:
# This portion of the code calculates the first and last bday at a given year at a station.
# The first and last bdays are defined as the last ice covered day and the first ice covered day
###################################################################################################

################    library    #################
library(dplyr)
library(zoo)
library(utils)

#########  Function: bday_calculation  ######### 
bday_calculation <- function(y){
  flow.late <<- flow.daily %>% filter(Year == y & Month > 8)
  firstbday <<- flow.late %>% filter(Symbol == "B", Month >= 10) %>% head(1)
  firstbday.date <<- ifelse(nrow(firstbday)>0, substring(firstbday$Date,1,10), NA)
  
  firstbday.long <<- ifelse(is.na(firstbday.date),paste0(y,"-10-01"),
                           ifelse(firstbday.date > as.Date(paste0(y, "-10-01")), 
                                  paste0(y, "-10-01"),firstbday.date))
  # Last B day
  flow.early <<- flow.daily %>% filter(Year == y & Month < 6)
  lastbday <<- flow.early %>% filter(Symbol == "B", Month < 6) %>% tail(1)
  lastbday.date <<- ifelse(nrow(lastbday)>0, substring(lastbday$Date,1,10), NA)
  
  lastbday.long <<- ifelse(is.na(lastbday.date),paste0(y,"-05-01"),
                          ifelse(lastbday.date < as.Date(paste0(y, "-05-01")),
                                 paste0(y,"-05-01"),lastbday.date))
}