###################################################################################################
# CESI project-Trend Test
# Before running this code, make sure that you have access to the following:
# 02_Figures_MK.R
# 02_Figures_Negbin.R
# 02_Figures_Hurdle.R
#
# This part of the code aims at finding both the existence and magnitude of trend within 
# metrics calculated in part 01. While normally Mann-Kendall Test is used normally, 
# there are times that the outcome of MK Test may not be robust enough. Specifically, 
# for a set of data that contains numerous zeros or ties, the trend detected by MK Test
# appear to be less reliable. Therefore, we incorporate the concept of general linear models
# that performs better when dealing with zeros. More specifically, we introcduce the hurdle as
# well as negative binomial models to help figuring out potential trend for metrics. 
###################################################################################################

################ Sourcing from the model building scripts&Setting environment ####################
stations <- read.csv("../Dependencies/RHBN_U.csv", header = TRUE)
list <-as.character(stations$STATION_NUMBER)
### Prompt to get the metric name
var_list = c("ann_mean_yield",
  "pot_days", "pot_events",  "pot_max_dur",
  "1_day_max", "dr_days", "dr_events", "dut_max_dur",
  "7_day_min") 
#var_list = c("dr_days")
# Use this when testing single variable
#var.t = 
snap <- list()
##################################################################################################

################ libraries ####################
library("dplyr")
#library("pscl")
library("MASS")
#install.packages("countreg", repos="http://R-Forge.R-project.org") 
# might be a better way to do this; there's no point in reinstalling every time...
library("countreg")
library("zyp")
###############################################

for (j in var_list){
  var.t = j
  print(var.t)
  output_name = paste0("./Summary_", var.t, "_trends.csv")
  for (i in 1: length(list)){
    stn.id <- list[i]
    print(stn.id)
    output1 <- paste("../Variables/", stn.id, ".csv", sep= "")
    
    # defaults to NA if data requirements aren't met
    slope <- NA
    intercept <- NA
    years.for.trend <- NA
    CATTrend <- NA
    test <- NA
    
    data <- read.csv(output1, header = TRUE)
    if (sum(!is.na(data[[var.t]]))>=30){
      data <- data[!is.na(data[[var.t]]),]
      data.p <- data[data$year>=1970,]
      
      # Data requirements: some data 1970-1975, >=30 points, no gap over 10 years
      goodyears <- data.p$year[!is.na(data.p[[var.t]])]
      gap.check <- na.omit(goodyears - lag(goodyears))
      
      if (all(any(goodyears %in% c(1970:1975)), (length(goodyears) >= 30), 
              (max(gap.check) <= 11))){
        
        # Are there any zero values?
        if((sum(data.p[[var.t]]==0)<3)&(var.t %in% c("1_day_max", "ann_mean_yield", "7_day_min"))){
          mk_test(var.t)
          }else{
            # Is hurdle necessary?
            model2 <- tryCatch(hurdle(var.t~year, data.p, dist="negbin", zero.dist = "negbin"),
                               error=function(e){return("A")}, warning=function(w){return("B")})
            
            hurdle<- FALSE #default to False
            if(!is.character(model2)){
              if(!is.nan(hurdletest(model2)[2,4])){
                hurdle <- ifelse(hurdletest(model2)[2,4]>0.1, TRUE, FALSE)
              }
            }
            if(hurdle){
              #Apply the hurdle model
              hurdle_test(var.t)
            } else {
              #Apply the negative binomial model
              negbin(var.t)
            }
            CATTrend <- pass
          }
        }
      } 
    # Load data and subset
    snap[[i]] <- data.frame(station=stn.id, slope=slope, intercept=intercept,
                            years.for.trend=years.for.trend,
                            CATTrend=CATTrend, test=test)
  }
  snap.all <- bind_rows(snap)
  write.csv(snap.all, output_name, row.names = FALSE)
  snap <-list()
}



