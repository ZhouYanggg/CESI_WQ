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
var.t = ""
while (!(var.t %in% colnames(read.csv(paste("../Variables/", list[1], ".csv", sep= ""), header = TRUE)))){
  var.t = readline(prompt = "Insert the name of the metric you would like to compute: ")
}
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


for (i in 1: length(list)){
  stn.id <- list[i]
  print(stn.id)
  output1 <- paste("../Variables/", stn.id, ".csv", sep= "")
  
  # defaults to NA if data requirements aren't met
  slope <- NA
  years.for.trend <- NA
  CATTrend <- NA
  test <- NA
  
  # Load data and subset
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
      if(sum(data.p[[var.t]]==0)>=3){
        
        # Is hurdle necessary?
        model2 <- tryCatch(hurdle(pot_days~year, data.p, dist="negbin", zero.dist = "negbin"),
                           error=function(e){return("A")}, warning=function(w){return("B")})
        
        hurdle<- FALSE #default to False
        if(!is.character(model2)){
          if(!is.nan(hurdletest(model2)[2,4])){
            hurdle <- ifelse(hurdletest(model2)[2,4]>0.1, TRUE, FALSE)
          }
        }
        
        if(hurdle){
          #Apply the hurdle model
          hurdle(var.t)
        } else {
          #Apply the negative binomial model
          negbin(var.t)
        }
      } else {
        model <- glm.nb(pot_days~year, data.p)
        low<-exp(confint(model, level=0.9))[2,1]
        hi<-exp(confint(model, level=0.9))[2,2]
        # confidence test @ 70% and 90% confidence
        pass <- ifelse(all(low<=1, hi>=1), "Maybe?", "Confident")
        if (pass=="Maybe?"){
          low<- exp(confint(model, level=0.7))[2,1]
          hi<-exp(confint(model, level=0.7))[2,2]
          pass <- ifelse(all(low<=1, hi>=1),"Uncertain", "Likely")
        }
        # Get slope from negative binomial
        fitted <- unname(model$fitted.values)
        slope <- round((fitted[length(fitted)] - fitted[1])/
                         (max(data.p$year) - min(data.p$year)),2)
        years.for.trend <- sum(!is.na(data.p$pot_days))
        test <- "glm.nb"
        # # use Mann-Kendall
        #  x <- data.p[[var.t]]
        #  y <- data.p$year
        #  sen <- zyp.sen(y~x)
        #  corr<- cor.test(y, x, method = "kendall", alternative = "two.sided", exact = FALSE)
        #  Z <-abs(corr$statistic[["z"]])
        #  slope <- round(sen$coefficients[[2]],2)
        #  years.for.trend <- sum(!is.na(data.p$pot_days))
        #  CATTrend <- case_when(Z>=1.28 ~ "Confident",
        #                        Z>=0.52 & Z<1.28 ~ "Likely",
        #                        Z<0.52 ~ "Uncertain")
        #  test <- "MK"
      }
      CATTrend <- pass
    }
  } 
  
  
  snap[[i]] <- data.frame(station=stn.id, slope=slope, 
                          years.for.trend=years.for.trend,
                          CATTrend=CATTrend, test=test)
}

snap.all <- bind_rows(snap)
pot_days_output <- snap.all
