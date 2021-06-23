# Hurdle trends for pot_max_dur
# to be incorporated into other code later

library("dplyr")
#library("pscl")
library("MASS")
install.packages("countreg", repos="http://R-Forge.R-project.org") # might be a better way to do this; there's no point in reinstalling every time...
library("countreg")
library("zyp")


# Streamlined -------------------
var.t <- "pot_max_dur"   # Couldn't replace variable name when using models

stations <- read.csv("../Dependencies/RHBN_U.csv", header = TRUE)
list <-as.character(stations$STATION_NUMBER)

snap <- list()

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
        model2 <- tryCatch(hurdle(pot_max_dur~year, data.p, dist="negbin", zero.dist = "negbin"),
                           error=function(e){return("A")}, warning=function(w){return("B")})
        hurdle<- FALSE #default to False
        if(!is.character(model2)){
          if(!is.nan(hurdletest(model2)[2,4])){
            hurdle <- ifelse(hurdletest(model2)[2,4]>0.1, TRUE, FALSE)
          }
        }
        
        if(hurdle){
          
          # Are we confident there is a trend?
          # Count portion of hurdle
          data.c <- data.p[data.p$pot_max_dur>0,]
          model.count <- MASS::glm.nb(pot_max_dur~year, data.c)
          low.c<- exp(confint(model.count, level=0.9))[2,1]
          hi.c <- exp(confint(model.count, level=0.9))[2,2]
          
          # Zero portion of hurdle
          data.p$pot_max_dur.z <- !I(data.p$pot_max_dur==0)
          model.zero <- glm(pot_max_dur.z~year, data.p, family = "binomial")
          low.z<-exp(confint(model.zero, level=0.9))[2,1]
          hi.z<-exp(confint(model.zero, level=0.9))[2,2]
          
          # combined confidence test @ 70% and 90% confidence
          pass <- ifelse(all(low.c*low.z<=1, hi.c*hi.z>=1), "Maybe?", "Confident")
          if (pass == "Maybe?"){
            low.c<- exp(confint(model.count, level=0.7))[2,1]
            hi.c <- exp(confint(model.count, level=0.7))[2,2]
            low.z<-exp(confint(model.zero, level=0.7))[2,1]
            hi.z<-exp(confint(model.zero, level=0.7))[2,2]
            pass <- ifelse(all(low.c*low.z<=1, hi.c*hi.z>=1), "Uncertain", "Likely")
          }
          
          # Get slope from hurdle
          model <- hurdle(pot_max_dur~year, data.p, dist="negbin", zero.dist = "binomial", link = "logit")
          fitted <- unname(model$fitted.values)
          slope <- round((fitted[length(fitted)] - fitted[1])/
                           (max(data.p$year) - min(data.p$year)),2)
          years.for.trend <- sum(!is.na(data.p$pot_max_dur))
          test <- "hurdle"
          
          } else {
            model <- glm.nb(pot_max_dur~year, data.p)
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
            years.for.trend <- sum(!is.na(data.p$pot_max_dur))
            test <- "glm.nb"
          }
        } else {
          model <- glm.nb(pot_max_dur~year, data.p)
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
          years.for.trend <- sum(!is.na(data.p$pot_max_dur))
          test <- "glm.nb"
          # # use Mann-Kendall
          #  x <- data.p[[var.t]]
          #  y <- data.p$year
          #  sen <- zyp.sen(y~x)
          #  corr<- cor.test(y, x, method = "kendall", alternative = "two.sided", exact = FALSE)
          #  Z <-abs(corr$statistic[["z"]])
          #  slope <- round(sen$coefficients[[2]],2)
          #  years.for.trend <- sum(!is.na(data.p$pot_max_dur))
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
pot_max_dur_output <- snap.all
write.csv(snap.all, "../Variables/Summary_pot_max_dur_trends.csv", row.names = FALSE)


