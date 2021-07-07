hurdle <- function(v){
  # Are we confident there is a trend?
  # Count portion of hurdle
  data.c <- data.p[data.p$pot_days>0,]
  model.count <- MASS::glm.nb(pot_days~year, data.c)
  low.c<- exp(confint(model.count, level=0.9))[2,1]
  hi.c <- exp(confint(model.count, level=0.9))[2,2]
  
  # Zero portion of hurdle
  data.p$pot_days.z <- !I(data.p$pot_days==0)
  model.zero <- glm(pot_days.z~year, data.p, family = "binomial")
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
  model <- hurdle(pot_days~year, data.p, dist="negbin", zero.dist = "binomial", link = "logit")
  fitted <- unname(model$fitted.values)
  slope <- round((fitted[length(fitted)] - fitted[1])/
                   (max(data.p$year) - min(data.p$year)),2)
  years.for.trend <- sum(!is.na(data.p$pot_days))
  test <- "hurdle"
}