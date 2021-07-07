negbin <- function(v){
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
}