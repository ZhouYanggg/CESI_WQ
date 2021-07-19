###################################################################################################
# CESI project-Negative Binomial Test
# This part of the code provides a function that calculates the trend of a set of data based
# on negative binomial Test.
###################################################################################################
negbin <- function(v){
  model <<- glm.nb(data.p[[v]]~data.p$year)
  low<-exp(confint(model, level=0.9))[2,1]
  hi<-exp(confint(model, level=0.9))[2,2]
  if (!is.na(low)&!is.na(hi)){
    # confidence test @ 70% and 90% confidence
    pass <<- ifelse(all(low<=1, hi>=1), "Maybe?", "Confident")
    if (pass=="Maybe?"){
      low<<- exp(confint(model, level=0.7))[2,1]
      hi<<-exp(confint(model, level=0.7))[2,2]
      pass <<- ifelse(all(low<=1, hi>=1),"Uncertain", "Likely")
    }
    # Get slope from negative binomial
    fitted <<- unname(model$fitted.values)
    slope <<- round((fitted[length(fitted)] - fitted[1])/
                      (max(data.p$year) - min(data.p$year)),2)
    intercept <<- round((fitted[1]-min(data.p$year)*((fitted[length(fitted)] - fitted[1])/
                                                       (max(data.p$year) - min(data.p$year)))),2)
    years.for.trend <<- sum(!is.na(data.p[[v]]))
    test <<- "glm.nb"
  }else{
    pass <<- NA
  }  
}