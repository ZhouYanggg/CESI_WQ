###################################################################################################
# CESI project-Mann-Kendall Test
# This part of the code provides a function that calculates the trend of a set of data based
# on Mann-Kendall Test.
# In order for the data set to be fed into this function, it needs to first satisfy the 
# following requirements:
# 1. Some data exists within the range 1970-1975, contains >=30 points, no gap over 10 years;
# 2. Contains less than 3 zeros
###################################################################################################
mk_test <-function(v){
  y <- data.p[[v]]
  x <- data.p$year
  sen <- zyp.sen(y~x)
  slope <<- round(sen$coefficients[[2]],2)
  intercept <<- round(sen$coefficients[[1]],2)
  corr<<- cor.test(y, x, method = "kendall", alternative = "two.sided", exact = FALSE)
  Z <<-abs(corr$statistic[["z"]])
  years.for.trend <<- sum(!is.na(data.p[[v]]))
  CATTrend <<- case_when(Z>=1.28 ~ "Confident",
                        Z>=0.52 & Z<1.28 ~ "Likely",
                        Z<0.52 ~ "Uncertain")
  test <<- "MK"
}