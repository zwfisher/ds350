# Homework 8 start
#
#  Objective:  Use bootstrapping to estimate the error. (NOT on the residuals)
#  Dependent variable: Predict whether or not indivual will buy ('Buy')
#
# Don't forget to add logging, a unit test, and functions for everything
# You will return a 95% confidence interval for every coefficient, including the intercept
#   (But not the residual error)
#-------------------------------------------------------

library(logging)

# Confidence Interval function
confints <- function(x){
  n_pred <- length(x$t0)
  ci_mat <- matrix(nrow = n_pred, ncol = 2)
  
  for(i in 1:n_pred){
    ci <- boot.ci(x, type="norm", index=i)
    ci_mat[i,] <- ci$normal[2:3]
  }
  
  return(ci_mat)
}


# Unit test
test_confints <- function(){

  ad_data = read.csv('AdvertisingPrediction.csv')
  ad_data$Obs.No. = NULL
  
  test_boot = function(data, indices){
    data_new = data[indices, ]
    fit_new = glm(Buy~ ., family=binomial, data = data_new)
    return(coef(fit_new))
  }
  
  test_boot_estimates = boot(data=ad_data, statistic=test_boot, R=10)
  
  test_ci <- confints(test_boot_estimates)
  
  stopifnot(!any(is.na(test_ci)))
}

if(interactive()){
  
  setwd('C:/users/zfishe1/Desktop/DS350/HW8/')
  
  # Setup logger
  logReset()
  file_log_handler = getLogger()
  setLevel("INFO", file_log_handler)
  basicConfig(level="INFO")
  addHandler(writeToFile, file = "test.log", level = "INFO", logger = file_log_handler)
  
  # Test func
  test_confints()
  loginfo("Unit test successful!")
  
  ad_data = read.csv('AdvertisingPrediction.csv')
  ad_data$Obs.No. = NULL
  
  # Bootstrapping the residuals.  First we have to get the residuals from
  #  the logistic regression
  
  logistic_fit = glm(Buy ~ ., family=binomial, data = ad_data)
  
  summary(logistic_fit)$coefficients[,2]
  
  logistic_boot = function(data, indices){
    data_new = data[indices, ]
    fit_new = glm(Buy ~ ., family=binomial, data = data_new)
    return(coef(fit_new))
  }
  
  N = 100 # What to increase this to?
  loginfo(paste("N:",N))
  boot_estimates = boot(data=ad_data, statistic=logistic_boot, R=N)
  
  # Calculate Confidence intervals for everything with 'boot.ci()'
  ci <- confints(boot_estimates)
  loginfo(paste("Lower:",ci[,1],"Upper:",ci[,2]))
}
