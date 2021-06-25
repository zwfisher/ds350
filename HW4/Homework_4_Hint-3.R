##--------------------------------------------
##
## Chicago Diabetes Homework (Lecture 4)
##
## Class: PCE Data Science Methods Class
##
## Zach Fisher
##--------------------------------------------

library(logging)

# Get the log file name that has a date-time in the name
get_log_filename = function(){
  log_file_name = format(Sys.time(), format="HW4_log_%Y_%m_%d_%H%M%S.log")
  return(log_file_name)
}

# Function to use on admission rates
sum_rates <- function(x, num_rows){
  return (x/num_rows)
}

# Unit test
test_sum_rates = function(){
  rand <- sample(1200:1350,1)
  outcome <- sum_rates(rand, 47)
  stopifnot((outcome>0)&&(outcome<100))
}

if(interactive()){
  # Setup Logging
  log_file_name = get_log_filename()
  basicConfig()
  addHandler(writeToFile, file=log_file_name, level='INFO')
  
  # Set working dir
  setwd('/home/zfisher/datasci/ds350/hw4/')
  
  # Perform unit test
  loginfo("Testing function")
  test_sum_rates()
  loginfo("Test successful")
  
  # Read in data
  loginfo("Reading data")
  data = read.csv('ChicagoDiabetesData.csv', stringsAsFactors = FALSE)
  loginfo("Read successful")

  loginfo("Getting sums")
    
  # Get sum of data
  data_sum = apply(data[-1],2,sum)

  # Get means of each years hospitalizations and admissions
  hospitalizations = data_sum[grepl('Hospitalizations', names(data[-1]))]
  admit_rate = data_sum[grepl('Crude.Rate.[0-9]+$', names(data[-1]), perl = TRUE)]
  
  # Divide by total num of rows
  admit_rate <- sum_rates(admit_rate, num_rows=nrow(data))
  
  loginfo("Plotting sums")
  
  plot(hospitalizations, admit_rate, pch=16)
  
  # Fit model
  fit1 <- lm(admit_rate~hospitalizations)
  
  # Plot line of best fit
  abline(fit1, lty=2)
  
  summary(fit1)
  
  loginfo("R-squared: .3395")
  
  loginfo("The equation of the line is a = 6.825616 + 0.002681h. 
  If the number of hospitalizations is zero, the admission rate is 6.8% .
  If the number of hospitalizations increases by one, we are predicting that the admission rate will increase by 0.002%.")
  
  # The equation of the line is a = 6.825616 + 0.002681h. 
  # If the number of hospitalizations is zero, the admission rate is 6.8% .
  # If the number of hospitalizations increases by one, we are predicting that the admission rate will increase by 0.002%.
  
  loginfo("Getting differences")
  
  # Get differences between years 
  hospitalizations_diff = diff(hospitalizations)
  admit_rate_diff = diff(admit_rate)
  
  plot(hospitalizations_diff, admit_rate_diff, pch=16)
  
  loginfo("Plotting lm of differences")
  
  # Fit model 2
  fit2 <- lm(admit_rate_diff ~ hospitalizations_diff)
  
  # Plot line of best fit
  abline(fit2, lty=2)
  
  summary(fit2)

  loginfo("R-squared: .9516")
  
  loginfo("The equation of the line is da = 0.2143 + 0.0036dh. 
  # If the difference in hospitalizations is zero, the difference in admission rate will be 0.21% year to year.
  # If the difference in hospitalizations increases by one, we are predicting that the number of admissions will increase by 0.0036%.
  # This rate of change in admission rates is higher which makes sense because we're looking at velocity of number of hospitalization increases.")
  
  # The equation of the line is da = 0.2143 + 0.0036dh. 
  # If the difference in hospitalizations is zero, the difference in admission rate will be 0.21% year to year.
  # If the difference in hospitalizations increases by one, we are predicting that the number of admissions will increase by 0.0036%.
  # This rate of change in admission rates is higher which makes sense because we're looking at velocity of number of hospitalization increases.
  
}
