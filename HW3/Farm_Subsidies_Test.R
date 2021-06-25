##--------------------------------------------
##
## Test Farm-Subsidies Data set
##
## Class: PCE Data Science Methods Class
##
## Purpose: Framework for Homework 3
##
## Datasets located:
##
##  http://www.fsa.usda.gov/FSA/webapp?area=newsroom&subject=landing&topic=foi-er-fri-pfi
##
##   Need:
##
##    -2011 Farm Payment File (27MB) txt file
##    -State and County Code List (374KB) xls file (probably convert this to csv)
##
##--------------------------------------------

##----Import Libraries-----
require(RSQLite)
require(logging)
library(dplyr)

##----Hypotheses to test-----
#
#  Test these two things:
#
#    1.  Does our sample equally represent all 50 states?
#    H0 = The sample equally represents all 50 states
#    Ha = The sample represents less than all 50 states
#   
#    2.  Does our sample equally represent all 50 states, weighted by number of farms/state?
#    H0 = The sample equally represents all 50 states weighted by num farms/state
#    Ha = The sample represents states disproportionately by not accounting for num farms/state
#
#     Note- you can find the farms per state in census data.
#

trim = function (x) gsub("^\\s+|\\s+$", "", x)

##-----Declare Functions Here-----


##----Run Main Function Here----
if(interactive()){
  
  ## set working dir
  setwd("c:/users/zfishe1/desktop/ds350/hw3")
  
  ##----Setup Test Logger-----
  basicConfig()
  addHandler(writeToFile, file=paste("./testing.log"), level='DEBUG')
  
  ##-----Read in the data-----
  data = read.csv("CAS.WDC11019.PMT11.FINAL.DT11186.TXT", sep=";",
                  header=FALSE, stringsAsFactors=FALSE)
  
  ##----Trim Whitespaces-----
  data = as.data.frame(apply(data,2,trim), stringsAsFactors=FALSE)
  
  names(data) = c('state_code', 'county_code', 'cust_num', 'program_code', 'program_year',
                  'commodity_code', 'amount', 'date', 'cat_code', 'farm_num',
                  'calendar_year', 'fiscal_year', 'seq_num')
  
  ##------Read State/County File-----
  county_state_codes = read.csv("foia_state_county_codes-1.csv", stringsAsFactors=FALSE)
  county_state_codes$state_code = county_state_codes$Stcd
  county_state_codes$Stcd = NULL
  county_state_codes$county_code = county_state_codes$Cntycd
  county_state_codes$Cntycd = NULL
  
  ##----Merge files together----
  data = merge(data, county_state_codes, by=c("state_code", "county_code"), all.x=TRUE)
  
  ##-----Probably do some data exploration----
  str(data)
  summary(data)
  
  data$farm_num <- as.numeric(data$farm_num)
  data$state_code <- as.numeric(data$state_code)
  
  cust_counts <- data %>% filter(cust_num != 0) %>% group_by(state_code) %>% summarise(num_customers = n_distinct(cust_num))
  loginfo(paste("Number of customer numbers:", sum(cust_counts$num_customers)))
  
  farm_counts <- data %>% filter(farm_num != 0) %>% group_by(state_code) %>% summarise(num_farms = n_distinct(farm_num))
  loginfo(paste("Number of farm numbers:", sum(farm_counts$num_farms)))
  
  sample_counts <- data %>% group_by(state_code) %>% count
  loginfo(paste("Number of data points:", sum(sample_counts$n)))
  
  ##----Perform a test for equal representation-----
  exp_percentage <- rep(0.02, 50)
  
  ## Log hypotheses
  loginfo("Hypothesis set 1")
  loginfo("H1_0 - The data equally represents all states. That is, 2% of the total sample comes from each state.")
  loginfo("H1_a - The data does not equally represent all states. That is, more than 2% of the sample is represented by at least one state.")
  
  x <- chisq.test(sample_counts$n, p = exp_percentage)
  loginfo(paste("X-squared:", x$statistic, "- Degrees of Freedom:", x$parameter, "- p-value < 2.2e-16"))
  loginfo("Reject H1_0 based on the p-value.")
  
  ##----Access the farms/state data-----
  farms_per_state <- read.csv("FarmsPerState.csv")
  
  ##----Derive the weights for each state----
  total_farms <- as.double(sum(farms_per_state$Farms))
  fps_exp_percentage <- farms_per_state$Farms/total_farms
  
  ## Log Hypotheses
  loginfo("Hypothesis set 2")
  loginfo("H2_0 - The data equally represents all states weighted by the number of farms per state.")
  loginfo("H2_a - The data does not equally represent all states weighted by the number of farms per state. That is, the percentage of total data in the sample for at least one state is greater than the number of farms in that state divided by the total number of farms in the country.")
  
  ##----Perform a test for equal repreentation by farms/state-----
  x <- chisq.test(sample_counts$n, p = fps_exp_percentage)
  loginfo(paste("X-squared:", x$statistic, "- Degrees of Freedom:", x$parameter, "- p-value < 2.2e-16"))
  x <- chisq.test(farm_counts$num_farms, p = fps_exp_percentage)
  loginfo(paste("X-squared:", x$statistic, "- Degrees of Freedom:", x$parameter, "- p-value < 2.2e-16"))  x <- chisq.test(cust_counts$num_customers, p = fps_exp_percentage)
  x <- chisq.test(cust_counts$num_customers, p = fps_exp_percentage)
  loginfo(paste("X-squared:", x$statistic, "- Degrees of Freedom:", x$parameter, "- p-value < 2.2e-16"))

  # Reject H_0
  loginfo("Reject H2_0 based on p-value.")
  
  ##----Output Results----
  # Acceptable to print output, log output, save in DB, or write to file. Your choice.
  
}




