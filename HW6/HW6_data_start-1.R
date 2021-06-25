# SVD on features then regression on those features for crime prediction

library(logging)

load_data = function(input_file, header_file){
  # Load data
  crime_data = read.table(input_file, sep=",", header=FALSE, na.strings = c("NA","?"))
  # Load headers
  crime_headers = read.table(header_file)
  names(crime_data) = crime_headers$V1
  
  #drop features that are missing a majority of observations:
  crime_data = crime_data[colSums(is.na(crime_data)) < 100]
  
  # Drop missing rows
  crime_data = crime_data[complete.cases(crime_data),]
  
  return(crime_data)
}

# Get AIC for specific number of principal components
get_aic_specific_pc <- function(pc_mat, orig_data, num_pc, index_dep_var){
  formula_rhs_temp = paste(paste0('pc_mat$x[,',1:num_pc,']'), collapse = ' + ')
  formula_temp = paste('orig_data[,',index_dep_var,'] ~',formula_rhs_temp)
  pc_all_components_temp = lm(eval(parse(text=formula_temp)))
  return(AIC(pc_all_components_temp))
}

# Unit test
test_aic_specific_pc <- function(){
  dummy <- data.frame(matrix(sample(1:100), nrow=5, ncol=5))
  cols <- c("y","x1","x2","x3","x4")
  names(dummy) <- cols

  dummy_mat <- model.matrix(y~., data = dummy)
  dummy_pc <- prcomp(dummy_mat)  

  dummy_aic <- get_aic_specific_pc(pc_mat = dummy_pc, orig_data = dummy_mat, num_pc = 3, index_dep_var = 1)
  stopifnot(is.numeric(dummy_aic) && dummy_aic != Inf)
}

if(interactive()){
  
  setwd('C:/users/zfishe1/Desktop/DS350/HW6/')
  
  # Setup logger
  logReset()
  file_log_handler = getLogger()
  setLevel("INFO", file_log_handler)
  basicConfig(level="INFO")
  addHandler(writeToFile, file = "test.log", level = "INFO", logger = file_log_handler)
  
  # load data
  crime_df = load_data('communities.data', header_file = 'crime_headers.txt')
  
  loginfo('info')
  logdebug('debug')
  logwarn('warn')
  
  # Run test
  test_aic_specific_pc()
  loginfo("Unit test successful!")
  
  # Disregard 'state' and 'communityname'.
  crime_df$state = NULL
  crime_df$communityname = NULL
  
  # Consider 'ViolentCrimesPerPop' as the y-dependent variable.
  summary(crime_df$ViolentCrimesPerPop)
  # Notice the y-values only vary between 0 and 1.  This is a logistic regression!
  
  # Regular logistic regression:
  regular_logistic = glm(ViolentCrimesPerPop ~ ., data = crime_df, family="binomial")
  
  # Predictions (above or below 0.5) don't make sense since we are predicted a probability (not binary event)
  # So we use AIC as our measure
  regular_logistic_AIC = regular_logistic$aic
  loginfo(paste("Regular logit AIC:", regular_logistic_AIC))
  
  # Histogram of fitted values
  hist(regular_logistic$fitted.values)
  hist(crime_df$ViolentCrimesPerPop)
  
  # Bonus!!!! Can you think of a way to check that the hypothesis that the two distributions:
  #  are similar?  If we were going to simulate the null, we would want to repeatedly generate
  #  the ViolentCrimesPerPop distribution.  We have 1993 observations of it.  Why not just look at
  #  repeated subsample of it?  Say we sample 500 observations randomly at a time (and repeat many times)...
  #  Then compare the null distribution to our sample statistic...
  #  (Not required for the homework... Just something to think about!)
  
  # Create SVD features and perform linear regression.
  
  # Step 1:  Create the data matrix:
  data_matrix = model.matrix(ViolentCrimesPerPop ~ .,data = crime_df)  # modify this equation
  
  # Step 2: Calculate the principle components:
  pc_data_matrix = prcomp(data_matrix)
  
  # Step 3: Look at magnitude of the variances explained (These are the eigenvalues!)
  plot(pc_data_matrix$sdev)
  
  # Step 4: Perform linear regression on components:
  pc_all_components = glm(crime_df$ViolentCrimesPerPop ~ pc_data_matrix$x[,1:10], family="binomial")
  summary(pc_all_components)
  
  # Get aic
  pc_AIC = AIC(pc_all_components) # Slightly better...
  loginfo(paste("AIC of all components:",pc_AIC))
  
  # What number of components minimizes the AIC?
  
  # Here, we use VERY similar code to the lines 181-192 from the "R_Examples_Lecture6.R"
  aic_by_num_pc = sapply(2:102, function(x){
    formula_rhs_temp = paste(paste0('pc_data_matrix$x[,',1:x,']'), collapse = ' + ')
    formula_temp = paste('crime_df$ViolentCrimesPerPop ~',formula_rhs_temp)
    pc_all_components_temp = lm(eval(parse(text=formula_temp)))
    return(AIC(pc_all_components_temp))
  })
  
  plot(aic_by_num_pc, type='l', lwd=2,
       main='AIC of P.C. Linear Reg with X components',
       xlab="# of components", ylab='AIC')
  
  min_pc <- which.min(aic_by_num_pc)
  col_num <- grep("ViolentCrimesPerPop", colnames(crime_df))
  aic_of_min <- get_aic_specific_pc(pc_mat = pc_data_matrix, orig_data = crime_df, num_pc = min_pc, index_dep_var = col_num)
  # Report your findings.
  
  loginfo(paste("Number of principal components:",min_pc,"AIC:",aic_of_min))
}




