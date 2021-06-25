##--------------------------------------------
##
## Example Monty Hall Homework Solution
##
## Class: PCE Data Science Methods Class
##
## Name: Zach Fisher
##
##--------------------------------------------

library(logging)


# Get the log file name that has a date-time in the name
get_log_filename = function(){
  log_file_name = format(Sys.time(), format="HW2_log_%Y_%m_%d_%H%M%S.log")
  return(log_file_name)
}


# Redefine the sample() function properly (Straight from the help file)
resample <- function(x, ...) x[sample.int(length(x), ...)]


# One game simulation function
one_game = function(switch_logical){
  
  # Perform one game logical steps in here.
  # Remember that if 'switch_logical' is TRUE,
  #   we must switch doors to the non-revealed door.
  doors <- 1:3
  # Select a prize door
  prize_door <- resample(doors)[1]
  
  # Select my door
  my_door <- resample(doors)[1]
  
  # Host picks from doors not picked
  host_door <- resample(doors[-c(my_door, prize_door)])[1]
  
  # The switch door is the not the one the host opened
  switch_door <- resample(doors[-c(my_door,host_door)])
  
  # If switch_logical is true, switch, otherwise don't
  if(switch_logical){
    my_door <- switch_door
  }
  
  # This function returns either a 0/1 or a FALSE/TRUE based on my_door==prize_door
  return(my_door == prize_door)
}


# Unit test
# Test if a simulation returns TRUE or FALSE
test_simulation_return_val = function(){
  one_game_outcome = one_game(switch_logical=TRUE)
  stopifnot(one_game_outcome %in% c(TRUE, FALSE))
}


if (interactive()){
  # Setup Logging
  log_file_name = get_log_filename()
  basicConfig()
  addHandler(writeToFile, file=log_file_name, level='INFO')
  
  # Setup working directory
  setwd('C:/users/zfishe1/desktop/DS350/HW2/')
  
  # Perform unit test
  loginfo("Testing simulation")
  test_simulation_return_val()
  loginfo("Test successful")
  
  # Set simulation parameters
  N_sims = 10000 # Number of games to simulate
  loginfo(paste("Running simulations:", N_sims))
  
  # Perform Stay Simulations
  # Here we come up with a vector of wins and losses
  #   by running the above function.
  # It should look like a vector of 1's and 0's (0, 0, 1, 1, ...)
  #   OR a vector of TRUE and FALSE values (FALSE, FALSE, ...)
  
  # For example:
  stay_results = sapply(1:N_sims, function(x) one_game(switch_logical=FALSE))
  
  # Perform Switch Simulations
  # Here we come up with a vector of wins and losses
  #   by running the above function.
  # It should look like a vector of 1's and 0's (0, 0, 1, 1, ...)
  #   OR a vector of TRUE and FALSE values (FALSE, FALSE, ...)
  switch_results <- sapply(1:N_sims, function(x) one_game(switch_logical=TRUE))
  
  # Compute Results
  # First we compute the average
  prob_win_switch = mean(switch_results)
  prob_win_stay = mean(stay_results)
  
  # Then we compute the variance of the results
  var_switch = var(switch_results)
  var_stay = var(stay_results)
  
  # Log Results
  # Here is an example of how to log a result
  loginfo(paste('Probability of winning when switching', prob_win_switch))
  loginfo(paste('Probability of winning when staying', prob_win_stay))
  loginfo(paste('Variance of switching results ', var_switch))
  loginfo(paste('Variance of staying results', var_stay))
}