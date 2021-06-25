setwd('C:/Users/zfishe1/Desktop/DS350/HW5/')

library(logging)
basicConfig()
addHandler(writeToFile, file="hw5.log", level='INFO')

fb_data = read.csv('facebook_edge_list.csv', stringsAsFactors = FALSE)

fb_degree_list = as.numeric(table(fb_data$Source))

degree_mean = mean(fb_degree_list)

loginfo(paste("Mean of degrees:",degree_mean))

# Create Histogram
hist(fb_degree_list)

# Create k-s statistic function
ks_stat = function(x_min,x_max, dist_a, dist_b){
  x_seq = seq(x_min,x_max,len=1000)
  y_cdf1 = sapply(x_seq, function(x){
    sum(dist_a<x)/length(dist_a)
  })
  y_cdf2 = sapply(x_seq, function(x){
    sum(dist_b<x)/length(dist_b)
  })
  k_s_stat = max(abs(y_cdf1-y_cdf2))
  return(k_s_stat)
}

min_p = 1
max_p = 107

degree_poisson = dpois(fb_degree_list, degree_mean)

hist(degree_poisson)

degree_ks = ks_stat(min_p, max_p, fb_degree_list, degree_poisson)

loginfo(paste("KS statistic for poisson:",degree_ks))

k_s_simulate = function(mean_lambda, length_vec){
  # Simulate two same poisson distributions with the same mean/length
  dist1 = rpois(length_vec, mean_lambda)
  dist2 = rpois(length_vec, mean_lambda)
  # Get k-s distance
  return(ks_stat(1, length_vec, dist1, dist2))
}

# Now that we can get 1 k-s stat under the null, let's get 10,000 of them.
k_s_distribution = sapply(1:1000, function(x){
  k_s_simulate(degree_mean, 107)
})

# Now we just have to sum up how many are equal to or bigger than 1.
# Note we can never get larger than 1).  Since none are larger than 1,
# we say our p-value is at most 1 / 10,000 = 0.0001.  So we reject the null.
p_val <- sum(k_s_distribution>degree_ks)/1000

loginfo(paste("P-value for poisson:", p_val))
loginfo("No KS statistics are larger than 1 so reject the null that the original sample could follow a poisson distribution")

# Load igraph
library(igraph)

# Fit to Power Law
pl_fit <- power.law.fit(fb_degree_list)

loginfo(paste("KS Stat for Power Law distribution:", pl_fit$KS.stat))

# P-value is .999 so we cannot reject the hypothesis that the original data follows a power-law distribution
loginfo(paste("P-value for Power Law KS Stat:", pl_fit$KS.p))
loginfo("P-value is .999 so we cannot reject the null hypothesis that the original data follows a power-law distribution")
