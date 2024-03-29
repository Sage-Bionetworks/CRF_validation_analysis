########################################################################
# CRF Project 
# Purpose: To check if the ACF from R-stats package is the same
#          as the ACF used in the phone app implementation
# Author: Meghasyam Tummalacherla
# email: meghasyam@sagebase.org
########################################################################

##############
# Required libraries 
##############
library(tidyverse)
library(mhealthtools)
# devtools::install_github('itismeghasyam/mhealthtools@crfAppVersion')

##############
# Data 
############## 
hr.data <- mhealthtools::heartrate_data
sampling_rate_round <- mhealthtools:::get_sampling_rate(hr.data) %>% 
  round()
x <- hr.data$red[1500:2000]
plot(x, type = 'l')

##############
# ACF using R-stats package 
##############
par(mfrow = c(2,1))
xacf_stats <- stats::acf(x, lag.max = 80, plot = T)$acf[,1,1] 

##############
# App implementation of autocorrelation function
##############
# REF: https://www.mathworks.com/matlabcentral/fileexchange/30540-autocorrelation-function-acf
# NOTE: Normalized ACF is Unscaled Co-variance divided by Unscaled variance

our_acf <- function(x, lag.max){
  
  x_acf <- rep(0, lag.max+1) # the +1 is because, we will have a 0 lag value also
  xl <- length(x) # total no of samples
  mx <- mean(x) # average/mean
  varx <- sum((x-mx)^2) # Unscaled variance
  
  for(i in seq(0, lag.max)){ # for i=0:lag.max
    x_acf[i+1] <- sum( (x[1:(xl-i)]-mx) * (x[(i+1):xl]-mx))/varx # (Unscaled Co-variance)/(Unscaled variance)
  }
  return(x_acf)
}

# our output
xacf_our <- our_acf(x, lag.max = 80)
plot(xacf_our, type = 'l')

##############
# Check if the app implementation is in-line with the stats::acf implementation
##############
all.equal(xacf_stats, xacf_our)
