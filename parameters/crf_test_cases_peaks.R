########################################################################
# CRF Project 
# Purpose: To generate test cases for getAliasedPeaks, and get_hr_from_time_series
#          of the mhealthtools package
# Author: Meghasyam Tummalacherla
# email: meghasyam@sagebase.org
########################################################################
# Assuming that the working directory is ~/.../CRF_validation_analysis/

##############
# Required libraries 
##############
library(tidyverse)
library(mhealthtools)
# devtools::install_github('itismeghasyam/mhealthtools@crfAppVersion')
library(synapser)
library(githubr)
# https://github.com/brian-bot/githubr

synapser::synLogin()

##############
# Global parameters
##############
set.seed(12345678)
t <- seq(0,10,1/60)
sampling_rate <- 60
min_hr <- 45
max_hr <- 210

##############
# Required functions 
##############
getTestCaseJson <- function(x_signal,
                            sampling_rate = 60,
                            min_hr = 45,
                            max_hr = 210){
  
  # min and max lag in ACF based on sampling rate, min and max hr 
  max_lag = round(60 * sampling_rate / min_hr) # 4/3 fs is 45BPM
  min_lag = round(60 * sampling_rate / max_hr) # 1/3.5 fs is 210BPM
  
  # Hr and conf estimates from mhealthtools 
  x_op <- mhealthtools:::get_hr_from_time_series(x_signal, sampling_rate)
  
  # Testing the mhealthtools implementation of getAliasedPeaks,
  # and get_hr_from_time_series
  
  # Get ACF for hr estimation
  x <- stats::acf(x_signal, lag.max = max_lag, plot = F)$acf
  y <- 0 * x
  y[seq(min_lag, max_lag)] <- x[seq(min_lag, max_lag)]
  y_max_pos <- which.max(y)
  y_max <- max(y)
  y_min <- min(y)
  hr_initial_guess <- 60 * sampling_rate / (y_max_pos - 1)
  
  # Get Aliased peaks
  aliasedPeak <- mhealthtools:::getAliasingPeakLocation(hr = hr_initial_guess,
                                                        actual_lag = y_max_pos,
                                                        sampling_rate = sampling_rate,
                                                        min_lag = min_lag,
                                                        max_lag = max_lag)
  # Store data into a JSON
  io_example_signal_peak <- list(x = x_signal,
                                  y = y,
                                  y_min = y_min,
                                  y_max = y_max, 
                                  x_min = min(x),
                                  x_max = max(x),
                                  xacf = x[,1,1] %>% unlist(),
                                  hr_initial_guess = hr_initial_guess,
                                  est_hr = x_op[1],
                                  est_conf = x_op[2],
                                  aliased_peak = aliasedPeak,
                                  sampling_rate = sampling_rate,
                                  min_lag = min_lag,
                                  max_lag = max_lag)
  
  return(io_example_signal_peak)
}

##############
# The earlier peaks(for getAliasedPeaks) test case
##############
# Sample data to generate the earlier peaks(for getAliasedPeaks) test case
x_earlier <- 0.4*sin(5*t) + sin(10*t) + 0.2*cos(5*t)^2 + 0.2*rnorm(601)

# How does this sample look like
plot(x_earlier, type = 'l')
acf(x_earlier, lag.max = 80)

# Get the test parameters JSON file
io_example_earlier_peak <- getTestCaseJson(x_earlier)

# Write the JSON file
jsonlite::toJSON(io_example_earlier_peak, digits = 10) %>% 
  write_lines('io_example_earlier_peak.json')

# Check if the written JSON is same as the one in the environment
a1 <- jsonlite::fromJSON('io_example_earlier_peak.json')
all.equal(a1, io_example_earlier_peak) # Check to see no data loss during json conversion

##############
# The later peaks(for getAliasedPeaks) test case
##############
# Sample data to generate the later peaks(for getAliasedPeaks) test case
x_later <- 0.1*sin(10*t) + sin(20*t) + 0.2*cos(10*t)^2 + 0.1*rnorm(601)

# How does this sample look like
plot(x_later, type = 'l')
acf(x_later, lag.max = 80)$acf

# Get the test parameters JSON file
io_example_later_peak <- getTestCaseJson(x_later)

# Write the JSON file
jsonlite::toJSON(io_example_later_peak, digits = 10) %>% 
  write_lines('io_example_later_peak.json')

# Check if the written JSON is same as the one in the environment
a1 <- jsonlite::fromJSON('io_example_later_peak.json')
all.equal(a1, io_example_later_peak) # Check to see no data loss during json conversion

##############
# Upload data to Synapse
##############
# Github link
synapser::synLogin()
gtToken = 'github_token.txt';
githubr::setGithubToken(as.character(read.table(gtToken)$V1))
thisFileName <- 'parameters/crf_test_cases_peaks.R'
thisRepo <- getRepo(repository = "itismeghasyam/CRF_validation_analysis", ref="branch", refName='sagebio_master')
thisFile <- getPermlink(repository = thisRepo, repositoryPath=thisFileName)

# Write to Synapse
obj = File('io_example_earlier_peak.json',
           name = 'io_example_earlier_peak.json',
           parentId = 'syn18497861')
obj = synStore(obj, executed = thisFile)

obj = File('io_example_later_peak.json',
           name = 'io_example_later_peak.json',
           parentId = 'syn18497861')
obj = synStore(obj, executed = thisFile)
