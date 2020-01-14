## The following generate test cases for getAliasedPeaks, and get_hr_from_time_series
## of the mhealthtools package
rm(list = ls())
gc()

library(tidyverse)
library(synapser)
library(githubr)

# Global parameters
set.seed(12345678)
t <- seq(0,10,1/60)
sampling_rate <- 60
min_hr <- 45
max_hr <- 210
max_lag = round(60 * sampling_rate / min_hr) # 4/3 fs is 45BPM
min_lag = round(60 * sampling_rate / max_hr) # 1/3.5 fs is 210BPM

## Sample data to generate the earlier peaks test case
x_earlier <- 0.4*sin(5*t) + sin(10*t) + 0.2*cos(5*t)^2 + 0.2*rnorm(601)
x_op <- mhealthtools:::get_hr_from_time_series(x_earlier, 60)

# How does this sample look like
plot(x_earlier, type = 'l')
acf(x_earlier, lag.max = 80)
x <- stats::acf(x_earlier, lag.max = max_lag, plot = F)$acf
y <- 0 * x
y[seq(min_lag, max_lag)] <- x[seq(min_lag, max_lag)]
y_max_pos <- which.max(y)
y_max <- max(y)
y_min <- min(y)
hr_initial_guess <- 60 * sampling_rate / (y_max_pos - 1)
aliasedPeak <- mhealthtools:::getAliasingPeakLocation(hr = hr_initial_guess,
                                                      actual_lag = y_max_pos,
                                                      sampling_rate = sampling_rate,
                                                      min_lag = min_lag,
                                                      max_lag = max_lag)
# Store data into a JSON
io_example_earlier_peak <- list(x = x_earlier,
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
jsonlite::toJSON(io_example_earlier_peak, digits = 10) %>% 
  write_lines('io_example_earlier_peak.json')
a1 <- jsonlite::fromJSON('io_example_earlier_peak.json')
all.equal(a1, io_example_earlier_peak) # Check to see no data loss during json conversion


## Sample data to generate the later peaks test case
x_later <- 0.1*sin(10*t) + sin(20*t) + 0.2*cos(10*t)^2 + 0.1*rnorm(601)
x_op <- mhealthtools:::get_hr_from_time_series(x_later, 60)

# How does this sample look like
plot(x_later, type = 'l')
acf(x_later, lag.max = 80)$acf
x <- stats::acf(x_later, lag.max = max_lag, plot = F)$acf
y <- 0 * x
y[seq(min_lag, max_lag)] <- x[seq(min_lag, max_lag)]
y_max_pos <- which.max(y)
y_max <- max(y)
y_min <- min(y)

hr_initial_guess <- 60 * sampling_rate / (y_max_pos - 1)
aliasedPeak <- mhealthtools:::getAliasingPeakLocation(hr = hr_initial_guess,
                                                      actual_lag = y_max_pos,
                                                      sampling_rate = sampling_rate,
                                                      min_lag = min_lag,
                                                      max_lag = max_lag)

# Store data into a JSON
io_example_later_peak <- list(x = x_later,
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
jsonlite::toJSON(io_example_later_peak, digits = 10) %>% 
  write_lines('io_example_later_peak.json')
a1 <- jsonlite::fromJSON('io_example_later_peak.json')
all.equal(a1, io_example_later_peak) # Check to see no data loss during json conversion

## Upload data to Synapse
# Github link
synapser::synLogin()
gtToken = 'github_token.txt';
githubr::setGithubToken(as.character(read.table(gtToken)$V1))
thisFileName <- 'crf_parameters_peaks.R'
thisRepo <- getRepo(repository = "Sage-Bionetworks/CRF_validation_analysis", ref="branch", refName='master')
thisFile <- getPermlink(repository = thisRepo, repositoryPath=thisFileName)

# Input - output files for the peaks examples
obj = File('io_example_earlier_peak.json',
           name = 'io_example_earlier_peak.json',
           parentId = 'syn18497861')
obj = synStore(obj, executed = thisFile)

obj = File('io_example_later_peak.json',
           name = 'io_example_later_peak.json',
           parentId = 'syn18497861')
obj = synStore(obj, executed = thisFile)
