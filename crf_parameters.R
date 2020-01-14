########################################################################
# CRF Project 
# Purpose: To encode parameter values for the CRF algo, so that it can 
#          be ported into another environment, for eg., iOS, android etc.,
# Author: Meghasyam Tummalacherla
# email: meghasyam@sagebase.org
########################################################################
rm(list=ls())
gc()
options(digits.secs = 10)
##############
# Required libraries 
##############
library(tidyverse)
library(synapseClient)
library(githubr)
synapseLogin()

##############
# Required functions 
##############
getFilterCoeff <- function(sampling_rate){
  sampling_rate_rounded <- round(sampling_rate)
  # Filter the signal based on fiters designed
  if(sampling_rate_rounded > 15){
    bf_low <- signal::butter(7, 5/(sampling_rate_rounded/2), type = 'low')
    bf_high <- signal::butter(7, 0.5/(sampling_rate_rounded/2), type = 'high')
  }else{
    bf_low <- signal::butter(7, 4/(sampling_rate_rounded/2), type = 'low')
    bf_high <- signal::butter(7, 0.5/(sampling_rate_rounded/2), type = 'high')
  }
  
  return(list(low = unlist(bf_low),
              high = unlist(bf_high)))
}

getMeanFilterOrder <- function(sampling_rate){
  mean_filter_order <- 65
  if(sampling_rate <= 32){
    mean_filter_order <- 33
  }
  if(sampling_rate <= 18){
    mean_filter_order <- 19
  }
  if(sampling_rate <= 15){
    mean_filter_order <- 15
  }
  return(data.frame(mean_filter_order = mean_filter_order,
                    sampling_rate = sampling_rate))
}

meanCenteringFilter <- function(x, mean_filter_order = 65){
  y <- 0 * x # Initializing y
  sequence_limits <- seq((mean_filter_order + 1) / 2,
                         length(x) - (mean_filter_order - 1) / 2, 1)
  for (i in sequence_limits) {
    temp_sequence <- x[seq(i - (mean_filter_order - 1) / 2,
                           (i + (mean_filter_order - 1) / 2),1)]
    
    y[i] <- (((x[i]) -
                (sum(temp_sequence) - (max(temp_sequence)) + min(temp_sequence)) / (mean_filter_order - 2)) /
               (max(temp_sequence) - min(temp_sequence) + 0.00001)) 
    # 0.00001 is a small value, ideally this should be machine epsilon
  }
  y <- y[sequence_limits] 
  # Subset y to only the sequence limits, i.e we only need to caluculate y for the values in sequence_limits
  return(y)
}

getACF <- function(x, max_lag){
  x[is.na(x)] <- 0
  return(stats::acf(x, lag.max = max_lag, plot = F)$acf)
}
##############
# Calculate the filter parameters for various integer values of sampling rates
##############

sampling_rates <- seq(10,65) # the various range of expected sampling rates (ROUNDED)

# Get Lowpass and Highpass filter coefficients for various sampling rates
filter_parameters <- lapply(sampling_rates, function(fs){
  filter_coeff <- getFilterCoeff(fs) %>%
    as.data.frame() %>% t() %>% as.data.frame() %>%
    dplyr::mutate(filter_type = rownames(.),
                  sampling_rate = fs)
  return(filter_coeff)
}) %>% data.table::rbindlist()

lowpass_filter_params <- filter_parameters %>% 
  dplyr::filter(filter_type == 'low')

highpass_filter_params <- filter_parameters %>% 
  dplyr::filter(filter_type == 'high')

# Get Mean filter orders
mean_centering_filter_params <- lapply(sampling_rates, getMeanFilterOrder) %>% 
  data.table::rbindlist()

# Get sample input-output example for lowpass/highpass filters
x <- mhealthtools::heartrate_data$red[1100:1400]

sampling_rate <- mhealthtools:::get_sampling_rate(
  mhealthtools::heartrate_data %>%
    dplyr::filter(t > 19.53))

# sampling_rate <- 1/median(diff(na.omit(mhealthtools::heartrate_data$t[1100:1400])))
# Use median based method to estimate sampling rate

sampling_rate_round <- sampling_rate %>% # This is to get remove the first 1099 samples 
  round() # this is 59
bf_low <- signal::butter(7, 5/(sampling_rate_round/2), type = 'low')
bf_high <- signal::butter(7, 0.5/(sampling_rate_round/2), type = 'high')
xf_low <- signal::filter(bf_low,x) %>% as.numeric()
xf_high <- signal::filter(bf_high,x) %>% as.numeric()

# Get sample input-output example for mean-centering filter
xf_mcf <- meanCenteringFilter(x, mean_filter_order = 65)

# Get max and min lags for the ACF based on sampling rate, min and max hr
min_hr = 45
max_hr = 210
max_lag = round(60 * sampling_rate / min_hr) # 4/3 fs is 45BPM
min_lag = round(60 * sampling_rate / max_hr) # 1/3.5 fs is 210BPM

# Get sample input-output example for ACF
x_acf <- stats::acf(x, lag.max = 80, plot = F)$acf[,1,1]

io_example_list <- list(input = x ,
                  lowpass = xf_low,
                  highpass = xf_high,
                  mcfilter = xf_mcf,
                  acf = x_acf,
                  b_lowpass = bf_low$b,
                  a_lowpass = bf_low$a,
                  b_highpass = bf_high$b,
                  a_highpass = bf_high$a,
                  mean_filter_order = 65,
                  sampling_rate_round = 60,
                  sampling_rate = sampling_rate,
                  max_hr = max_hr,
                  min_hr = min_hr,
                  max_lag = max_lag,
                  min_lag = min_lag
)

# Store the input output examples as a JSON
jsonlite::toJSON(io_example_list, digits = 10) %>% 
  write_lines('io_examples.json')
a1 <- jsonlite::fromJSON('io_examples.json')
all.equal(a1, io_example_list) # Check to see no data loss during json conversion

##############
# Calculate the outputs at each stage of the algo:
# input -> filtered signal -> chunk it into 10s windows -> HR estimates per window
##############

window_length = 15 # 15s
window_overlap = 14/15 # 14s overlap for 15s windows => 1s updates
method = 'acf' 

hr.data <- mhealthtools::heartrate_data
nrow_data <- nrow(hr.data)
hr.data <- hr.data[2:nrow_data,]

sampling_rate <- mhealthtools:::get_sampling_rate(hr.data)
# sampling_rate <- 1/median(diff(na.omit(hr.data$t)))
# Use median based sampling_rate calculation

# Convert window length from seconds to samples
window_length <- round(sampling_rate * window_length)
mean_filter_order <- 65
if(sampling_rate <= 32){
  mean_filter_order <- 33
}
if(sampling_rate <= 18){
  mean_filter_order <- 19
}
if(sampling_rate <= 15){
  mean_filter_order <- 15
}

hr.data.chunks <- hr.data %>%
  dplyr::select(red, green, blue) %>%
  na.omit() %>%
  lapply(mhealthtools:::window_signal, window_length, window_overlap, 'rectangle')


hr.data.filtered.chunks <- hr.data.chunks %>%
  lapply(function(dfl){
    dfl[is.na(dfl)] <- 0
    dfl = tryCatch({
      apply(dfl,2,
            mhealthtools:::get_filtered_signal,
            sampling_rate,
            mean_filter_order,
            method) %>% 
        as.data.frame()
    }, error = function(e){NA})
  })


hr.data.filtered.chunks.acf <- hr.data.filtered.chunks %>% 
  lapply(function(dfl) {
    dfl <- tryCatch({
      apply(dfl, 2, getACF, max_lag)
    }, error = function(e) {c(hr= NA, confidence = NA) })
    return(dfl)
  })

hr.estimates <- hr.data.filtered.chunks %>%
  lapply(function(dfl) {
    dfl <- tryCatch({
      apply(dfl, 2, mhealthtools:::get_hr_from_time_series, sampling_rate, method)
    }, error = function(e) {c(hr= NA, confidence = NA) })
    dfl <- as.data.frame(t(dfl))
    colnames(dfl) <- c("hr", "confidence")
    return(dfl)
  })


io_examples_whole <- list(hr_data = hr.data,
                 hr_data_chunks = hr.data.chunks,
                 hr_data_filtered_chunked = hr.data.filtered.chunks,
                 hr_data_filtered_chunked_acf = hr.data.filtered.chunks.acf,
                 hr_estimates = hr.estimates)

# Store the input output examples as a JSON
jsonlite::toJSON(io_examples_whole, digits = 10) %>% 
  write_lines('io_examples_whole.json')
a1 <- jsonlite::fromJSON('io_examples_whole.json')
all.equal(a1, io_examples_whole) # Check to see no data loss during json conversion



#######################################
# Upload Data to Synapse 
#######################################
# Github link
gtToken = 'github_token.txt';
githubr::setGithubToken(as.character(read.table(gtToken)$V1))
thisFileName <- 'crf_parameters.R'
thisRepo <- getRepo(repository = "Sage-Bionetworks/CRF_validation_analysis", ref="branch", refName='master')
thisFile <- getPermlink(repository = thisRepo, repositoryPath=thisFileName)

# Write params data to Synapse
# lowpass params
write.csv(lowpass_filter_params,file = paste0('lowpass_filter_params','.csv'),na="")
obj = File(paste0('lowpass_filter_params','.csv'), 
           name = paste0('lowpass_filter_params','.csv'), 
           parentId = 'syn18497861')
obj = synStore(obj, executed = thisFile)

# highpass params
write.csv(highpass_filter_params,file = paste0('highpass_filter_params','.csv'),na="")
obj = File(paste0('highpass_filter_params','.csv'), 
           name = paste0('highpass_filter_params','.csv'), 
           parentId = 'syn18497861')
obj = synStore(obj, executed = thisFile)

# mean centering filter params
write.csv(mean_centering_filter_params,file = paste0('mean_centering_filter_params','.csv'),na="")
obj = File(paste0('mean_centering_filter_params','.csv'), 
           name = paste0('mean_centering_filter_params','.csv'), 
           parentId = 'syn18497861')
obj = synStore(obj, executed = thisFile)

# Input - output example JSON
obj = File('io_examples.json',
           name = 'io_examples.json',
           parentId = 'syn18497861')
obj = synStore(obj, executed = thisFile)

# Input - output file for the whole algo
obj = File('io_examples_whole.json',
           name = 'io_examples_whole.json',
           parentId = 'syn18497861')
obj = synStore(obj, executed = thisFile)

