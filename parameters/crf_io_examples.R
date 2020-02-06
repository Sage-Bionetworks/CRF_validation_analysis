########################################################################
# CRF Project 
# Purpose: To generate input-output(IO) examples to verify the inplementation
#          of the filtering part of the CRF algo on phone app vs R
# Author: Meghasyam Tummalacherla
# email: meghasyam@sagebase.org
########################################################################
# Assuming that the working directory is ~/.../CRF_validation_analysis/
rm(list=ls())
gc()
options(digits.secs = 10)

##############
# Required libraries 
##############
library(tidyverse)
library(synapser)
library(githubr)
library(mhealthtools)
# devtools::install_github('itismeghasyam/mhealthtools@crfAppVersion')

synapser::synLogin()

##############
# Global parameters
##############
min_hr <- 45
max_hr <- 210

##############
# Required functions 
##############
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

getIOExamplesHRData <- function(hr.data,
                                window_length = 15,
                                window_overlap = 14/15,
                                method = 'acf',
                                min_hr = 45,
                                max_hr = 210){
  
  # Get sampling rate
  sampling_rate <- mhealthtools:::get_sampling_rate(hr.data)
  
  # Convert window length from seconds to samples
  window_length <- round(sampling_rate * window_length)
  
  # Get mean filter order
  mean_filter_order <- getMeanFilterOrder(sampling_rate)$mean_filter_order
  
  # Get max and min lags for the ACF based on sampling rate, min and max hr
  max_lag = round(60 * sampling_rate / min_hr) # 4/3 fs is 45BPM
  min_lag = round(60 * sampling_rate / max_hr) # 1/3.5 fs is 210BPM
  
  # Chunk data into blocks, using window_length and window_overlap
  hr.data.chunks <- hr.data %>%
    dplyr::select(red, green, blue) %>%
    na.omit() %>%
    lapply(mhealthtools:::window_signal, window_length, window_overlap, 'rectangle')
  
  # Filter chunked data
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
  
  # Get ACF of the filtered chunks
  hr.data.filtered.chunks.acf <- hr.data.filtered.chunks %>% 
    lapply(function(dfl) {
      dfl <- tryCatch({
        apply(dfl, 2, getACF, max_lag)
      }, error = function(e) {c(hr= NA, confidence = NA) })
      return(dfl)
    })
  
  # Get HR estimates from the filtered chunks using mhealthtools
  hr.estimates <- hr.data.filtered.chunks %>%
    lapply(function(dfl) {
      dfl <- tryCatch({
        apply(dfl, 2, mhealthtools:::get_hr_from_time_series, sampling_rate, method)
      }, error = function(e) {c(hr= NA, confidence = NA) })
      dfl <- as.data.frame(t(dfl))
      colnames(dfl) <- c("hr", "confidence")
      return(dfl)
    })
  
  # output the IO examples as a list
  io_examples_whole <- list(hr_data = hr.data,
                            hr_data_chunks = hr.data.chunks,
                            hr_data_filtered_chunked = hr.data.filtered.chunks,
                            hr_data_filtered_chunked_acf = hr.data.filtered.chunks.acf,
                            hr_estimates = hr.estimates)
  return(io_examples_whole)
}

##############
# IO example for lowpass and highpass filters on a time-series vector 
##############
# Get sample input-output example for lowpass/highpass filters
x <- mhealthtools::heartrate_data$red[1100:1400]

# Get sampling rate
sampling_rate <- mhealthtools:::get_sampling_rate(
  mhealthtools::heartrate_data %>%
    dplyr::filter(t > 19.53))

sampling_rate_round <- sampling_rate %>% # This is to get remove the first 1099 samples 
  round() 

bf_low <- signal::butter(7, 5/(sampling_rate_round/2), type = 'low')
bf_high <- signal::butter(7, 0.5/(sampling_rate_round/2), type = 'high')
xf_low <- signal::filter(bf_low,x) %>% as.numeric()
xf_high <- signal::filter(bf_high,x) %>% as.numeric()

# Get sample input-output example for mean-centering filter
mean_filter_order <- getMeanFilterOrder(sampling_rate_round)$mean_filter_order
xf_mcf <- meanCenteringFilter(x, mean_filter_order = mean_filter_order)

# Get max and min lags for the ACF based on sampling rate, min and max hr
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
                        mean_filter_order = mean_filter_order,
                        sampling_rate_round = sampling_rate_round,
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
# IO example of a full HR data JSON file
# Calculate the outputs at each stage of the algo:
# input -> filtered signal -> chunk it into 15s windows -> HR estimates per window
##############
## 60Hz Data
# Get sample data (60Hz sampling rate) from mhealthtools
hr.data <- mhealthtools::heartrate_data
nrow_data <- nrow(hr.data)
hr.data <- hr.data[2:nrow_data,]

# Get IO example list
io_examples_whole <- getIOExamplesHRData(hr.data = hr.data)

# Store the input output examples as a JSON
jsonlite::toJSON(io_examples_whole, digits = 10) %>% 
  write_lines('io_examples_whole.json')
a1 <- jsonlite::fromJSON('io_examples_whole.json')
all.equal(a1, io_examples_whole) # Check to see no data loss during json conversion

## < 15Hz data (download data from Synapse for real world data)
phone.tableId = 'syn17007713'
name = 'HeartRate Measurement-v8'

hr.tbl.syn <- synapser::synTableQuery(paste("select * from ", phone.tableId))
hr.tbl <- hr.tbl.syn$asDataFrame() %>%
  dplyr::filter(answers.participantID > 1200) # our participantsIDs are 12xx, 34xx and 56xx
columnsToDownload = c('rawData')

hr.json.loc = lapply(columnsToDownload, function(col.name){
  tbl.files = synapser::synDownloadTableColumns(hr.tbl.syn, col.name) %>%
    lapply(function(x) data.frame(V1 = x)) %>% 
    data.table::rbindlist(idcol = col.name) %>% 
    plyr::rename(c('V1' = gsub('Data','.fileLocation', col.name)))
})

hr.tbl$rawData <- as.character(hr.tbl$rawData)
hr.table.meta = data.table::rbindlist(list(hr.tbl %>%
                                             left_join(do.call(cbind, hr.json.loc[1]))),
                                      use.names = T, fill = T) %>% 
  as.data.frame 

# Moto G phones have a low sampling rate
hr.table.meta.low.hz <- hr.table.meta %>% 
  dplyr::filter(phoneInfo == 'Motorola moto g(6) play') 

# Get a file from the moto g6 data for IO examples
hr.json.fileLocation <- "./cameraHeartRate_recorder.json" # android
# hr.json.fileLocation <- "./cameraHeartRate_rgb.json" # iphone
rawFiles <- unzip(hr.table.meta.low.hz$raw.fileLocation[2] %>% as.character())
hr.data <- jsonlite::fromJSON(hr.json.fileLocation) %>% 
  dplyr::select(-bpm_camera)
nrow_data <- nrow(hr.data)
hr.data <- hr.data[2:nrow_data,]
hr.data$t <- hr.data$timestamp - min(hr.data$timestamp, na.rm = T)
hr.data <- hr.data %>% 
  dplyr::filter(timestamp>0) %>% 
  dplyr::select(-timestampDate)
mhealthtools:::get_sampling_rate(hr.data)

# Get IO example list
io_examples_whole_12hz <- getIOExamplesHRData(hr.data = hr.data)

# Store the input output examples as a JSON
jsonlite::toJSON(io_examples_whole_12hz, digits = 10) %>% 
  write_lines('io_examples_whole_12hz.json')
a1 <- jsonlite::fromJSON('io_examples_whole_12hz.json')
all.equal(a1, io_examples_whole_12) # Check to see no data loss during json conversion

#######################################
# Upload Data to Synapse 
#######################################
# Github link
gtToken = 'github_token.txt';
githubr::setGithubToken(as.character(read.table(gtToken)$V1))
thisFileName <- 'parameters/crf_io_examples.R'
thisRepo <- getRepo(repository = "itismeghasyam/CRF_validation_analysis", ref="branch", refName='sagebio_master')
thisFile <- getPermlink(repository = thisRepo, repositoryPath=thisFileName)

# Input - output example JSON
obj = File('io_examples.json',
           name = 'io_examples.json',
           parentId = 'syn18497861')
obj = synStore(obj, executed = thisFile)

# Input - output file for the whole algo (60Hz)
obj = File('io_examples_whole.json',
           name = 'io_examples_whole.json',
           parentId = 'syn18497861')
obj = synStore(obj, executed = thisFile)

# Input - output file for the whole algo (<15Hz)
obj = File('io_examples_whole_12hz.json',
           name = 'io_examples_whole_12hz.json',
           parentId = 'syn18497861')
obj = synStore(obj, executed = thisFile)
