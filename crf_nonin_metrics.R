########################################################################
# CRF Project 
# Purpose: To extract metrics per window on the phone PPG (Done at UCSD)
# Author: Meghasyam Tummalacherla
# email: meghasyam@sagebase.org
########################################################################
rm(list=ls())
gc()
# devtools::install_github("Sage-Bionetworks/mhealthtools")
devtools::install_github("itismeghasyam/mhealthtools@develop")
options(digits.secs = 6)
# to get the millisecond resolution

##############
# Required libraries 
##############
library(data.table)
library(tidyr)
library(plyr)
library(dplyr)
library(seewave)
library(mhealthtools) 
library(synapser)
library(githubr)
library(ggplot2)
library(parsedate)
library(lubridate)
synLogin()

##############
# Required functions
##############
get_hrdata_window_metrics <- function(hr_data,
                                      window_length = 10,
                                      window_overlap = 0.5,
                                      method = 'acf') {
  ## We will throw away ~5s worth of data(180 samples) after filtering,
  ## keep this in mind
  
  heartrate_error_frame <- data.frame(red = NA, green = NA, blue = NA,
                                      error = NA, sampling_rate = NA)
  
  sampling_rate <- mhealthtools:::get_sampling_rate(hr_data)
  
  diff_t_vec  <- tryCatch({
    diff(na.omit(hr_data$t))
    },
         error = function(e){NA})  
  if(is.null(diff_t_vec)){
    diff_t_vec <- NA
  }
  
  md_diff_t <- median(diff_t_vec)
  sd_diff_t <- sd(diff_t_vec)
  max_diff_t <- max(diff_t_vec)
  min_diff_t <- min(diff_t_vec)
  
  sampling_rate_md <- 1/md_diff_t
  sampling_rate_md_plus <- 1/(md_diff_t - sd_diff_t)
  sampling_rate_md_minus <- 1/(md_diff_t + sd_diff_t)
  sampling_rate_max <- 1/min_diff_t
  sampling_rate_min <- 1/max_diff_t
  
  if (is.infinite(sampling_rate) || is.na(sampling_rate)) {
    heartrate_error_frame$error <- paste("Sampling Rate calculated from timestamp is Inf",
                                         "or NaN / timestamp not found in json")
    return(heartrate_error_frame)
  }
  
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
  
  # Split each color into segments based on window_length
  hr_data_windowed <- tryCatch({
    hr_data %>%
      dplyr::select(red, green, blue) %>%
      na.omit() %>%
      lapply(mhealthtools:::window_signal, window_length, window_overlap, 'rectangle')
  }, error = function(e) { NA })
  if (all(is.na(hr_data))) {
    heartrate_error_frame$error <- "red, green, blue cannot be read from JSON"
    return(heartrate_error_frame)
  }
  
  # Get ACF metrics for each filtered segment of each color
  hr_data_time <- hr_data_windowed %>%
    lapply(function(dfl) {
      dfl <- tryCatch({
        apply(dfl, 2, getWindowMetricsTime, sampling_rate) %>% 
          data.table::rbindlist()
      }, error = function(e) {NA })
      # dfl <- as.data.frame(t(dfl))
      # colnames(dfl) <- c("hr", "confidence")
      dfl$window <- seq(nrow(dfl))
      return(dfl)
    })
  hr_data_time$green$channel <- 'green'
  hr_data_time$blue$channel <- 'blue'
  hr_data_time <- hr_data_time %>%
    data.table::rbindlist(fill = T)
  # hr_data$error <- "none"
  
  
  
  hr_data_filtered <- tryCatch({
    hr_data %>% 
      dplyr::select(red, green, blue) %>% 
      na.omit() %>% 
      lapply(mhealthtools:::get_filtered_signal,
             sampling_rate,
             mean_filter_order,
             method) %>% 
      as.data.frame()
  }, error = function(e){NA})
  if (all(is.na(hr_data))) {
    heartrate_error_frame$error <- "Error in filtering the signal"
    return(heartrate_error_frame)
  }
  
  
  # Split each color into segments based on window_length
  hr_data_filtered_windowed <- tryCatch({
    hr_data_filtered %>%
      dplyr::select(red, green, blue) %>%
      na.omit() %>%
      lapply(mhealthtools:::window_signal, window_length, window_overlap, 'rectangle')
  }, error = function(e) { NA })
  if (all(is.na(hr_data))) {
    heartrate_error_frame$error <- "red, green, blue cannot be read from JSON"
    return(heartrate_error_frame)
  }
  
  # Get ACF metrics for each filtered segment of each color
  hr_data_acf <- hr_data_filtered_windowed %>%
    lapply(function(dfl) {
      dfl <- tryCatch({
        apply(dfl, 2, getWindowMetricsAcf, sampling_rate) %>% 
          data.table::rbindlist()
      }, error = function(e) {NA })
      # dfl <- as.data.frame(t(dfl))
      # colnames(dfl) <- c("hr", "confidence")
      dfl$window <- seq(nrow(dfl))
      return(dfl)
    })
  hr_data_acf$green$channel <- 'green'
  hr_data_acf$blue$channel <- 'blue'
  hr_data_acf <- hr_data_acf %>%
    data.table::rbindlist(fill = T)
  # hr_data$error <- "none"
  hr_data_acf$window <- hr_data_acf$window +
    max(hr_data_time$window) - max(hr_data_acf$window)
  
  hr_data_metrics <- hr_data_time %>% 
    dplyr::left_join(hr_data_acf) %>% 
    dplyr::mutate(sampling_rate = sampling_rate,
                  sampling_rate_md = sampling_rate_md,
                  sampling_rate_md_plus = sampling_rate_md_plus,
                  sampling_rate_md_minus = sampling_rate_md_minus,
                  sampling_rate_max = sampling_rate_max,
                  sampling_rate_min = sampling_rate_min)
  
  return(hr_data_metrics)
}


getWindowMetricsTime <- function(x, sampling_rate, channel = 'red'){
  x_time_stats <- mhealthtools:::time_domain_summary(values = x, sampling_rate = sampling_rate)
  return(cbind(channel = channel, x_time_stats))
}


getWindowMetricsAcf <- function(x, sampling_rate, channel = 'red',
                                min_hr = 45, max_hr = 240){
  # x is a time series
  max_lag = floor(60 * sampling_rate / min_hr) # 4/3 fs is 45BPM
  min_lag = ceiling(60 * sampling_rate / max_hr) # 1/3.5 fs is 210BPM
  x_mhealthtools <- mhealthtools:::get_hr_from_time_series(x, sampling_rate)
  
  x <- stats::acf(x, lag.max = max_lag, plot = F)$acf
  y <- NA * x
  y[seq(min_lag, max_lag)] <- x[seq(min_lag, max_lag)]
  y_max_pos <- which.max(y)
  y_min_pos <- which.min(y)
  most_conf_hr <-  60 * sampling_rate / (y_max_pos - 1)
  min_conf_hr <- 60 * sampling_rate / (y_min_pos - 1)
  max_conf_x <- max(x, na.rm = T)
  max_conf_y <- max(y, na.rm = T)
  min_conf_y <- min(y, na.rm = T)
  
  # Find HR most and min conf in each of the HR ranges
  # 45-90, 90-135, 135-180, 180-225 and 225-240
  # 45-90
  y <- NA * x
  lag_left <- ceiling(60 * sampling_rate / 90)
  lag_right <- floor(60 * sampling_rate / 45)
  y[seq(lag_left, lag_right)] <- x[seq(lag_left, lag_right)]
  y_max_pos <- which.max(y)
  y_min_pos <- which.min(y)
  most_conf_hr_45_90 <-  60 * sampling_rate / (y_max_pos - 1)
  min_conf_hr_45_90 <- 60 * sampling_rate / (y_min_pos - 1)
  max_conf_y_45_90 <- max(y, na.rm = T)
  min_conf_y_45_90 <- min(y, na.rm = T)
  
  # 90-135
  y <- NA * x
  lag_left <- ceiling(60 * sampling_rate / 135)
  lag_right <- floor(60 * sampling_rate / 90)
  y[seq(lag_left, lag_right)] <- x[seq(lag_left, lag_right)]
  y_max_pos <- which.max(y)
  y_min_pos <- which.min(y)
  most_conf_hr_90_135 <-  60 * sampling_rate / (y_max_pos - 1)
  min_conf_hr_90_135 <- 60 * sampling_rate / (y_min_pos - 1)
  max_conf_y_90_135 <- max(y, na.rm = T)
  min_conf_y_90_135 <- min(y, na.rm = T)
  
  # 135-180
  y <- NA * x
  lag_left <- ceiling(60 * sampling_rate / 180)
  lag_right <- floor(60 * sampling_rate / 135)
  y[seq(lag_left, lag_right)] <- x[seq(lag_left, lag_right)]
  y_max_pos <- which.max(y)
  y_min_pos <- which.min(y)
  most_conf_hr_135_180 <-  60 * sampling_rate / (y_max_pos - 1)
  min_conf_hr_135_180 <- 60 * sampling_rate / (y_min_pos - 1)
  max_conf_y_135_180 <- max(y, na.rm = T)
  min_conf_y_135_180 <- min(y, na.rm = T)
  
  # 180-225
  y <- NA * x
  lag_left <- ceiling(60 * sampling_rate / 225)
  lag_right <- floor(60 * sampling_rate / 180)
  y[seq(lag_left, lag_right)] <- x[seq(lag_left, lag_right)]
  y_max_pos <- which.max(y)
  y_min_pos <- which.min(y)
  most_conf_hr_180_225 <-  60 * sampling_rate / (y_max_pos - 1)
  min_conf_hr_180_225 <- 60 * sampling_rate / (y_min_pos - 1)
  max_conf_y_180_225 <- max(y, na.rm = T)
  min_conf_y_180_225 <- min(y, na.rm = T)
  
  # 225-240
  y <- NA * x
  lag_left <- ceiling(60 * sampling_rate / 240)
  lag_right <- floor(60 * sampling_rate / 225)
  y[seq(lag_left, lag_right)] <- x[seq(lag_left, lag_right)]
  y_max_pos <- which.max(y)
  y_min_pos <- which.min(y)
  most_conf_hr_225_240 <-  60 * sampling_rate / (y_max_pos - 1)
  min_conf_hr_225_240 <- 60 * sampling_rate / (y_min_pos - 1)
  max_conf_y_225_240 <- max(y, na.rm = T)
  min_conf_y_225_240 <- min(y, na.rm = T)
  
  
  acf_feat <- data.frame(
    mhealthtools_hr = x_mhealthtools[1],
    mhealthtools_conf = x_mhealthtools[2],
    most_conf_hr = most_conf_hr,
    min_conf_hr = min_conf_hr,
    max_conf_x = max_conf_x,
    max_conf_y = max_conf_y,
    min_conf_y = min_conf_y,
    most_conf_hr_45_90 = most_conf_hr_45_90,
    min_conf_hr_45_90 = min_conf_hr_45_90,
    max_conf_y_45_90 = max_conf_y_45_90,
    min_conf_y_45_90 = min_conf_y_45_90,
    most_conf_hr_90_135 = most_conf_hr_90_135,
    min_conf_hr_90_135 = min_conf_hr_90_135,
    max_conf_y_90_135 = max_conf_y_90_135,
    min_conf_y_90_135 = min_conf_y_90_135,
    most_conf_hr_135_180 = most_conf_hr_135_180,
    min_conf_hr_135_180 = min_conf_hr_135_180,
    max_conf_y_135_180 = max_conf_y_135_180,
    min_conf_y_135_180 = min_conf_y_135_180,
    most_conf_hr_180_225 = most_conf_hr_180_225,
    min_conf_hr_180_225 = min_conf_hr_180_225,
    max_conf_y_180_225 = max_conf_y_180_225,
    min_conf_y_180_225 = min_conf_y_180_225,
    most_conf_hr_225_240 = most_conf_hr_225_240,
    min_conf_hr_225_240 = min_conf_hr_225_240,
    max_conf_y_225_240 = max_conf_y_225_240,
    min_conf_y_225_240 = min_conf_y_225_240
  )  
  
  return(cbind(channel = channel, acf_feat))
  
}

# Get a curated heart rate dataframe from the mhealthtools::get_heartrate() result,
# and add method column ('acf','peak','psd') to it
getHRMetricsdataframe <- function(hr.json.fileLocation, window_length_ = 10,
                           window_overlap_ = 0.9, method_ = 'acf'){
  
  if(is.na(hr.json.fileLocation)){
    hr.data <- NA
  }else{
    hr.data <- jsonlite::fromJSON(hr.json.fileLocation)
    nrow_data <- nrow(hr.data)
    hr.data <- hr.data[2:nrow_data,] # remove the first sample for time
    hr.data$timestamp <- hr.data$timestamp - min(hr.data$timestamp, na.rm = T)
    hr.data$t <- hr.data$timestamp
    
    if(hr.json.fileLocation == './cameraHeartRate_recorder.json'){
      tag <- 'android'
    }else{
      tag <- 'iPhone'
    }
    # hr.times <- getStartAndStopTime(dat = hr.data, tag_ = tag)
  }
  ele <- get_hrdata_window_metrics(hr.data,
                                   window_length = window_length_,
                                   window_overlap = window_overlap_)
  return(ele)
}

# Convert phone string tags into their simple commercial names
deMystifyPhone <- function(phone_string){
  phone <- NA
  if(grepl('iPhone SE', phone_string)){
    phone <- 'iPhone SE'
  }
  if(grepl('iPhone 8+', phone_string)){
    phone <- 'iPhone 8+'
  }
  if(grepl('Unknown iPhone', phone_string)){
    phone <- 'iPhone XS'
  }
  if(grepl('Samsung SM-J701M', phone_string)){
    phone <- 'Samsung Galaxy J7'
  }
  if(grepl('HUAWEI', phone_string)){
    phone <- 'Huawei Mate SE'
  }
  if(grepl('Motorola', phone_string)){
    phone <- 'Moto G6 Play'
  }
  if(grepl('LGE', phone_string)){
    phone <- 'LG Stylo 4'
  }
  if(grepl('Samsung SM-G965U1', phone_string)){
    phone <- 'Samsung Galaxy S9+'
  }
  return(phone)
}

deMystifyITA <- function(ITA){
  ITA <- as.numeric(ITA)
  de.ita <- NA
  tryCatch({
    if(ITA > 55){
      de.ita <- 1
    }else if(ITA > 41){
      de.ita <- 2
    }else if(ITA > 28){
      de.ita <- 3
    }else if(ITA > 10){
      de.ita <- 4
    }else if(ITA > -30){
      de.ita <- 5
    }else if(ITA < -30){
      de.ita <- 6
    }
  },
  error = function(e){
    print(ITA)
  })
  
  return(de.ita)
}

est.fitz.tbl <- read.csv(synGet('syn18082209')$path, header = T, stringsAsFactors = F) %>% 
  dplyr::select(-X)
merged.tbl <- read.csv(synGet('syn17973172')$path, header = T, stringsAsFactors = F) %>% 
  dplyr::select(-X)
merged.tbl$participantID <- as.character(merged.tbl$participantID)

est.fitz.tbl$`Participant.ID` <- as.character(est.fitz.tbl$`Participant.ID`)
# phone.hr.metrics.tbl <- phone.hr.metrics.tbl %>% 
#   dplyr::left_join(est.fitz.tbl %>% 
#                      dplyr::rename(participantID = Participant.ID)) %>% 
#   na.omit()
# 
# red.tbl <- phone.hr.metrics.tbl %>% 
#   dplyr::filter(channel == 'red')
# green.tbl <- phone.hr.metrics.tbl %>% 
#   dplyr::filter(channel == 'green')
# blue.tbl <- phone.hr.metrics.tbl %>% 
#   dplyr::filter(channel == 'blue')



#######################################
# Download Synapse Table, and select and download required columns, figure out filepath locations
#######################################
######
## CRF Validation Data (Nonin and the SpectroColorimeter readings) filepaths
######
crf.tableId = 'syn17009128'
name = 'CRF_HR_validation_questions'

all.used.ids = crf.tableId
columnsToDownload = c('iPhone SE Nonin File','iPhone 8+ Nonin file','iPhone XS Nonin File',
                      'Samsung Galaxy J7 Nonin File','Moto G6 Play Nonin File',
                      'Huawei Mate SE Nonin File','LG Stylo 4 Nonin File',
                      'Samsung Galaxy S9+ Nonin File') # For Cardio 12MT
crf.validation.tbl.syn = synTableQuery(paste('select * from', crf.tableId))
crf.validation.tbl <- crf.validation.tbl.syn$asDataFrame() %>%
  dplyr::filter(`Participant ID` > 1200)

nonin.json.loc = lapply(columnsToDownload, function(col.name){
  tbl.files = synDownloadTableColumns(crf.validation.tbl.syn, col.name) %>%
    lapply(function(x) data.frame(V1 = x)) %>% 
    data.table::rbindlist(idcol = col.name) %>% 
    plyr::rename(c('V1' = gsub('Nonin','fileLocation', col.name)))
})

crf.validation.tbl$`iPhone SE Nonin File` <- as.character(crf.validation.tbl$`iPhone SE Nonin File`)
crf.validation.tbl$`iPhone 8+ Nonin file` <- as.character(crf.validation.tbl$`iPhone 8+ Nonin file`)
crf.validation.tbl$`iPhone XS Nonin File` <- as.character(crf.validation.tbl$`iPhone XS Nonin File`)
crf.validation.tbl$`Samsung Galaxy J7 Nonin File` <- as.character(crf.validation.tbl$`Samsung Galaxy J7 Nonin File`)
crf.validation.tbl$`Samsung Galaxy S9+ Nonin File` <- as.character(crf.validation.tbl$`Samsung Galaxy S9+ Nonin File`)
crf.validation.tbl$`Huawei Mate SE Nonin File` <- as.character(crf.validation.tbl$`Huawei Mate SE Nonin File`)
crf.validation.tbl$`LG Stylo 4 Nonin File` <- as.character(crf.validation.tbl$`LG Stylo 4 Nonin File`)
crf.validation.tbl$`Moto G6 Play Nonin File` <- as.character(crf.validation.tbl$`Moto G6 Play Nonin File`)

crf.validation.table.meta = data.table::rbindlist(list(crf.validation.tbl %>%
                                                         left_join(do.call(cbind, nonin.json.loc[1]))),
                                                  use.names = T, fill = T)%>%as.data.frame

for(i in seq(2,length(columnsToDownload))){
  crf.validation.table.meta = data.table::rbindlist(list(crf.validation.table.meta %>%
                                                           left_join(do.call(cbind, nonin.json.loc[i]))),
                                                    use.names = T, fill = T) %>% as.data.frame()
}

######
## Phone Camera Json data filepaths
######
phone.tableId = 'syn17007713'
name = 'HeartRate Measurement-v8'

all.used.ids <- c(all.used.ids, phone.tableId)
hr.tbl.syn <- synTableQuery(paste("select * from ", phone.tableId))
hr.tbl <- hr.tbl.syn$asDataFrame() %>%
  dplyr::filter(answers.participantID %in% crf.validation.table.meta$`Participant ID`) %>% 
  dplyr::filter(answers.participantID > 1200) # our participantsIDs are 12xx, 34xx and 56xx

columnsToDownload = c('rawData')
# columnsToSelect = c('recordId', 'healthCode','rawData','phoneInfo','createdOn', 'createdOnTimeZone') 

hr.json.loc = lapply(columnsToDownload, function(col.name){
  tbl.files = synDownloadTableColumns(hr.tbl.syn, col.name) %>%
    lapply(function(x) data.frame(V1 = x)) %>% 
    data.table::rbindlist(idcol = col.name) %>% 
    plyr::rename(c('V1' = gsub('Data','.fileLocation', col.name)))
})

hr.tbl$rawData <- as.character(hr.tbl$rawData)
hr.tbl$createdOn <- as.POSIXct(hr.tbl$createdOn/1000, origin = '1970-01-01')
hr.table.meta = data.table::rbindlist(list(hr.tbl %>%
                                             left_join(do.call(cbind, hr.json.loc[1]))),
                                      use.names = T, fill = T)%>%as.data.frame

# Get createdOn date and timezone for each participant, for adjusting Nonin 
# data later on
createdon.tbl <- hr.table.meta %>%
  dplyr::select(answers.participantID, createdOn, createdOnTimeZone) %>% 
  dplyr::mutate(createdOnDate = as.Date(createdOn)) %>% 
  dplyr::select(createdOnDate, createdOnTimeZone,
                `Participant ID` = answers.participantID) %>% 
  unique()
createdon.tbl$`Participant ID` <- as.numeric(createdon.tbl$`Participant ID`)
crf.validation.table.meta <- crf.validation.table.meta %>% 
  dplyr::left_join(createdon.tbl)

#######################################
# Extract Metrics from phone json files
#######################################

phone.hr.metrics.tbl <- apply(hr.table.meta,1,function(x){
  tryCatch({
    hr.json.fileLocation <- tryCatch({
      rawFiles <- unzip(x['raw.fileLocation'] %>% as.character())
      rawFiles[which(rawFiles %in% c('./cameraHeartRate_recorder.json',
                                     './cameraHeartRate_rgb.json'))]
    },
    error = function(e){NA}
    )
    if(is.na(hr.json.fileLocation)){
      hr.results <- NULL 
      # unlink(rawFiles)
    }else{
      hr.results <- getHRMetricsdataframe(hr.json.fileLocation, window_length_ = 10,
                                            window_overlap_ = 0.9, method_ = 'acf') 
      hr.results <- hr.results %>% 
        dplyr::mutate(phone = deMystifyPhone(x['phoneInfo'] %>% as.character()),
                      participantID = x['answers.participantID'])
      unlink(rawFiles)
    }
    return(hr.results)
  },
  error = function(e){
  return(NULL)
  })
}) %>% data.table::rbindlist() %>%
  as.data.frame() %>%
  unique()


#######################################
# Upload Data to Synapse 
#######################################
# Github link
gtToken = 'github_token.txt';
githubr::setGithubToken(as.character(read.table(gtToken)$V1))
thisFileName <- 'crf_nonin_metrics.R'
thisRepo <- getRepo(repository = "itismeghasyam/CRF_validation_analysis", ref="branch", refName='master')
thisFile <- getPermlink(repository = thisRepo, repositoryPath=thisFileName)

# Write Metrics data to Synapse
write.csv(phone.hr.metrics.tbl,file = paste0('phone_hr_metrics','.csv'),na="")
obj = File(paste0('phone_hr_metrics','.csv'), 
           name = paste0('phone_hr_metrics','.csv'), 
           parentId = 'syn11968320')
obj = synStore(obj,  used = crf.tableId, executed = thisFile)
