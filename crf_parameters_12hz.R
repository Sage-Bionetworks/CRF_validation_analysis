rm(list=ls())
gc()
options(digits.secs = 10)
devtools::install_github('itismeghasyam/mhealthtools@develop')
# source('getHrFromJson.R')

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
library(tidyverse)

synLogin()

getACF <- function(x, max_lag){
  x[is.na(x)] <- 0
  return(stats::acf(x, lag.max = max_lag, plot = F)$acf)
}

######
## Phone Camera Json data filepaths
######
phone.tableId = 'syn17007713'
name = 'HeartRate Measurement-v8'

hr.tbl.syn <- synTableQuery(paste("select * from ", phone.tableId))
hr.tbl <- hr.tbl.syn$asDataFrame() %>%
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


## INDIVIDUAL FILES TEST
hr.json.fileLocation <- "./cameraHeartRate_recorder.json"
# hr.json.fileLocation <- "./cameraHeartRate_rgb.json"
rawFiles <- unzip(hr.table.meta$raw.fileLocation[13] %>% as.character())
hr.data <- jsonlite::fromJSON(hr.json.fileLocation) %>% 
  dplyr::select(-bpm_camera)
hr.data$t <- hr.data$timestamp - min(hr.data$timestamp, na.rm = T)

par(mfrow = c(3,1))
plot(hr.data$red, type = 'l')
plot(hr.data$green, type = 'l')
plot(hr.data$blue, type = 'l')


##############
# Calculate the outputs at each stage of the algo:
# input -> filtered signal -> chunk it into 10s windows -> HR estimates per window
##############

window_length = 10 # 10s
window_overlap = 0.9 # 90% overlap for 10s windows => 1s updates
method = 'acf' 

hr.data <- hr.data %>% 
  dplyr::filter(timestamp>0)
# sampling_rate <- mhealthtools:::get_sampling_rate(hr.data)
sampling_rate <- 1/median(diff(na.omit(hr.data$t)))

# Get max and min lags for the ACF based on sampling rate, min and max hr
min_hr = 45
max_hr = 210
max_lag = round(60 * sampling_rate / min_hr) # 4/3 fs is 45BPM
min_lag = round(60 * sampling_rate / max_hr) # 1/3.5 fs is 210BPM

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


hr.data.filtered <- hr.data %>% 
  dplyr::select(red, green, blue) %>% 
  na.omit() %>% 
  lapply(mhealthtools:::get_filtered_signal,
         sampling_rate,
         mean_filter_order,
         method) %>% 
  as.data.frame()


hr.data.filtered.chunks <- hr.data.filtered %>%
  dplyr::select(red, green, blue) %>%
  na.omit() %>%
  lapply(mhealthtools:::window_signal, window_length, window_overlap, 'rectangle')


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


io_examples_whole_12 <- list(hr_data = hr.data,
                          hr_data_filtered = hr.data.filtered,
                          hr_data_filtered_chunked = hr.data.filtered.chunks,
                          hr_data_filtered_chunked_acf = hr.data.filtered.chunks.acf,
                          hr_estimates = hr.estimates)

# Store the input output examples as a JSON
jsonlite::toJSON(io_examples_whole_12, digits = 10) %>% 
  write_lines('io_examples_whole_12hz.json')
a1 <- jsonlite::fromJSON('io_examples_whole_12hz.json')
all.equal(a1, io_examples_whole_12) # Check to see no data loss during json conversion



#######################################
# Upload Data to Synapse 
#######################################
# Github link
gtToken = 'github_token.txt';
githubr::setGithubToken(as.character(read.table(gtToken)$V1))
thisFileName <- 'crf_parameters_12hz.R'
thisRepo <- getRepo(repository = "itismeghasyam/CRF_validation_analysis", ref="branch", refName='master')
thisFile <- getPermlink(repository = thisRepo, repositoryPath=thisFileName)

# Write params data to Synapse
# Input - output file for the whole algo
obj = File('io_examples_whole_12hz.json',
           name = 'io_examples_whole_12hz.json',
           parentId = 'syn18497861')
obj = synStore(obj, executed = thisFile)

