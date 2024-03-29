########################################################################
# CRF Project 
# Purpose: To merge all the Polar CSV data files and make a table out of it 
# Author: Meghasyam Tummalacherla
# email: meghasyam@sagebase.org
########################################################################
# Assuming that the working directory is ~/.../CRF_validation_analysis/
rm(list=ls())
gc()
devtools::install_github('itismeghasyam/mhealthtools@crfAppVersion')

##############
# Required libraries
##############
library(data.table)
library(synapser)
library(githubr)
library(plyr)
library(dplyr)
library(stringr)
library(jsonlite)

synLogin()

#############
# Download polar data from Synapse and figure out filepath locations
#############
polar.syn.id <- 'syn22125134'
polar.syn <- synGet(polar.syn.id)
# Get file path locations after unzipping the zip file
polar.files <- unzip(polar.syn$path) 

# read and merge all polar data into a file
polar_data <- NULL

for(file.i in polar.files){
  
  # For participantID
  a0 <- stringr::str_split(file.i, '/') %>% 
    unlist()
  a0 <- a0[length(a0)]
  current_external_id <- substring(a0, 1,6)
  
  # For time and HR
  a1 <- read.csv(file.i, skip = 2) %>% 
    dplyr::select('Time', 'HR..bpm.') %>% 
    dplyr::rename('time' = 'Time',
                  'hr' = 'HR..bpm.')
  
  # For other values in the polar data
  a2 <- read.csv(file.i, nrows = 1) %>% 
    dplyr::select('Sport', 'Date', 'Start.time', 'Duration') %>% 
    dplyr::select('sport' = 'Sport',
                  'date' = 'Date',
                  'start.time' = 'Start.time',
                  'duration' = 'Duration') %>% 
    dplyr::mutate(participantID = current_external_id)
  polar_data <- rbind(polar_data, cbind(a1,a2)) 
}

# Change date format in polar data to ymd from dmy
polar_data$date <- as.character(as.Date.character(polar_data$date, format = '%d-%m-%Y'))

# SynIds and names of reference tables 
ref.details <- data.frame(tableId = c('syn22254983',
                                      'syn22119505',
                                      'syn22254980'),
                          name = c('12-MRT',
                                   'Cardio Stress Test',
                                   '3-MST'))

# Create a polar table tailored for each reference table
for(i in seq(nrow(ref.details))){
  
  # ref details from ref.details dataframe
  ref.name = as.character(ref.details$name[i])
  ref.tableId = as.character(ref.details$tableId[i])
  
  # Get ref table from Synapse
  ref.tbl.syn <- synTableQuery(paste('select * from', ref.tableId))
  ref.tbl <- ref.tbl.syn$asDataFrame()
  ref.tbl <- ref.tbl %>%
    dplyr::select(recordId, participantID, createdOn, createdOnTimeZone) %>% 
    dplyr::mutate(createdDate = as.character(as.Date.character(createdOn))) %>%
    unique()
  
  all.used.ids = c(polar.syn.id,ref.tableId, 'syn22254979') # provenance tracking
  
  # Find participantIDs and timezones they are in
  hc.timezone.tbl <- ref.tbl %>% 
    dplyr::select(participantID, createdOnTimeZone) %>%
    unique()
  
  # Let us first consider participantIDs that have only one time zone, we will deal with participantID having
  # multiple timezones later
  
  # One time zone
  a <- hc.timezone.tbl %>%
    dplyr::group_by(participantID) %>% 
    dplyr::count() %>% 
    dplyr::filter(n == 1)
  
  # Multiple time zones
  b <- hc.timezone.tbl %>%
    dplyr::group_by(participantID) %>% 
    dplyr::count() %>% 
    dplyr::filter(n > 1)
  
  # Subset participantIDs to participantIDs in one timezone
  hc.timezone.tbl.monoTimeZone  <- hc.timezone.tbl %>%
    dplyr::filter(participantID %in% a$participantID) %>% 
    unique()
  
  # Subset participantIDs to participantIDs in multiple timezones
  hc.timezone.tbl.multTimeZone <- ref.tbl %>%
    dplyr::select(participantID, createdOnTimeZone, createdDate) %>% 
    dplyr::filter(participantID %in% b$participantID) %>% 
    unique() %>% 
    dplyr::rename('date' = 'createdDate')
  
  # single time zone
  polar_data_monoTimeZone <- polar_data %>% 
    dplyr::inner_join(hc.timezone.tbl.monoTimeZone)
  
  # Multiple timezones, here we will consider createdOn date from the fitbit table, and from the activity table
  # to break ties in timezones for a participantID
  polar_data_multTimeZone <- polar_data %>% 
    dplyr::inner_join(hc.timezone.tbl.multTimeZone)
  
  # Get the updated polar data that has timezone for all participantIDs
  polar_data <- rbind(polar_data_monoTimeZone, polar_data_multTimeZone)
  
  # Correct the times for the timezone
  polar_data$start.timestamp <- apply(polar_data[,c('date','start.time')],1,paste,collapse ='')
  polar_data$start.timestamp <- strptime(polar_data$start.timestamp, format = '%Y-%m-%d %H:%M:%S')
  polar_data$timestamp <- polar_data$start.timestamp + (as.numeric(polar_data$time)-1)
  polar_data$timestamp <- polar_data$timestamp - 60*60*as.numeric(polar_data$createdOnTimeZone)/100
  polar_data$start.timestamp <- polar_data$start.timestamp - 60*60*as.numeric(polar_data$createdOnTimeZone)/100
  
  #############
  # Upload to Synapse
  #############
  # Github link
  gtToken = 'github_token.txt';
  githubr::setGithubToken(as.character(read.table(gtToken)$V1))
  thisFileName <- 'feature_extraction/crf_polar.R'
  thisRepo <- getRepo(repository = "Sage-Bionetworks/CRF_validation_analysis", ref="branch", refName='master')
  thisFile <- getPermlink(repository = thisRepo, repositoryPath=thisFileName)
  
  # Upload to Synapse
  write.csv(polar_data,file = paste0('polar ', ref.name,'.csv'),na="")
  obj = File(paste0('polar ', ref.name, '.csv'), 
             name = paste0('polar', ref.name, '.csv'), 
             parentId = 'syn22125127')
  obj = synStore(obj,  used = all.used.ids, executed = thisFile)
}
