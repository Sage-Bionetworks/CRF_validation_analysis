########################################################################
# CRF Project 
# Purpose: To merge fitbit, polar and crf app data and upload to synapse
# Author: Meghasyam Tummalacherla
# email: meghasyam@sagebase.org
########################################################################
rm(list=ls())
gc()

##############
# Required libraries
##############
library(data.table)
library(tidyr)
library(plyr)
library(dplyr)
library(synapser)
library(githubr)
library(lubridate)

synLogin()

## Download both tables crf and fitbit
# SynIds and names of reference tables 
ref.details <- data.frame(crf_tableId = c('syn22269158',
                                          'syn22269166',
                                          'syn22269160'),
                          main_tableId = c('syn22254983',
                                           'syn22254980',
                                           'syn22119505'),
                          fitbit_tableId = c('syn22269077',
                                             'syn22269079',
                                             'syn22269078'),
                          polar_tableId = c('syn22268504',
                                            'syn22268506',
                                            'syn22268505'),
                          name = c('12-MRT',
                                   'Cardio Stress Test',
                                   '3-MST'), 
                          stringsAsFactors = F)

for(i in 1:nrow(ref.details)){
  
  # SynIds of the required tables
  crf.tableId = ref.details$crf_tableId[i]
  main.tableId = ref.details$main_tableId[i]
  fitbit.tableId = ref.details$fitbit_tableId[i]
  polar.tableId = ref.details$polar_tableId[i]
  name = ref.details$name[i]
  
  # Download the tables from synapse
  crf.tbl <- read.csv(synapser::synGet(crf.tableId)$path) %>% dplyr::select(-X)
  main.tbl <- synapser::synTableQuery(paste('select * from', main.tableId)) %>% as.data.frame()
  # main.tbl <- main.tbl[grep('PMI', main.tbl$externalId),]
  fitbit.tbl <- read.csv(synapser::synGet(fitbit.tableId)$path) %>% dplyr::select(-X)
  polar.tbl <- read.csv(synapser::synGet(polar.tableId)$path) %>% dplyr::select(-X)
  all.used.ids <- c(crf.tableId, fitbit.tableId, polar.tableId) # provenance tracking
  
  # Add required columns from main table
  crf.tbl <- crf.tbl %>% 
    dplyr::left_join(main.tbl %>% 
                       dplyr::select(recordId, 
                                     appVersion, createdOn,
                                     createdOnTimeZone, phoneInfo))
  
  # Convert times into POSIXct format
  fitbit.tbl$timestamp <- strptime(fitbit.tbl$timestamp, format = '%Y-%m-%d %H:%M:%S',tz='') %>% as.POSIXct()
  crf.tbl$startTime <- strptime(crf.tbl$startTime, format = '%Y-%m-%d %H:%M:%S',tz='') %>% as.POSIXct()
  crf.tbl$stopTime <- strptime(crf.tbl$stopTime, format = '%Y-%m-%d %H:%M:%S',tz='') %>% as.POSIXct()
  polar.tbl$timestamp <- strptime(polar.tbl$timestamp, format = '%Y-%m-%d %H:%M:%S',tz='') %>% as.POSIXct()
  polar.tbl$externalId <- as.character(polar.tbl$externalId)
  
  # Merge crf, fitbit and polar data
  merged.tbl <- apply(crf.tbl,1,function(x){ 
    tryCatch({
      
      ## fitbit
      # Pick all data that lies in the given window
      fitbit.data <- fitbit.tbl %>%
        dplyr::filter(healthCode == x['healthCode']) %>% 
        dplyr::filter(timestamp <= x['stopTime']) %>%
        dplyr::filter(timestamp >= x['startTime'])
      
      # Take a median of all values in the window
      x['fitbit.timestamp'] <- median(fitbit.data$timestamp)
      x['fitbit.hr'] <- median(fitbit.data$fitbitHR)
      
      ## polar
      polar.data <- polar.tbl %>%
        dplyr::filter(healthCode == x['healthCode']) %>%
        dplyr::filter(timestamp <= x['stopTime']) %>%
        dplyr::filter(timestamp >= x['startTime'])
      
      x['polar.timestamp'] <- median(polar.data$timestamp)
      x['polar.hr'] <- median(polar.data$hr)
      
      return(x)
    },
    error = function(e){ NA })
  }) %>%
    t() %>%
    as.data.frame(stringsAsFactors = F)
  
  # Convert times back into POSIXct format
  merged.tbl$fitbit.timestamp <- as.POSIXct(as.numeric(merged.tbl$fitbit.timestamp), origin = '1970-01-01')
  merged.tbl$polar.timestamp <- as.POSIXct(as.numeric(merged.tbl$polar.timestamp), origin = '1970-01-01')
  merged.tbl$startTime <- strptime(merged.tbl$startTime, format = '%Y-%m-%d %H:%M:%S',tz='') %>% as.POSIXct()
  merged.tbl$stopTime <- strptime(merged.tbl$stopTime, format = '%Y-%m-%d %H:%M:%S',tz='') %>% as.POSIXct()
  
  #############
  # Upload to Synapse
  #############
  # Github link
  gtToken = 'github_token.txt';
  githubr::setGithubToken(as.character(read.table(gtToken)$V1))
  thisFileName <- 'analysis/crf_mergeCompare.R'
  thisRepo <- getRepo(repository = "Sage-Bionetworks/CRF_validation_analysis", ref="branch", refName='master')
  thisFile <- getPermlink(repository = thisRepo, repositoryPath=thisFileName)
  
  # Write to Synapse
  write.csv(merged.tbl,file = paste0('merged',name,'.csv'),na="")
  obj = File(paste0('merged',name,'.csv'), 
             name = paste0('merged',name,'.csv'), 
             parentId = 'syn22268519')
  obj = synStore(obj,  used = all.used.ids, executed = thisFile)
}
