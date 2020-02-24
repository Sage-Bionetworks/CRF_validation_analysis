########################################################################
# CRF Project 
# Purpose: To merge fitbit, polar and crf app data to compare the results
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
ref.details <- data.frame(crf_tableId = c('syn12010238',
                                      'syn12010132',
                                      'syn12010237'),
                          fitbit_tableId = c('syn12550816',
                                          'syn12550818',
                                          'syn12550817'),
                          polar_tableId = c('syn16811362',
                                          'syn16811501',
                                          'syn16811363'),
                          name = c('Cardio 12MT-v5',
                                   'Cardio Stair Step-v1',
                                   'Cardio Stress Test-v1'))

for(i in 1:nrow(ref.details)){
  
  # SynIds of the required tables
  crf.tableId = ref.details$crf_tableId[i]
  fitbit.tableId = ref.details$fitbit_tableId[i]
  polar.tableId = ref.details$polar_tableId[i]
  name = ref.details$name[i]
  
  # Download the tables from synapse
  crf.tbl <- read.csv(synapser::synGet(crf.tableId)$path) %>% dplyr::select(-X)
  fitbit.tbl <- read.csv(synapser::synGet(fitbit.tableId)$path) %>% dplyr::select(-X)
  polar.tbl <- read.csv(synapser::synGet(polar.tableId)$path) %>% dplyr::select(-X)
  all.used.ids <- c(crf.tableId, fitbit.tableId, polar.tableId) # provenance tracking
  
  # Convert times into POSIXlt format
  fitbit.tbl$timestamp <- strptime(fitbit.tbl$timestamp, format = '%Y-%m-%d %H:%M:%S',tz='') %>% as.POSIXct()
  crf.tbl$startTime <- strptime(crf.tbl$startTime, format = '%Y-%m-%d %H:%M:%S',tz='') %>% as.POSIXct()
  crf.tbl$stopTime <- strptime(crf.tbl$stopTime, format = '%Y-%m-%d %H:%M:%S',tz='') %>% as.POSIXct()
  polar.tbl$timestamp <- strptime(polar.tbl$timestamp, format = '%Y-%m-%d %H:%M:%S',tz='') %>% as.POSIXct()
  polar.tbl$externalId <- as.character(polar.tbl$externalId)
  
  # Merge crf and fitbit data
  merged.tbl <- apply(crf.tbl,1,function(x){ 
    tryCatch({
      fitbit.data <- fitbit.tbl %>% dplyr::filter(healthCode == x['healthCode']) %>% 
        dplyr::filter(timestamp <= x['stopTime']) %>%  dplyr::filter(timestamp >= x['startTime'])
      
      x['fitbit.timestamp'] <- median(fitbit.data$timestamp)
      x['fitbit.hr'] <- median(fitbit.data$fitbitHR)
      return(x)
    },
    error = function(e){ NA })
  }) %>% as.data.frame()
  
  colN <- rownames(merged.tbl)
  merged.tbl <- merged.tbl %>% transpose() %>% `colnames<-`(colN)
  merged.tbl$fitbit.timestamp <- as.POSIXct(as.numeric(merged.tbl$fitbit.timestamp), origin = '1970-01-01')
  merged.tbl$startTime <- strptime(merged.tbl$startTime, format = '%Y-%m-%d %H:%M:%S',tz='') %>% as.POSIXct()
  merged.tbl$stopTime <- strptime(merged.tbl$stopTime, format = '%Y-%m-%d %H:%M:%S',tz='') %>% as.POSIXct()
  
  # Merge polar data
  merged.tbl.2 <- apply(merged.tbl,1,function(x){ 
    tryCatch({
      polar.data <- polar.tbl %>% dplyr::filter(externalId == x['externalId']) %>% 
        dplyr::filter(timestamp <= x['stopTime']) %>%  dplyr::filter(timestamp >= x['startTime'])
      x['polar.timestamp'] <- median(polar.data$timestamp)
      x['polar.hr'] <- median(polar.data$hr)
      return(x)
    },
    error = function(e){ NA })
  }) %>% as.data.frame() 
  
  colN <- rownames(merged.tbl.2)
  merged.tbl.2 <- merged.tbl.2 %>% transpose() %>% `colnames<-`(colN)
  merged.tbl.2$polar.timestamp <- as.POSIXct(as.numeric(merged.tbl.2$polar.timestamp), origin = '1970-01-01')
  
  #############
  # Upload to Synapse
  #############
  # Github link
  gtToken = 'github_token.txt';
  githubr::setGithubToken(as.character(read.table(gtToken)$V1))
  thisFileName <- 'analysis/crf_mergeCompare.R'
  thisRepo <- getRepo(repository = "itismeghasyam/CRF_validation_analysis", ref="branch", refName='sagebio_master')
  thisFile <- getPermlink(repository = thisRepo, repositoryPath=thisFileName)
  
  # Write to Synapse
  write.csv(merged.tbl.2,file = paste0('merged',name,'.csv'),na="")
  obj = File(paste0('merged',name,'.csv'), 
             name = paste0('merged',name,'.csv'), 
             parentId = 'syn11968320')
  obj = synStore(obj,  used = all.used.ids, executed = thisFile)
}

