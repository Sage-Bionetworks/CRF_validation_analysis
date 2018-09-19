########################################################################
# CRF Project 
# Purpose: To merge fitbit data and crf app data to compare the results
# Author: Meghasyam Tummalacherla
# email: meghasyam@sagebase.org
########################################################################
rm(list=ls())
gc()
devtools::install_github('itismeghasyam/mpowertools')

##############
# Required libraries
##############
library(data.table)
library(tidyr)
library(plyr)
library(dplyr)
library(seewave)
library(mpowertools) 
library(synapseClient)
library(githubr)
library(ggplot2)
library(parsedate)
library(lubridate)
library(CovariateAnalysis)

synapseLogin()

## Download both tables crf and fitbit

# crf.tableId = 'syn12010238'
# fitbit.tableId = 'syn12550816'
# name = 'Cardio 12MT-v5'

# crf.tableId = 'syn12010132'
# fitbit.tableId = 'syn12550818'
# name = 'Cardio Stair Step-v1'
 
crf.tableId = 'syn12010237'
fitbit.tableId = 'syn12550817'
name = 'Cardio Stress Test-v1'

crf.tbl <- CovariateAnalysis::downloadFile(crf.tableId) %>% dplyr::select(-V1)
fitbit.tbl <- CovariateAnalysis::downloadFile(fitbit.tableId) %>% dplyr::select(-V1)
all.used.ids <- c(crf.tableId, fitbit.tableId)

# Convert times into POSIXlt format
fitbit.tbl$timestamp <- strptime(fitbit.tbl$timestamp, format = '%Y-%m-%d %H:%M:%S',tz='') %>% as.POSIXct()
crf.tbl$startTime <- strptime(crf.tbl$startTime, format = '%Y-%m-%d %H:%M:%S',tz='') %>% as.POSIXct()
crf.tbl$stopTime <- strptime(crf.tbl$stopTime, format = '%Y-%m-%d %H:%M:%S',tz='') %>% as.POSIXct()

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


# Merge Polar data
tableId = 'syn16809558'
polar.tbl <- synGet(tableId)@filePath %>% 
  read.csv() %>% 
  dplyr::select(-X) %>% 
  dplyr::select(hr, externalId, timestamp)

polar.tbl$timestamp <- strptime(polar.tbl$timestamp, format = '%Y-%m-%d %H:%M:%S',tz='') %>% as.POSIXct()
merged.tbl$startTime <- strptime(merged.tbl$startTime, format = '%Y-%m-%d %H:%M:%S',tz='') %>% as.POSIXct()
merged.tbl$stopTime <- strptime(merged.tbl$stopTime, format = '%Y-%m-%d %H:%M:%S',tz='') %>% as.POSIXct()
polar.tbl$externalId <- as.character(polar.tbl$externalId)

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
thisFileName <- 'crf_mergeCompare.R'
thisRepo <- getRepo(repository = "itismeghasyam/CRF_validation_analysis", ref="branch", refName='master')
thisFile <- getPermlink(repository = thisRepo, repositoryPath=thisFileName)

# Write to Synapse
write.csv(merged.tbl.2,file = paste0('merged',name,'.csv'),na="")
obj = File(paste0('merged',name,'.csv'), 
           name = paste0('merged',name,'.csv'), 
           parentId = 'syn11968320')
obj = synStore(obj,  used = all.used.ids, executed = thisFile)
