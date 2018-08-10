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

crf.tableId = 'syn12010238'
fitbit.tableId = 'syn12550816'
name = 'Cardio 12MT-v5'

# crf.tableId = 'syn12010132'
# fitbit.tableId = 'syn12550818'
# name = 'Cardio Stair Step-v1'
# 
# crf.tableId = 'syn12010237'
# fitbit.tableId = 'syn12550817'
# name = 'Cardio Stress Test-v1'

crf.tbl <- CovariateAnalysis::downloadFile(crf.tableId)
fitbit.tbl <- CovariateAnalysis::downloadFile(fitbit.tableId)

# Convert times into POSIXlt format
fitbit.tbl$timestamp <- strptime(fitbit.tbl$timestamp, format = '%Y-%m-%d %H:%M:%S',tz='') %>% as.POSIXct()
crf.tbl$startTime <- strptime(crf.tbl$startTime, format = '%Y-%m-%d %H:%M:%S',tz='') %>% as.POSIXct()
crf.tbl$stopTime <- strptime(crf.tbl$stopTime, format = '%Y-%m-%d %H:%M:%S',tz='') %>% as.POSIXct()

merged.tbl <- apply(crf.tbl,1,function(x){ 
  tryCatch({
    fitbit.data <- fitbit.tbl %>% dplyr::filter(recordId == x['recordId']) %>% 
    dplyr::filter(timestamp <= x['stopTime']) %>%  dplyr::filter(timestamp >= x['startTime'])
    
    x['fitbit.timestamp'] <- fitbit.data$timestamp[1]
    x['fitbit.hr'] <- fitbit.data$fitbitHR[1]
    return(x)
  },
           error = function(e){ NA })
}) %>% as.data.frame()

colN <- rownames(merged.tbl)
merged.tbl <- merged.tbl %>% transpose() %>% `colnames<-`(colN)
merged.tbl$fitbit.timestamp <- as.POSIXct(as.numeric(merged.tbl$fitbit.timestamp), origin = '1970-01-01')

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
write.csv(merged.tbl,file = paste0('merged',name,'.csv'),na="")
obj = File(paste0('merged',name,'.csv'), 
           name = paste0('merged',name,'.csv'), 
           parentId = 'syn11968320')
obj = synStore(obj,  used = all.used.ids, executed = thisFile)
