########################################################################
# CRF Project 
# Purpose: To study the Fitbit data and make a table out of it 
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
library(jsonlite)

synapseLogin()

#############
# Download Synapse Table, and select and download required columns, figure out filepath locations
#############

## Reference Table
ref.tableId = 'syn11665074'
ref.name = 'Cardio 12MT-v5'

# ref.tableId = 'syn11580624'
# ref.name = 'Cardio Stress Test-v1'

# ref.tableId = 'syn11432994'
# ref.name = 'Cardio Stair Step-v1'

ref.tbl <- synTableQuery(paste('select * from', ref.tableId))@values
ref.tbl <- ref.tbl %>% dplyr::select(recordId, healthCode, createdOn, createdOnTimeZone) %>% 
  dplyr::mutate(createdDate = as.character(as.Date.character(createdOn))) %>% unique()

all.used.ids = ref.tableId

tableId = 'syn11673533'
name = 'HeartRate activities heart intraday'

all.used.ids = c(all.used.ids, tableId)
columnsToSelect = c('healthCode','createdDate','dataset','datasetInterval','datasetType')
columnsToDownload = c('dataset')

fitbit.tbl = synTableQuery(paste('select * from', tableId))

fitbit.tbl@values = fitbit.tbl@values %>% dplyr::select(columnsToSelect)  

fitbit.json.loc = lapply(columnsToDownload, function(col.name){
  tbl.files = synDownloadTableColumns(fitbit.tbl, col.name) %>%
    lapply(function(x) data.frame(V1 = x)) %>% 
    data.table::rbindlist(idcol = col.name) %>% 
    plyr::rename(c('V1' = paste0(col.name,'.fileLocation')))
})

fitbit.table.meta = data.table::rbindlist(list(fitbit.tbl@values %>%
                                             left_join(do.call(cbind, fitbit.json.loc))),
                                      use.names = T, fill = T)%>%as.data.frame %>%
  dplyr::rename('fitbitCreatedDate' = 'createdDate')

hc.timezone.tbl <- ref.tbl %>% 
  dplyr::select(healthCode, createdOnTimeZone) %>%
  unique()

# Let us first consider healthCodes that have only one time zone, we will deal with healthCode having
# multiple timezones later

a <- hc.timezone.tbl %>%
  dplyr::group_by(healthCode) %>% 
  dplyr::count() %>% 
  dplyr::filter(n == 1)

b <- hc.timezone.tbl %>%
  dplyr::group_by(healthCode) %>% 
  dplyr::count() %>% 
  dplyr::filter(n > 1)
  
# Subset healthCodes to healthCodes in one timezone
hc.timezone.tbl.monoTimeZone  <- hc.timezone.tbl %>%
  dplyr::filter(healthCode %in% a$healthCode) %>% 
  unique()

# Subset healthCodes to healthCodes in multiple timezones
hc.timezone.tbl.multTimeZone <- ref.tbl %>%
  dplyr::select(healthCode, createdOnTimeZone, createdDate) %>% 
  dplyr::filter(healthCode %in% b$healthCode) %>% 
  unique() %>% 
  dplyr::rename('fitbitCreatedDate' = 'createdDate')

# Single timezone
fitbit.common.ref.monoTimeZone <- fitbit.table.meta %>%
  dplyr::inner_join(hc.timezone.tbl.monoTimeZone) %>%
  unique()

# Multiple timezones, here we will consider createdOn date from the fitbit table, and from the activity table
# to break ties in timezones for a healthCode
fitbit.common.ref.multTimeZone <- fitbit.table.meta %>%
  dplyr::inner_join(hc.timezone.tbl.multTimeZone) %>%
  unique()

# Get merged common table for all healthCodes\
fitbit.common.ref <- rbind(fitbit.common.ref.monoTimeZone,
                           fitbit.common.ref.multTimeZone)

fitbit.hr.tbl <- apply(fitbit.common.ref,1,function(x){ 
  tryCatch({dat <- jsonlite::fromJSON(as.character(x['dataset.fileLocation']))
  # dat <- dat %>% dplyr::mutate(recordId = x['recordId'])
  dat <- dat %>% dplyr::mutate(healthCode = x['healthCode'])
  dat <- dat %>% dplyr::mutate(fitbitCreatedDate = x['fitbitCreatedDate'])
  dat <- dat %>% dplyr::mutate(createdOnTimeZone = x['createdOnTimeZone'])
  },
           error = function(e){ NA })
}) %>% plyr::ldply(data.frame) %>% dplyr::select(time, value, healthCode, fitbitCreatedDate, createdOnTimeZone) %>%
  dplyr::rename('fitbitHR' = 'value') %>% na.omit() %>% unique()

# Merge createdDate and time to create a timestamp and convert that into POSIXlt format
fitbit.hr.tbl$timestamp <- apply(fitbit.hr.tbl[,c('fitbitCreatedDate','time')],1,paste, collapse=' ')
fitbit.hr.tbl$timestamp <- strptime(fitbit.hr.tbl$timestamp, format = '%Y-%m-%d %H:%M:%S')
fitbit.hr.tbl$timestamp <- fitbit.hr.tbl$timestamp - 60*60*as.numeric(fitbit.hr.tbl$createdOnTimeZone)/100

#############
# Upload to Synapse
#############

# Github link
gtToken = 'github_token.txt';
githubr::setGithubToken(as.character(read.table(gtToken)$V1))
thisFileName <- 'crf_fitbit.R'
thisRepo <- getRepo(repository = "itismeghasyam/CRF_validation_analysis", ref="branch", refName='master')
thisFile <- getPermlink(repository = thisRepo, repositoryPath=thisFileName)

# Write to Synapse
write.csv(fitbit.hr.tbl,file = paste0('fitbit',ref.name,'.csv'),na="")
obj = File(paste0('fitbit',ref.name,'.csv'), 
           name = paste0('fitbit',ref.name,'.csv'), 
           parentId = 'syn11968320')
obj = synStore(obj,  used = all.used.ids, executed = thisFile)

