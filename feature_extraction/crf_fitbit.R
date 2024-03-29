########################################################################
# CRF Project 
# Purpose: To study the Fitbit data and make a table out of it 
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
library(jsonlite)

synLogin()

#############
# Download Synapse Table, and select and download required columns, figure out filepath locations
#############
# Download fitbit data from Synapse
tableId = 'syn22254943'
name = 'Fitbit Heart rate intraday'

all.used.ids = tableId # provenance tracking
columnsToSelect = c('participantID','createdDate','dataset','datasetInterval','datasetType')
columnsToDownload = c('dataset')

fitbit.tbl.syn = synTableQuery(paste('select * from', tableId))
fitbit.tbl <- fitbit.tbl.syn$asDataFrame() %>% 
  dplyr::select(columnsToSelect) 

fitbit.json.loc = lapply(columnsToDownload, function(col.name){
  tbl.files = synDownloadTableColumns(fitbit.tbl.syn, col.name) %>%
    lapply(function(x) data.frame(V1 = x)) %>% 
    data.table::rbindlist(idcol = col.name) %>% 
    plyr::rename(c('V1' = paste0(col.name,'.fileLocation')))
})

fitbit.tbl$dataset <- as.character(fitbit.tbl$dataset)
fitbit.table.meta = data.table::rbindlist(list(fitbit.tbl %>%
                                                 left_join(do.call(cbind, fitbit.json.loc))),
                                          use.names = T, fill = T) %>%
  as.data.frame %>%
  dplyr::rename('fitbitCreatedDate' = 'createdDate')

# SynIds and names of reference tables 
ref.details <- data.frame(tableId = c('syn22254983',
                                      'syn22119505',
                                      'syn22254980'),
                          name = c('12-MRT',
                                   'Cardio Stress Test',
                                   '3-MST'))

# Create a fitbit table tailored for each reference table
for (i in seq(nrow(ref.details))){
  
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
  
  all.used.ids = c(tableId,ref.tableId) # provenance tracking
  
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
    dplyr::rename('fitbitCreatedDate' = 'createdDate')
  
  # Single timezone
  fitbit.common.ref.monoTimeZone <- fitbit.table.meta %>%
    dplyr::inner_join(hc.timezone.tbl.monoTimeZone) %>%
    unique()
  
  # Multiple timezones, here we will consider createdOn date from the fitbit table, and from the activity table
  # to break ties in timezones for a participantID
  fitbit.common.ref.multTimeZone <- fitbit.table.meta %>%
    dplyr::inner_join(hc.timezone.tbl.multTimeZone) %>%
    unique()
  
  # Get merged common table for all participantIDs
  fitbit.common.ref <- rbind(fitbit.common.ref.monoTimeZone,
                             fitbit.common.ref.multTimeZone)
  
  fitbit.hr.tbl <- apply(fitbit.common.ref,1,function(x){ 
    tryCatch({dat <- jsonlite::fromJSON(as.character(x['dataset.fileLocation']))
    # dat <- dat %>% dplyr::mutate(recordId = x['recordId'])
    dat <- dat %>% dplyr::mutate(participantID = x['participantID'])
    dat <- dat %>% dplyr::mutate(fitbitCreatedDate = x['fitbitCreatedDate'])
    dat <- dat %>% dplyr::mutate(createdOnTimeZone = x['createdOnTimeZone'])
    },
    error = function(e){ NA })
  }) %>% plyr::ldply(data.frame) %>% dplyr::select(time, value, participantID, fitbitCreatedDate, createdOnTimeZone) %>%
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
  thisFileName <- 'feature_extraction/crf_fitbit.R'
  thisRepo <- getRepo(repository = "Sage-Bionetworks/CRF_validation_analysis", ref="branch", refName='master')
  thisFile <- getPermlink(repository = thisRepo, repositoryPath=thisFileName)
  
  # Write to Synapse
  write.csv(fitbit.hr.tbl,file = paste0('fitbit',ref.name,'.csv'),na="")
  obj = File(paste0('fitbit',ref.name,'.csv'), 
             name = paste0('fitbit',ref.name,'.csv'), 
             parentId = 'syn22268519')
  obj = synStore(obj,  used = all.used.ids, executed = thisFile)
}
