########################################################################
# CRF Project 
# Purpose: To create function to calculate HR and confidence values for each recordId(PMI) given a synapse table
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

synapseLogin()

#############
# Required functions
#############

getTimeZone = function(time)
{
  last = substring(time, nchar(time), nchar(time));
  if(last == 'Z')
  {
    return(0);
  }
  else
  {
    mins = as.numeric(substring(time, nchar(time)-1, nchar(time)));
    hours = as.numeric(substring(time, nchar(time)-4, nchar(time)-3));
    signt = substring(nchar(time)-5,nchar(time)-5)
    
    if(signt == '-'){
    return(timezone = hours + mins/60);
    }else{
    return(timezone = -(hours + mins/60));
  }
}
}

getStartAndStopTime <- function(x, assay){
  # Column containing reference timepoint is timestampDate
  # Column containing time (with respect to the reference point) is timestamp
  
  if(assay == 'before'){
    hrJsonFileLoc <- as.character(x['heartRate_before_recorder.fileLocation'][[1]])
  }else{
    hrJsonFileLoc <- as.character(x['heartRate_after_recorder.fileLocation'][[1]])
  }
  
  tag <- 'recorder'
  dat <- jsonlite::fromJSON(as.character(hrJsonFileLoc))
  
  if('timestampDate' %in% names(dat)){
    startTime <- strptime(dat$timestampDate[1], format = '%Y-%m-%dT%H:%M:%S') -  60*60*getTimeZone(dat$timestampDate[1])
    stopTime <- startTime + dat$timestamp[length(dat$timestamp)]
  }else{
    startTime <- as.POSIXct(dat$timestamp[1], origin = '1970-01-01')
    stopTime <- as.POSIXct(dat$timestamp[length(dat$timestamp)], origin = '1970-01-01')
  }
  
  if(stopTime < '2015-01-01'){
    tag <- 'motion'
    if(assay == 'before'){
      hrJsonFileLoc <- as.character(x['heartRate_before_motion.fileLocation'][[1]])
    }else{
      hrJsonFileLoc <- as.character(x['heartRate_after_motion.fileLocation'][[1]])
    }
    dat <- jsonlite::fromJSON(as.character(hrJsonFileLoc))
    if('timestampDate' %in% names(dat)){
      startTime <- strptime(dat$timestampDate[1], format = '%Y-%m-%dT%H:%M:%S') -  60*60*getTimeZone(dat$timestampDate[1])
      stopTime <- startTime + dat$timestamp[length(dat$timestamp)]
    }else{
      startTime <- as.POSIXct(dat$timestamp[1], origin = '1970-01-01')
      stopTime <- as.POSIXct(dat$timestamp[length(dat$timestamp)], origin = '1970-01-01')
    }
  }
  
  return(list(startTime = startTime, stopTime = stopTime, tag = tag))
}

#############
# Download Synapse Table, and select and download required columns, figure out filepath locations
#############
# tableId = 'syn11665074'
# name = 'Cardio 12MT-v5'

# tableId = 'syn11580624'
# name = 'Cardio Stress Test-v1'

tableId = 'syn11432994'
name = 'Cardio Stair Step-v1'

all.used.ids = tableId
columnsToDownload = c('heartRate_before_recorder.json','heartRate_after_recorder.json',
                      'heartRate_before_motion.json','heartRate_after_motion.json') # For Cardio 12MT
columnsToSelect = c('recordId', 'healthCode','externalId','dataGroups','appVersion','createdOn',
                    'createdOnTimeZone','phoneInfo','metadata.startDate','metadata.endDate',
                    'heartRate_before_recorder.json','heartRate_after_recorder.json',
                    'heartRate_before_motion.json','heartRate_after_motion.json') # For Cardio 12MT
hr.tbl = synTableQuery(paste('select * from', tableId))
hr.tbl@values = hr.tbl@values %>% dplyr::select(columnsToSelect)  

hr.json.loc = lapply(columnsToDownload, function(col.name){
  tbl.files = synDownloadTableColumns(hr.tbl, col.name) %>%
    lapply(function(x) data.frame(V1 = x)) %>% 
    data.table::rbindlist(idcol = col.name) %>% 
    plyr::rename(c('V1' = gsub('.json','.fileLocation', col.name)))
})

hr.table.meta = data.table::rbindlist(list(hr.tbl@values %>%
                                            left_join(do.call(cbind, hr.json.loc[1]))),
                                     use.names = T, fill = T)%>%as.data.frame
 
hr.table.meta = data.table::rbindlist(list(hr.table.meta %>%
                                             left_join(do.call(cbind, hr.json.loc[2]))),
                                      use.names = T, fill = T) %>% as.data.frame()

hr.table.meta = data.table::rbindlist(list(hr.table.meta %>%
                                             left_join(do.call(cbind, hr.json.loc[3]))),
                                      use.names = T, fill = T) %>% as.data.frame()

hr.table.meta = data.table::rbindlist(list(hr.table.meta %>%
                                             left_join(do.call(cbind, hr.json.loc[4]))),
                                      use.names = T, fill = T) %>% as.data.frame()

# Subset to PMI Ids
hr.table.meta <- hr.table.meta[grep('PMI', hr.table.meta$externalId),]
rownames(hr.table.meta) <- hr.table.meta$recordId
hr.table.meta$originalTable = rep(tableId, nrow(hr.table.meta))

#############
# Extract start and stop time for each row
#############

hr.before.times <- apply(hr.table.meta,1,function(x){ 
  tryCatch({getStartAndStopTime(x,'before')},
           error = function(e){ NA })
}) %>% plyr::ldply(data.frame) %>% dplyr::rename('recordId' = '.id') %>% dplyr::select(recordId, startTime, stopTime, tag) %>% 
  dplyr::mutate(Assay = 'before')

hr.after.times <- apply(hr.table.meta,1,function(x){ 
  tryCatch({getStartAndStopTime(x, 'after')},
           error = function(e){ NA })
}) %>% plyr::ldply(data.frame) %>% dplyr::rename('recordId' = '.id') %>% dplyr::select(recordId, startTime, stopTime, tag) %>% 
  dplyr::mutate(Assay = 'after')

hr.times <- rbind(hr.before.times, hr.after.times)

#############
# Extract HR for each row
#############

# Remove rows that do not have a file, i.e NA value (because the participant has not completed the task)
# hr.table.meta = hr.table.meta[complete.cases(hr.table.meta[,paste0(columnsToDownload)]),]

# Extract HR for each row for before cardio 12MT, and merge all results into a list
## If using different table please check the corresponding column name and update accordingly below
hr.before = apply(hr.table.meta,1,function(x){ 
                tryCatch({mpowertools::getHrFromJson(as.character(x['heartRate_before_recorder.fileLocation']))},
                         error = function(e){ NA })
              }) 

hr.after = apply(hr.table.meta,1,function(x){ 
  tryCatch({mpowertools::getHrFromJson(as.character(x['heartRate_after_recorder.fileLocation']))},
           error = function(e){ NA })
}) 

# reshaping data
hr.before.table = lapply(hr.before, function(ele){
  temp = list()
  temp$error <- ele$error
  temp$samplingRate <- ele$samplingRate
  temp$redHR <-   tryCatch({unlist(ele$red$hr)}, error = function(e){ NA })
  temp$redConf <- tryCatch({unlist(ele$red$confidence)}, error = function(e){ NA })
  temp$greenHR <- tryCatch({unlist(ele$green$hr)}, error = function(e){ NA })
  temp$greenConf <- tryCatch({unlist(ele$green$confidence)}, error = function(e){ NA })
  temp$blueHR <- tryCatch({unlist(ele$blue$hr)}, error = function(e){ NA })
  temp$blueConf <- tryCatch({unlist(ele$blue$confidence)}, error = function(e){ NA })
  return(temp)
})


hr.after.table = lapply(hr.after, function(ele){
  temp = list()
  temp$error <- ele$error
  temp$samplingRate <- ele$samplingRate
  temp$redHR <-   tryCatch({unlist(ele$red$hr)}, error = function(e){ NA })
  temp$redConf <- tryCatch({unlist(ele$red$confidence)}, error = function(e){ NA })
  temp$greenHR <- tryCatch({unlist(ele$green$hr)}, error = function(e){ NA })
  temp$greenConf <- tryCatch({unlist(ele$green$confidence)}, error = function(e){ NA })
  temp$blueHR <- tryCatch({unlist(ele$blue$hr)}, error = function(e){ NA })
  temp$blueConf <- tryCatch({unlist(ele$blue$confidence)}, error = function(e){ NA })
  return(temp)
})

############
# !!!! FIND RED FLAGS: RECORDS WITH FAILED HR EXTRACTIONS
############
hr.table.meta <- hr.table.meta %>% dplyr::select(-heartRate_before_recorder.fileLocation,
                                                 -heartRate_after_recorder.fileLocation,
                                                 -heartRate_before_recorder.json,
                                                 -heartRate_after_recorder.json,
                                                 -heartRate_before_motion.fileLocation,
                                                 -heartRate_after_motion.fileLocation,
                                                 -heartRate_before_motion.json,
                                                 -heartRate_after_motion.json)
# Single out the records for which the extraction has produced any error.
a.before <- vector()
for (i in 1:length(hr.before)){
  if(all(is.na(hr.before[[i]]))){
    a.before <- c(a.before,i)
  }else{
    if(!hr.before[[i]]$error == 'none'){a.before <- c(a.before,i)}
  }
}

a.after <- vector()
for (i in 1:length(hr.after)){
  if(all(is.na(hr.after[[i]]))){
    a.after <- c(a.after,i)
  }else{
    if(!hr.after[[i]]$error == 'none'){a.after <- c(a.after,i)}
  }
}

a <- unique(c(a.after,a.before))

a <- vector()
for (i in 1:length(hr.after)){
  a <- c(a, dim(hr.after[[i]]$red)[1])
  }

# Subset the failed records and their details(columnsToSelect)
#Note: These include low sampling rate errors too, crosscheck with the hr.table.meta
# to find out more, and look at hr.before to see the results
hr.error.records = hr.table.meta[a,]
hr.before.error = hr.before[names(hr.before) %in% hr.error.records$recordId]
hr.after.error = hr.after[names(hr.after) %in% hr.error.records$recordId]

# Put hr.after and hr.before in dataframes so that exporting to tsv/csv is easier
hr.after.table <- data.frame(t(data.table::rbindlist(list(hr.after.table)) %>% as.data.frame))
hr.after.table$recordId <- rownames(hr.after.table)
hr.after.table$Assay <- rep('after', nrow(hr.after.table))
colnames(hr.after.table) <- c('error','samplingRate','redHR','redConf','greenHR','greenConf','blueHR','blueConf','recordId','Assay')
hr.after.table <- merge( hr.table.meta,hr.after.table, by='recordId')

hr.before.table <- data.frame(t(data.table::rbindlist(list(hr.before.table)) %>% as.data.frame))
hr.before.table$recordId <- rownames(hr.before.table)
hr.before.table$Assay <- rep('before', nrow(hr.before.table))
colnames(hr.before.table) <- c('error','samplingRate','redHR','redConf','greenHR','greenConf','blueHR','blueConf','recordId','Assay')
hr.before.table <- merge( hr.table.meta,hr.before.table, by='recordId')

# Expand each HR reading for each record(row) into a seperate row for each window
aa <- apply(hr.before.table,1,function(rown){
  window  <- max(length(rown$redHR), length(rown$greenHR), length(rown$blueHR))
  temp <- data.frame(rown,window)
  temp$window <- paste0('Window',1:window)
  temp$redHR <- as.numeric(rown$redHR)
  temp$greenHR <- as.numeric(rown$greenHR)
  temp$blueHR <- as.numeric(rown$blueHR)
  temp$redConf <- as.numeric(rown$redConf)
  temp$greenConf <- as.numeric(rown$greenConf)
  temp$blueConf <- as.numeric(rown$blueConf)
  temp$createdOn <- rep(hr.before.table[hr.before.table$recordId == temp$recordId[1],]$createdOn, window)
  return(temp)
}) %>% cbind %>% ldply(data.frame)
hr.before.table <- copy(aa)
rm(aa)

aa <- apply(hr.after.table,1,function(rown){
  window  <- max(length(rown$redHR), length(rown$greenHR), length(rown$blueHR))
  temp <- data.frame(rown,window)
  temp$window <- paste0('Window',1:window)
  temp$redHR <- as.numeric(rown$redHR)
  temp$greenHR <- as.numeric(rown$greenHR)
  temp$blueHR <- as.numeric(rown$blueHR)
  temp$redConf <- as.numeric(rown$redConf)
  temp$greenConf <- as.numeric(rown$greenConf)
  temp$blueConf <- as.numeric(rown$blueConf)
  temp$createdOn <- rep(hr.after.table[hr.after.table$recordId == temp$recordId[1],]$createdOn, window)
  return(temp)
}) %>% cbind %>% ldply(data.frame)
hr.after.table <- copy(aa)
rm(aa)

hr.results <- rbind(hr.after.table, hr.before.table) %>% dplyr::left_join(hr.times)

# Change start and stop time to reflect start and stop time of each window, not record
hr.results <- hr.results %>% dplyr::mutate(startTime = startTime + 5*(as.numeric(gsub('Window','',window))-1)) %>% 
  dplyr::mutate(stopTime = startTime+10)

# Github link
gtToken = 'github_token.txt';
githubr::setGithubToken(as.character(read.table(gtToken)$V1))
thisFileName <- 'crf_extractHR.R'
thisRepo <- getRepo(repository = "itismeghasyam/CRF_validation_analysis", ref="branch", refName='master')
thisFile <- getPermlink(repository = thisRepo, repositoryPath=thisFileName)

# Write to Synapse
write.csv(hr.results,file = paste0('hr_results',name,'.csv'),na="")
obj = File(paste0('hr_results',name,'.csv'), 
           name = paste0('hr_results',name,'.csv'), 
           parentId = 'syn11968320')
obj = synStore(obj,  used = all.used.ids, executed = thisFile)
