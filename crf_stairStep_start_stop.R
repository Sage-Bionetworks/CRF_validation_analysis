########################################################################
# CRF Project 
# Purpose: To find out start and stop times for step test using stair step accelerometer / gyrscope data
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

getStairStartAndStopTime <- function(stairJsonFileLoc){
  # Column containing reference timepoint is timestampDate
  # Column containing time (with respect to the reference point) is timestamp
 
  # iPhone has a column called stepPath that contains countdown/motion tag for the data, android does not. All the data in the android
  # json file is from the motion activity of the stair test. So if iPhone data then filter only to have the motion data

  dat <- jsonlite::fromJSON(as.character(stairJsonFileLoc))
  if(c('stepPath') %in% colnames(dat)){
    dat <- dat %>% dplyr::filter(stepPath == 'Cardio Stair Step/stairStep/motion')
  }
  
  # iPhone data, if not give NA 
  # dat <- tryCatch({ jsonlite::fromJSON(as.character(stairJsonFileLoc)) %>% dplyr::filter(stepPath == 'Cardio Stair Step/stairStep/motion') }, 
  #                error = function(e){ NA })
  # # if dat is NA, then phone is android
  # if (is.na(dat)){
  #   dat <- tryCatch({ jsonlite::fromJSON(as.character(stairJsonFileLoc))}, 
  #                   error = function(e){ NA })
  # }
  # dat <- jsonlite::fromJSON(as.character(stairJsonFileLoc)) %>% dplyr::filter(stepPath == 'Cardio Stair Step/stairStep/motion')
  if('timestampDate' %in% names(dat)){
    startTime <- strptime(dat$timestampDate[1], format = '%Y-%m-%dT%H:%M:%S') -  60*60*getTimeZone(dat$timestampDate[1])
    stopTime <- startTime + dat$timestamp[length(dat$timestamp)]
  }else{
    startTime <- as.POSIXct(dat$timestamp[1], origin = '1970-01-01')
    stopTime <- as.POSIXct(dat$timestamp[length(dat$timestamp)], origin = '1970-01-01')
  }
  return(list(stairStartTime = startTime, stairStopTime = stopTime))
}


#############
# Download Synapse Table, and select and download required columns, figure out filepath locations
#############

tableId = 'syn11432994'
name = 'Cardio Stair Step-v1'

all.used.ids = tableId
columnsToDownload = c('heartRate_before_recorder.json','heartRate_after_recorder.json','stairStep_motion.json') # For Cardio 12MT
columnsToSelect = c('recordId', 'healthCode','externalId','dataGroups','appVersion','createdOn',
                    'createdOnTimeZone','phoneInfo','metadata.startDate','metadata.endDate',
                    'heartRate_before_recorder.json','heartRate_after_recorder.json','stairStep_motion.json') # For Cardio 12MT
hr.tbl = synTableQuery(paste('select * from', tableId))
hr.tbl@values = hr.tbl@values %>% dplyr::select(columnsToSelect)  

# heart rate files
hr.json.loc = lapply(columnsToDownload[1:2], function(col.name){
  tbl.files = synDownloadTableColumns(hr.tbl, col.name) %>%
    lapply(function(x) data.frame(V1 = x)) %>% 
    data.table::rbindlist(idcol = col.name) %>% 
    plyr::rename(c('V1' = gsub('.json','.fileLocation', col.name)))
})

hr.table.meta = data.table::rbindlist(list(hr.tbl@values %>%
                                             left_join(do.call(cbind, hr.json.loc))),
                                      use.names = T, fill = T)%>%as.data.frame
# stair step motion files
hr.json.loc = lapply(columnsToDownload[3], function(col.name){
  tbl.files = synDownloadTableColumns(hr.tbl, col.name) %>%
    lapply(function(x) data.frame(V1 = x)) %>% 
    data.table::rbindlist(idcol = col.name) %>% 
    plyr::rename(c('V1' = gsub('.json','.fileLocation', col.name)))
})

hr.table.meta = data.table::rbindlist(list(hr.table.meta %>%
                                             left_join(do.call(cbind, hr.json.loc))),
                                      use.names = T, fill = T)%>%as.data.frame

# Subset to PMI Ids
hr.table.meta <- hr.table.meta[grep('PMI', hr.table.meta$externalId),]
rownames(hr.table.meta) <- hr.table.meta$recordId
hr.table.meta$originalTable = rep(tableId, nrow(hr.table.meta))

# Start and Stop times for stair test
stair.times <- apply(hr.table.meta,1,function(x){ 
  tryCatch({getStairStartAndStopTime(as.character(x['stairStep_motion.fileLocation']))},
           error = function(e){ NA })
}) %>% plyr::ldply(data.frame) %>% dplyr::rename('recordId' = '.id') %>% dplyr::select(recordId, stairStartTime, stairStopTime) 

# Github link
gtToken = 'github_token.txt';
githubr::setGithubToken(as.character(read.table(gtToken)$V1))
thisFileName <- 'crf_stairStep_start_stop.R'
thisRepo <- getRepo(repository = "itismeghasyam/CRF_validation_analysis", ref="branch", refName='master')
thisFile <- getPermlink(repository = thisRepo, repositoryPath=thisFileName)

# Write to Synapse
write.csv(stair.times,file = paste0('stair_times',name,'.csv'),na="")
obj = File(paste0('stair_times',name,'.csv'), 
           name = paste0('stair_times',name,'.csv'), 
           parentId = 'syn11968320')
obj = synStore(obj,  used = all.used.ids, executed = thisFile)

# # test
# energyVecPerTimeWindow <- function(dat, windowLen = 5){
#   nWindows = floor(max(dat$time)/windowLen)
#   energyVec <- rep(NA, nWindows)
#   for(i in seq(nWindows)){
#     tempDat <- dat %>% dplyr::filter(time >= windowLen*(i-1)) %>% dplyr::filter(time < windowLen*i)
#     energyVec[i] <- sum(tempDat$value^2) # Energy
#     energyVec[i] <- statcomp::permutation_entropy(tempDat$value)
#   }
#   return(energyVec)
# }
# 
# plotAccelGyro <- function(data.fileLocation, windowLen,plotParam = 'plot'){
# dat <- jsonlite::fromJSON(as.character(data.fileLocation))
# datAccel <- dat %>% dplyr::filter(sensorType == 'accelerometer')
# datGyro <- dat %>% dplyr::filter(sensorType == 'gyro')
# 
# netAccel <- sqrt(datAccel$x^2 + datAccel$y^2 + datAccel$z^2)
# netGyro <- sqrt(datGyro$x^2 + datGyro$y^2 + datGyro$z^2)
# timeAccel <- datAccel$timestamp
# timeGyro <- datGyro$timestamp
# 
# netAccel <- netAccel/sum(netAccel^2)
# netGyro <- netGyro/sum(netGyro^2)
# 
# datAccel <- data.frame(value = netAccel, time = timeAccel)
# datGyro <- data.frame(value = netGyro, time= timeGyro)
# 
# par(mfrow=c(2,1))
# if(plotParam == 'plot'){
# plot(timeAccel, netAccel, type = 'l')
# plot(timeGyro, netGyro, type = 'l')
#   }else{
# # plot(energyVecPerTimeWindow(datAccel, windowLen), type = 'l')
# # plot(energyVecPerTimeWindow(datGyro, windowLen), type = 'l')
# a <- stats::acf(netAccel, lag.max = 1000, plot = F)$acf
# plot(a, type = 'l', main = str(49+which.max(a[50:400])))
# plot(stats::acf(netGyro, lag.max = 1000, plot = F)$acf, type = 'l')
# 
# }
# }
# 
# plotAccelGyro(hr.table.meta$stairStep_motion.fileLocation[14],2,'plot')
# plotAccelGyro(hr.table.meta$stairStep_motion.fileLocation[14],2,'energy')
# 
