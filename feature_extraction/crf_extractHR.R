########################################################################
# CRF Project 
# Purpose: To create function to calculate HR and confidence values for each recordId(PMI) given a synapse table
# Author: Meghasyam Tummalacherla
# email: meghasyam@sagebase.org
########################################################################
rm(list=ls())
gc()
devtools::install_github('itismeghasyam/mhealthtools@crfAppVersion')

##############
# Required libraries
##############
library(jsonlite)
library(mhealthtools)
library(synapser)
library(githubr)
library(data.table)
library(tidyr)
library(plyr)
library(dplyr)

synLogin()

#############
# Required functions
#############
getTimeZone = function(time, tag){
  last = substring(time, nchar(time), nchar(time));
  if(last == 'Z'){
    return(0);
  }else{
    
    if(tag == 'recorder'){
      mins = as.numeric(substring(time, nchar(time)-1, nchar(time)));
      hours = as.numeric(substring(time, nchar(time)-4, nchar(time)-3));
      signt = substring(nchar(time)-5,nchar(time)-5)
    }else{
      mins = as.numeric(substring(time, nchar(time)-1, nchar(time)));
      hours = as.numeric(substring(time, nchar(time)-3, nchar(time)-2));
      signt = substring(nchar(time)-4,nchar(time)-4)
    }
    
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
    startTime <- strptime(dat$timestampDate[1], format = '%Y-%m-%dT%H:%M:%S') -  60*60*getTimeZone(dat$timestampDate[1],tag = 'recorder')
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
    
    # To cater to missing android data
    if('timestampDate' %in% names(dat)){
      startTime <- strptime(dat$timestampDate[1], format = '%Y-%m-%dT%H:%M:%S') - 60*60*getTimeZone(dat$timestampDate[1], tag = 'motion')
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
# SynIds and names of reference tables
ref.details <- data.frame(tableId = c('syn11665074',
                                      'syn11580624',
                                      'syn11432994'),
                          name = c('Cardio 12MT-v5',
                                   'Cardio Stress Test-v1',
                                   'Cardio Stair Step-v1'))

# Create a hr table tailored for each reference table
for (i in seq(nrow(ref.details))){
  
  # ref details from ref.details dataframe
  name = as.character(ref.details$name[i])
  tableId = as.character(ref.details$tableId[i])
  
  all.used.ids = tableId # provenance tracking
  
  columnsToDownload = c('heartRate_before_recorder.json','heartRate_after_recorder.json',
                        'heartRate_before_motion.json','heartRate_after_motion.json') 
  columnsToSelect = c('recordId', 'healthCode','externalId','dataGroups','appVersion','createdOn',
                      'createdOnTimeZone','phoneInfo','metadata.startDate','metadata.endDate',
                      'heartRate_before_recorder.json','heartRate_after_recorder.json',
                      'heartRate_before_motion.json','heartRate_after_motion.json') 
  hr.tbl.syn = synTableQuery(paste('select * from', tableId))
  hr.tbl <- hr.tbl.syn$asDataFrame() %>%
    dplyr::select(columnsToSelect)  
  hr.tbl$heartRate_before_recorder.json <- as.character(hr.tbl$heartRate_before_recorder.json)
  hr.tbl$heartRate_after_recorder.json <- as.character(hr.tbl$heartRate_after_recorder.json)
  hr.tbl$heartRate_before_motion.json <- as.character(hr.tbl$heartRate_before_motion.json)
  hr.tbl$heartRate_after_motion.json <- as.character(hr.tbl$heartRate_after_motion.json)
  
  hr.json.loc = lapply(columnsToDownload, function(col.name){
    tbl.files = synDownloadTableColumns(hr.tbl.syn, col.name) %>%
      lapply(function(x) data.frame(V1 = x)) %>% 
      data.table::rbindlist(idcol = col.name) %>% 
      plyr::rename(c('V1' = gsub('.json','.fileLocation', col.name)))
  })
  
  hr.table.meta = data.table::rbindlist(list(hr.tbl %>%
                                               left_join(do.call(cbind, hr.json.loc[1]))),
                                        use.names = T, fill = T)%>%as.data.frame
  
  for(i in 2:length(columnsToDownload)){
    hr.table.meta = data.table::rbindlist(list(hr.table.meta %>%
                                                 left_join(do.call(cbind, hr.json.loc[i]))),
                                          use.names = T, fill = T) %>% as.data.frame()
  }
  
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
  }) %>%
    plyr::ldply(data.frame) %>% 
    dplyr::rename('recordId' = '.id') %>% 
    dplyr::select(recordId, startTime, stopTime, tag) %>% 
    dplyr::mutate(Assay = 'before')
  
  hr.after.times <- apply(hr.table.meta,1,function(x){ 
    tryCatch({getStartAndStopTime(x, 'after')},
             error = function(e){ NA })
  }) %>%
    plyr::ldply(data.frame) %>% 
    dplyr::rename('recordId' = '.id') %>% 
    dplyr::select(recordId, startTime, stopTime, tag) %>% 
    dplyr::mutate(Assay = 'after')
  
  hr.times <- rbind(hr.before.times, hr.after.times)
  
  #############
  # Extract HR for each row
  #############
  # Before
  hr.table.meta.noNA.before <- hr.table.meta[!(is.na(hr.table.meta$heartRate_before_recorder.fileLocation)),]
  hr.before = apply(hr.table.meta.noNA.before,1,function(x){ 
    temp.dat <- tryCatch({
      jsonlite::fromJSON(as.character(x['heartRate_before_recorder.fileLocation'])) %>% 
        dplyr::mutate(t = timestamp) %>% 
        mhealthtools::get_heartrate() %>% 
        as.data.frame() %>% 
        dplyr::mutate(recordId = as.character(x['recordId'][[1]]),
                      Assay = 'before')
      # Window length is 15s (10s post filtering)
      # Window overlap is such that each consecutive window is 1s away
    },
    error = function(e){ 
      NULL
    })
    
    if(ncol(temp.dat) < 10){ 
      # the check for 10 columns because of expected no. of
      # columns from previous output after mhealthtools and
      # dplyr mutate
      temp.dat <- temp.dat %>%
        dplyr::select(-red, -green, -blue) %>%
        dplyr::mutate(
          red.hr = NA,
          red.confidence = NA,
          green.hr = NA,
          green.confidence = NA,
          blue.hr = NA,
          blue.confidence = NA)
    }
    
    window  <- nrow(temp.dat)
    temp.dat$window <- paste0('Window',1:window)
    
    return(temp.dat)
    
  }) %>% data.table::rbindlist(fill = T)
  
  # After
  hr.table.meta.noNA.after <- hr.table.meta[!(is.na(hr.table.meta$heartRate_after_recorder.fileLocation)),]
  hr.after = apply(hr.table.meta.noNA.after,1,function(x){ 
    temp.dat <- tryCatch({
      jsonlite::fromJSON(as.character(x['heartRate_after_recorder.fileLocation'])) %>% 
        dplyr::mutate(t = timestamp) %>% 
        mhealthtools::get_heartrate() %>% 
        as.data.frame() %>% 
        dplyr::mutate(recordId = as.character(x['recordId'][[1]]),
                      Assay = 'after')
      # Window length is 15s (10s post filtering)
      # Window overlap is such that each consecutive window is 1s away
    },
    error = function(e){ 
      NULL
    })
    
    if(ncol(temp.dat) < 10){
      # the check for 10 columns because of expected no. of
      # columns from previous output after mhealthtools and
      # dplyr mutate
      temp.dat <- temp.dat %>%
        dplyr::select(-red, -green, -blue) %>%
        dplyr::mutate(
          red.hr = NA,
          red.confidence = NA,
          green.hr = NA,
          green.confidence = NA,
          blue.hr = NA,
          blue.confidence = NA)
    }
    
    window  <- nrow(temp.dat)
    temp.dat$window <- paste0('Window',1:window)
    
    return(temp.dat)
    
  }) %>% data.table::rbindlist(fill = T)
  
  # Merge the before and after results and select needed columns
  hr.results <- rbind(hr.before, hr.after) %>% 
    dplyr::left_join(hr.times) %>% 
    dplyr::left_join(hr.table.meta) %>% 
    dplyr::select(recordId,
                  healthCode,
                  externalId,
                  dataGroups,
                  metadata.startDate,
                  metadata.endDate,
                  originalTable,
                  error,
                  samplingRate = sampling_rate,
                  redHR = red.hr,
                  redConf = red.confidence,
                  greenHR = green.hr,
                  greenConf = green.confidence,
                  blueHR = blue.hr,
                  blueConf = blue.confidence,
                  Assay,
                  window,
                  startTime,
                  stopTime,
                  tag)
  
  # Change start and stop time to reflect start and stop time of each window, not record
  hr.results <- hr.results %>% dplyr::mutate(
    startTime = startTime + 5 + # because we lose ~5 seconds in filtering in mhealthtools
      (10)*(1-0.9)*(as.numeric(gsub('Window','',window))-1)) %>% 
    dplyr::mutate(stopTime = startTime+10)
  
  #############
  # Upload to Synapse
  #############
  # Github link
  gtToken = 'github_token.txt';
  githubr::setGithubToken(as.character(read.table(gtToken)$V1))
  thisFileName <- 'feature_extraction/crf_extractHR.R'
  thisRepo <- getRepo(repository = "Sage-Bionetworks/CRF_validation_analysis", ref="branch", refName='master')
  thisFile <- getPermlink(repository = thisRepo, repositoryPath=thisFileName)
  
  # Write to Synapse
  write.csv(hr.results,file = paste0('hr_results',name,'.csv'),na="")
  obj = File(paste0('hr_results',name,'.csv'),
             name = paste0('hr_results',name,'.csv'),
             parentId = 'syn11968320')
  obj = synStore(obj,  used = all.used.ids, executed = thisFile)
}
