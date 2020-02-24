########################################################################
# CRF Project 
# Purpose: Analysis on UCSD Validation Study
#          - Extract HR values for phone data
#          - Extract nonin data
#          - Merge phone hr and nonin hr data
#          - Estimate Fitz-Patrick value based on Spectrocolorimeter values
# Author: Meghasyam Tummalacherla
# email: meghasyam@sagebase.org
########################################################################
rm(list=ls())
gc()
devtools::install_github("itismeghasyam/mhealthtools@crfAppVersion")
source('feature_extraction/noninRead.R') 
# It is assumed that your working directory is ~/.../CRF_validation_analysis/
options(digits.secs = 6)
# to get the millisecond resolution

##############
# Required libraries 
##############
library(data.table)
library(tidyr)
library(plyr)
library(dplyr)
library(seewave)
library(mhealthtools) 
library(synapser)
library(githubr)
library(ggplot2)
library(parsedate)
library(lubridate)
library(jsonlite)
synLogin()

##############
# Required functions
##############
# Generate Error Frame when Nonin data is absent for the phone
noninReadError <- function(phone_){
  return(
    data.frame(
      TIMESTAMP = NA,HR.D = NA,PLETHY = NA,SpO2.D = NA,
      TAG = NA,TAG.VALUE = NA,ChkSumValid = NA,SNSD = NA,
      ARTF = NA,OOF = NA,SNSA = NA,COLOR = NA,
      phone = phone_
    )
  )
}

# Get time zone based on iPhone/non-iPhone 
getTimeZone = function(time, tag){
  last = substring(time, nchar(time), nchar(time));
  if(last == 'Z'){
    return(0);
  }else{
    
    if(tag == 'iPhone'){
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

# Get start and stop times in UTC for the given record (phone Json)
getStartAndStopTime <- function(dat, tag_ = 'iPhone'){
  # Column containing reference timepoint is timestampDate
  # Column containing time (with respect to the reference point) is timestamp
  
  if('timestampDate' %in% names(dat)){
    startTime <- strptime(dat$timestampDate[1], format = '%Y-%m-%dT%H:%M:%OS') - 60*60*getTimeZone(dat$timestampDate[1],tag = tag_)
    stopTime <- startTime + (dat$timestamp[length(dat$timestamp)]-min(dat$timestamp, na.rm = T))
  }else{
    startTime <- as.POSIXct(dat$timestamp[1], origin = '1970-01-01')
    stopTime <- as.POSIXct(dat$timestamp[length(dat$timestamp)] - min(dat$timestamp, na.rm = T),
                           origin = '1970-01-01')
  }
  return(list(startTime = startTime, stopTime = stopTime))
}

# Get a curated heart rate dataframe from the mhealthtools::get_heartrate() result,
# and add method column ('acf','peak','psd') to it
getHRdataframe <- function(hr.json.fileLocation, window_length_ = 10,
                           window_overlap_ = 0.5, method_ = 'acf'){
  
  if(is.na(hr.json.fileLocation)){
    hr.data <- NA
  }else{
    hr.data <- jsonlite::fromJSON(hr.json.fileLocation)
    hr.data$timestamp <- hr.data$timestamp - min(hr.data$timestamp, na.rm = T)
    hr.data$t <- hr.data$timestamp
    
    if(hr.json.fileLocation == './cameraHeartRate_recorder.json'){
      tag <- 'android'
    }else{
      tag <- 'iPhone'
    }
    hr.times <- getStartAndStopTime(dat = hr.data, tag_ = tag)
  }
  ele <- mhealthtools::get_heartrate(heartrate_data = hr.data,
                                     window_length = window_length_,
                                     window_overlap = window_overlap_,
                                     method = method_)
  temp = list()
  temp$error <- ele$error
  temp$samplingRate <- ele$samplingRate
  temp$redHR <-   tryCatch({unlist(ele$red$hr)}, error = function(e){ NA })
  temp$redConf <- tryCatch({unlist(ele$red$confidence)}, error = function(e){ NA })
  temp$greenHR <- tryCatch({unlist(ele$green$hr)}, error = function(e){ NA })
  temp$greenConf <- tryCatch({unlist(ele$green$confidence)}, error = function(e){ NA })
  temp$blueHR <- tryCatch({unlist(ele$blue$hr)}, error = function(e){ NA })
  temp$blueConf <- tryCatch({unlist(ele$blue$confidence)}, error = function(e){ NA })
  temp$method <- tryCatch({method_}, error = function(e){ NA })
  temp$samplingRate <- tryCatch({mhealthtools:::get_sampling_rate(hr.data)}, error = function(e){ NA })
  temp$startTime <- tryCatch({hr.times$startTime + 5}, # The offset in time is because
                             # of how mhealthtools works, we throw away 5s after filtering
                             error = function(e){ NA })
  temp$stopTime <- tryCatch({hr.times$stopTime}, error = function(e){ NA })
  
  nwindow <- max(length(temp$redHR), length(temp$greenHR), length(temp$blueHR))
  temp$window <- paste0('Window', 1:nwindow)
  
  hr.results <- as.data.frame(temp)
  # Change start and stop time to reflect start and stop time of each window, not record
  hr.results <- hr.results %>% dplyr::mutate(
    startTime = startTime +
      (window_length_)*(1-window_overlap_)*(as.numeric(gsub('Window','',window))-1)) %>% 
    dplyr::mutate(stopTime = startTime+window_length_)
  
  return(hr.results)
}

# Convert phone string tags into their simple commercial names
deMystifyPhone <- function(phone_string){
  phone <- NA
  if(grepl('iPhone SE', phone_string)){
    phone <- 'iPhone SE'
  }
  if(grepl('iPhone 8+', phone_string)){
    phone <- 'iPhone 8+'
  }
  if(grepl('Unknown iPhone', phone_string)){
    phone <- 'iPhone XS'
  }
  if(grepl('Samsung SM-J701M', phone_string)){
    phone <- 'Samsung Galaxy J7'
  }
  if(grepl('HUAWEI', phone_string)){
    phone <- 'Huawei Mate SE'
  }
  if(grepl('Motorola', phone_string)){
    phone <- 'Moto G6 Play'
  }
  if(grepl('LGE', phone_string)){
    phone <- 'LG Stylo 4'
  }
  if(grepl('Samsung SM-G965U1', phone_string)){
    phone <- 'Samsung Galaxy S9+'
  }
  return(phone)
}

# Convert ITA into Fitz-Patrick values
deMystifyITA <- function(ITA){
  ITA <- as.numeric(ITA)
  de.ita <- NA
  tryCatch({
    if(ITA > 55){
      de.ita <- 1
    }else if(ITA > 41){
      de.ita <- 2
    }else if(ITA > 28){
      de.ita <- 3
    }else if(ITA > 10){
      de.ita <- 4
    }else if(ITA > -30){
      de.ita <- 5
    }else if(ITA < -30){
      de.ita <- 6
    }
  },
  error = function(e){
    print(ITA)
  })
  
  return(de.ita)
}

#######################################
# Download Synapse Table, and select and download required columns, figure out filepath locations
#######################################
## CRF Validation Data (Nonin and the SpectroColorimeter readings) filepaths

crf.tableId = 'syn17009128'
name = 'CRF_HR_validation_questions'

all.used.ids = crf.tableId # provenance tracking
columnsToDownload = c('iPhone SE Nonin File','iPhone 8+ Nonin file','iPhone XS Nonin File',
                      'Samsung Galaxy J7 Nonin File','Moto G6 Play Nonin File',
                      'Huawei Mate SE Nonin File','LG Stylo 4 Nonin File',
                      'Samsung Galaxy S9+ Nonin File') 
crf.validation.tbl.syn = synTableQuery(paste('select * from', crf.tableId))
crf.validation.tbl <- crf.validation.tbl.syn$asDataFrame() %>%
  dplyr::filter(`Participant ID` > 1200)

nonin.json.loc = lapply(columnsToDownload, function(col.name){
  tbl.files = synDownloadTableColumns(crf.validation.tbl.syn, col.name) %>%
    lapply(function(x) data.frame(V1 = x)) %>% 
    data.table::rbindlist(idcol = col.name) %>% 
    plyr::rename(c('V1' = gsub('Nonin','fileLocation', col.name)))
})

crf.validation.tbl$`iPhone SE Nonin File` <- as.character(crf.validation.tbl$`iPhone SE Nonin File`)
crf.validation.tbl$`iPhone 8+ Nonin file` <- as.character(crf.validation.tbl$`iPhone 8+ Nonin file`)
crf.validation.tbl$`iPhone XS Nonin File` <- as.character(crf.validation.tbl$`iPhone XS Nonin File`)
crf.validation.tbl$`Samsung Galaxy J7 Nonin File` <- as.character(crf.validation.tbl$`Samsung Galaxy J7 Nonin File`)
crf.validation.tbl$`Samsung Galaxy S9+ Nonin File` <- as.character(crf.validation.tbl$`Samsung Galaxy S9+ Nonin File`)
crf.validation.tbl$`Huawei Mate SE Nonin File` <- as.character(crf.validation.tbl$`Huawei Mate SE Nonin File`)
crf.validation.tbl$`LG Stylo 4 Nonin File` <- as.character(crf.validation.tbl$`LG Stylo 4 Nonin File`)
crf.validation.tbl$`Moto G6 Play Nonin File` <- as.character(crf.validation.tbl$`Moto G6 Play Nonin File`)

crf.validation.table.meta = data.table::rbindlist(list(crf.validation.tbl %>%
                                                         left_join(do.call(cbind, nonin.json.loc[1]))),
                                                  use.names = T, fill = T)%>%as.data.frame

for(i in seq(2,length(columnsToDownload))){
  crf.validation.table.meta = data.table::rbindlist(list(crf.validation.table.meta %>%
                                                           left_join(do.call(cbind, nonin.json.loc[i]))),
                                                    use.names = T, fill = T) %>% as.data.frame()
}

## Phone Camera Json data filepaths

phone.tableId = 'syn17007713'
name = 'HeartRate Measurement-v8'

all.used.ids <- c(all.used.ids, phone.tableId)
hr.tbl.syn <- synTableQuery(paste("select * from ", phone.tableId))
hr.tbl <- hr.tbl.syn$asDataFrame() %>%
  dplyr::filter(answers.participantID %in% crf.validation.table.meta$`Participant ID`) %>% 
  dplyr::filter(answers.participantID > 1200) # our participantsIDs are 12xx, 34xx and 56xx

columnsToDownload = c('rawData')
# columnsToSelect = c('recordId', 'healthCode','rawData','phoneInfo','createdOn', 'createdOnTimeZone') 

hr.json.loc = lapply(columnsToDownload, function(col.name){
  tbl.files = synDownloadTableColumns(hr.tbl.syn, col.name) %>%
    lapply(function(x) data.frame(V1 = x)) %>% 
    data.table::rbindlist(idcol = col.name) %>% 
    plyr::rename(c('V1' = gsub('Data','.fileLocation', col.name)))
})

hr.tbl$rawData <- as.character(hr.tbl$rawData)
hr.table.meta = data.table::rbindlist(list(hr.tbl %>%
                                             left_join(do.call(cbind, hr.json.loc[1]))),
                                      use.names = T, fill = T)%>%as.data.frame

# Get createdOn date and timezone for each participant, for adjusting Nonin 
# data later on
createdon.tbl <- hr.table.meta %>%
  dplyr::select(answers.participantID, createdOn, createdOnTimeZone) %>% 
  dplyr::mutate(createdOnDate = as.Date(createdOn)) %>% 
  dplyr::select(createdOnDate, createdOnTimeZone,
                `Participant ID` = answers.participantID) %>% 
  unique()
createdon.tbl$`Participant ID` <- as.numeric(createdon.tbl$`Participant ID`)
crf.validation.table.meta <- crf.validation.table.meta %>% 
  dplyr::left_join(createdon.tbl)

#######################################
# Extract Heart rate data from Nonin and Phone JSON files
#######################################
######
## Extract Nonin Data for each participant and Collate all to form one Nonin data file 
######
# Note that it really does not matter if we index the nonin data by the phone type
# since we will anyway be looking at the timestamp, and for a given participant
# there is no way we can get confused because they will be using only one phone
# at any given instant. So if we have Nonin data indexed by timestamp and participantID
# it should suffice.

nonin.hr.tbl <- apply(crf.validation.table.meta,1,function(x){ 
  tryCatch(
    {
      dat.iPhoneSE <- tryCatch({noninRead(as.character(x['iPhone SE fileLocation File'])) %>% 
          dplyr::mutate(phone = 'iPhone SE')},
          error = function(e){noninReadError(phone_ = 'iPhone SE')})
      dat.iPhone8Plus <- tryCatch({noninRead(as.character(x['iPhone 8+ fileLocation file'])) %>% 
          # note it is file, not File in the column name, this was an error from the start
          dplyr::mutate(phone = 'iPhone 8+')},
          error = function(e){noninReadError(phone_ = 'iPhone 8+')})
      dat.iPhoneXs <- tryCatch({noninRead(as.character(x['iPhone XS fileLocation File'])) %>% 
          dplyr::mutate(phone = 'iPhone XS')},
          error = function(e){noninReadError(phone_ = 'iPhone XS')})
      dat.GalaxyJ7 <- tryCatch({noninRead(as.character(x['Samsung Galaxy J7 fileLocation File'])) %>% 
          dplyr::mutate(phone = 'Samsung Galaxy J7')},
          error = function(e){noninReadError(phone_ = 'Samsung Galaxy J7')})
      dat.MotoG6 <- tryCatch({noninRead(as.character(x['Moto G6 Play fileLocation File'])) %>% 
          dplyr::mutate(phone = 'Moto G6 Play')},
          error = function(e){noninReadError(phone_ = 'Moto G6 Play')})
      dat.HuaweiMateSE <- tryCatch({noninRead(as.character(x['Huawei Mate SE fileLocation File'])) %>% 
          dplyr::mutate(phone = 'Huawei Mate SE')},
          error = function(e){noninReadError(phone_ = 'Huawei Mate SE')})
      dat.LGStylo4 <- tryCatch({noninRead(as.character(x['LG Stylo 4 fileLocation File'])) %>% 
          dplyr::mutate(phone = 'LG Stylo 4')},
          error = function(e){noninReadError(phone_ = 'LG Stylo 4')})
      dat.Galaxy9Plus <- tryCatch({noninRead(as.character(x['Samsung Galaxy S9+ fileLocation File'])) %>% 
          dplyr::mutate(phone = 'Samsung Galaxy S9+')},
          error = function(e){noninReadError(phone_ = 'Samsung Galaxy S9+')})
      
      dat.Nonin <- rbind(dat.iPhoneSE, dat.iPhone8Plus, dat.iPhoneXs,
                         dat.GalaxyJ7, dat.MotoG6, dat.HuaweiMateSE,
                         dat.LGStylo4, dat.Galaxy9Plus) %>% 
        unique() %>% 
        dplyr::mutate(participantID = x['Participant ID'],
                      createdDate = as.character(x['createdOnDate']),
                      createdOnTimeZone = x['createdOnTimeZone']
                      # createdOnTimeZone = '0'
        ) %>% 
        dplyr::select(TIMESTAMP, HR.D, phone, participantID,
                      createdDate, createdOnTimeZone) %>% 
        dplyr::rename(noninHR = HR.D)
      
      # Merge createdDate and time to create a timestamp and convert that into POSIXlt format
      dat.Nonin$timestamp <- apply(dat.Nonin[,c('createdDate','TIMESTAMP')],1,paste, collapse=' ')
      dat.Nonin$timestamp <- strptime(dat.Nonin$timestamp, format = '%Y-%m-%d %H:%M:%OS')
      dat.Nonin$timestamp <- dat.Nonin$timestamp - 60*60*as.numeric(dat.Nonin$createdOnTimeZone)/100
      
      dat.Nonin <- dat.Nonin %>% 
        dplyr::select(timestamp, phone, participantID, noninHR)
      
      dat.Nonin$noninHR[as.numeric(dat.Nonin$noninHR) == 511] <- NA
      # If the pulseOx has an error estimating the HR, it will show 511BPM
      # as the default error HR
      
      return(dat.Nonin)
    },
    error = function(e){ NA }
  )
}) %>% plyr::ldply(data.frame) %>% 
  dplyr::select(timestamp, participantID, phone, noninHR)

######
## Extract heartrate for each participant for each phone and collate into one file
######

phone.hr.tbl <- apply(hr.table.meta,1,function(x){
  tryCatch({
    hr.json.fileLocation <- tryCatch({
      rawFiles <- unzip(x['raw.fileLocation'] %>% as.character())
      rawFiles[which(rawFiles %in% c('./cameraHeartRate_recorder.json',
                                     './cameraHeartRate_rgb.json'))]
    },
    error = function(e){NA}
    )
    
    if(is.na(hr.json.fileLocation)){
      hr.results <- getHRdataframe(hr.json.fileLocation, window_length_ = 10,
                                   window_overlap_ = 0.9, method_ = 'acf') 
    }else{
      hr.results.acf <- getHRdataframe(hr.json.fileLocation, window_length_ = 10,
                                       window_overlap_ = 0.9, method_ = 'acf') 
      hr.results.psd <- getHRdataframe(hr.json.fileLocation, window_length_ = 10,
                                       window_overlap_ = 0.9, method_ = 'psd')
      hr.results.peak <- getHRdataframe(hr.json.fileLocation, window_length_ = 10,
                                        window_overlap_ = 0.9, method_ = 'peak')
      hr.results <- rbind(hr.results.acf, hr.results.psd, hr.results.peak) %>% 
        dplyr::mutate(phone = deMystifyPhone(x['phoneInfo'] %>% as.character()),
                      participantID = x['answers.participantID'])
      unlink(rawFiles)
    }
    return(hr.results)
  },
  error = function(e){hr.results <- getHRdataframe(NA) %>% 
    dplyr::mutate(phone = deMystifyPhone(x['phoneInfo'] %>% as.character()),
                  participantID = x['answers.participantID'])
  hr.results$redHR <- as.numeric(hr.results$redHR)
  hr.results$greenHR <- as.numeric(hr.results$greenHR)
  hr.results$blueHR <- as.numeric(hr.results$blueHR)
  hr.results$redConf <- as.numeric(hr.results$redConf)
  hr.results$greenConf <- as.numeric(hr.results$greenConf)
  hr.results$blueConf <- as.numeric(hr.results$blueConf)
  hr.results$method <- as.character(hr.results$method)
  hr.results$samplingRate <- as.numeric(hr.results$samplingRate)
  hr.results$startTime <- as.POSIXct(hr.results$startTime, origin='1970-01-01')
  hr.results$stopTime <- as.POSIXct(hr.results$stopTime, origin = '1970-01-01')
  return(hr.results)
  })
}) %>% data.table::rbindlist() %>%
  as.data.frame() %>%
  unique()

#######################################
# Merge Nonin and CRF data together 
#######################################
merged.tbl <- apply(phone.hr.tbl,1,function(x){ 
  tryCatch({
    nonin.data <- nonin.hr.tbl %>%
      dplyr::filter(participantID == x['participantID']) %>% 
      dplyr::filter(phone == x['phone']) %>% 
      dplyr::filter(timestamp <= x['stopTime']) %>% 
      dplyr::filter(timestamp >= x['startTime'])
    
    x['noninTimestamp'] <- tryCatch({median(nonin.data$timestamp)},error = function(e){NA})
    x['noninHR'] <- tryCatch({median(nonin.data$noninHR)},error = function(e){NA})
    return(x)
  },
  error = function(e){
    x['noninTimestamp'] <- tryCatch({median(nonin.data$timestamp)},error = function(e){NA})
    x['noninHR'] <- tryCatch({median(nonin.data$noninHR)},error = function(e){NA})
    return(x)
  })
}) %>% as.data.frame()

colN <- rownames(merged.tbl)
merged.tbl <- merged.tbl %>% 
  data.table::transpose() %>%
  `colnames<-`(colN)
merged.tbl$noninTimestamp <- as.POSIXct(as.numeric(merged.tbl$noninTimestamp), origin = '1970-01-01')
merged.tbl$startTime <- strptime(merged.tbl$startTime, format = '%Y-%m-%d %H:%M:%OS',tz='') %>% as.POSIXct()
merged.tbl$stopTime <- strptime(merged.tbl$stopTime, format = '%Y-%m-%d %H:%M:%OS',tz='') %>% as.POSIXct()
options(digits.secs = 3) # reset the seconds digit resolution

#######################################
# Calculate a table of estimated Fitzpatrick scales from Spectrocolorimeter values 
#######################################
est.fitz.tbl <- crf.validation.table.meta %>% 
  dplyr::select(`Participant ID`, `Face L*`, `Face b*`,
                `Finger L*`, `Finger b*`, `Eye Color`,
                `Natural Hair Color`, `Natural Skin Color`,
                `Celeb Choice - Please select the number that best matches you based on your skin.`) %>% 
  unique() %>% 
  dplyr::mutate(face.fitzpatrick.ita = atan((`Face L*` - 50)/`Face b*`)*180/pi) %>% 
  dplyr::mutate(finger.fitzpatrick.ita = atan((`Finger L*` - 50)/`Finger b*`)*180/pi) %>%
  unique()

est.fitz.tbl$face.fitzpatrick <- lapply(est.fitz.tbl$face.fitzpatrick.ita, deMystifyITA) %>% 
  unlist()
est.fitz.tbl$finger.fitzpatrick <- lapply(est.fitz.tbl$finger.fitzpatrick.ita, deMystifyITA) %>% 
  unlist()

#######################################
# Upload Data to Synapse
#######################################
# Github link
gtToken = 'github_token.txt';
githubr::setGithubToken(as.character(read.table(gtToken)$V1))
thisFileName <- 'feature_extraction/crf_nonin.R'
thisRepo <- getRepo(repository = "itismeghasyam/CRF_validation_analysis", ref="branch", refName='sagebio_master')
thisFile <- getPermlink(repository = thisRepo, repositoryPath=thisFileName)

# Write Nonin data to Synapse
write.csv(nonin.hr.tbl,file = paste0('nonin','.csv'),na="")
obj = File(paste0('nonin','.csv'),
           name = paste0('nonin','.csv'),
           parentId = 'syn11968320')
obj = synStore(obj,  used = crf.tableId, executed = thisFile)

# Write Phone data to Synapse
write.csv(phone.hr.tbl,file = paste0('crf_phone','.csv'),na="")
obj = File(paste0('crf_phone','.csv'),
           name = paste0('crf_phone','.csv'),
           parentId = 'syn11968320')
obj = synStore(obj,  used = phone.tableId, executed = thisFile)

# Write Merged data to Synapse
write.csv(merged.tbl,file = paste0('merged_crf_nonin','.csv'),na="")
obj = File(paste0('merged_crf_nonin','.csv'),
           name = paste0('merged_crf_nonin','.csv'),
           parentId = 'syn12435196')
obj = synStore(obj,  used = all.used.ids, executed = thisFile)

# Write Estimated FitzPatrick scales to Synapse
write.csv(est.fitz.tbl,file = paste0('est_fitzpatrick','.csv'),na="")
obj = File(paste0('est_fitzpatrick','.csv'),
           name = paste0('est_fitzpatrick','.csv'),
           parentId = 'syn12435196')
obj = synStore(obj,  used = all.used.ids, executed = thisFile)

