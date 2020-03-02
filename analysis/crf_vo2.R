########################################################################
# CRF Project 
# Purpose: To estimate V02 max from hr30 (heart rate after 30sec) after completing the 12 Min Run in CRF Module
# Author: Meghasyam Tummalacherla
# email: meghasyam@sagebase.org
########################################################################
# The formula for Vo2 Max using Milligan equations is from 191 of the actual thesis at
# https://researchportal.port.ac.uk/portal/files/5926188/Ph.D._Thesis_Gemma_Milligan_April_2013.pdf
rm(list=ls())
gc()
devtools::install_github('itismeghasyam/mhealthtools@crfAppVersion')

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
library(readxl)
synLogin()

##############
# Required functions
##############
# Milligan Equations for Vo2 Max
vo2MaxMilligan1 <- function(hb30to60, age){
  vo2.max <- tryCatch({
    84.687-0.722*hb30to60-0.383*age
  }, 
  error = function(e){NA})
  return(vo2.max)
}

vo2MaxMilligan2 <- function(hb30to60, age, gender){
  if(tolower(gender) == 'female'){
    gender = 1
  }else if(tolower(gender) == 'male'){
    gender = 0
  }else{
    gender = NA
  }
  
  vo2.max <- tryCatch({
    83.477-0.586*hb30to60-0.404*age-7.030*gender
  },
  error = function(e){NA})
  return(vo2.max)
}

# Sharkey(Brian J Sharkey) Cited Equations for Vo2 Max
vo2MaxSharkey <- function(hb15to30, weight, gender){
  if(tolower(gender) == 'male' && !(is.na(hb15to30)) && !(is.na(weight))){
    if(hb15to30>=0){
      maxPulse <- 64.83 + 0.662*hb15to30*4
      # hb15to30*4 because we need post excercise heartrate
      # measured between 15 and 30s
      return(3.744*((weight+5)/(maxPulse-62))*1000/weight)
      # 1000/weight is to convert l/min to ml/kg/min
    }else{
      return(NA)
    }
  }else if(tolower(gender) == 'female' && !(is.na(hb15to30)) && !(is.na(weight))){
    if(hb15to30>=0){
      maxPulse <- 51.33 + 0.75*hb15to30*4
      return(3.75*((weight-3)/(maxPulse-65))*1000/weight)
    }else{
      return(NA)
    }
  }else{
    return(NA)
  }
}

# Remove NAs/ empty rows
completeCases <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

# Estimate V02 max
estimateVo2 <- function(pdat){
  ageIn <- pdat$Age[1]
  genderIn <- pdat$Gender[1]
  weightIn <- pdat$`Wt (kg)`[1]
  recordIdIn <- pdat$recordId[1]
  externalIdIn <- pdat$externalId[1]
  
  pdat <- pdat %>% dplyr::ungroup() %>% as.data.frame()
  pdat$startTime[which(pdat$startTime == '')] <- NA
  pdat$stopTime[which(pdat$stopTime == '')] <- NA
  pdat$stairStartTime[which(pdat$stairStartTime == '')] <- NA
  pdat$stairStopTime[which(pdat$stairStopTime == '')] <- NA
  
  # CRF heartrate data
  pdat.crf <- completeCases(pdat,c('startTime','stopTime',
                                   'stairStartTime','stairStopTime'))
  
  pdat.crf <- tryCatch({pdat.crf %>%
      dplyr::mutate_at(.vars = c('startTime','stopTime',
                                 'stairStartTime','stairStopTime'),
                       .funs = as.POSIXct)
  }, error = function(e){
    pdat.crf <- data.frame()
  })
  
  pdat.crf.15to30 <- pdat.crf %>% 
    dplyr::filter(startTime >= pdat.crf$stairStopTime[1] + 10, # 15sec
                   startTime <= pdat.crf$stairStopTime[1] + 25) %>%  # 30sec 
    dplyr::summarise(recordId = recordId[1],
                     red = mean(redHR[redConf>0.5], na.rm = T)*0.25,
                     green = mean(greenHR[greenConf>0.5], na.rm = T)*0.25,
                     camerahr = mean(estHR, na.rm = T)*0.25,
                     fitbit = mean(fitbit.hr, na.rm = T)*0.25,
                     polar = mean(polar.hr, na.rm = T)*0.25) %>% 
    tidyr::gather(metric, hb15to30, 2:6)
  
  pdat.crf.30to60 <- pdat.crf %>% 
    dplyr::filter(startTime >= pdat.crf$stairStopTime[1] + 25, # 15sec
                  startTime <= pdat.crf$stairStopTime[1] + 55) %>%  # 30sec 
    dplyr::summarise(recordId = recordId[1],
                     red = mean(redHR[redConf>0.5], na.rm = T)*0.5,
                     green = mean(greenHR[greenConf>0.5], na.rm = T)*0.5,
                     camerahr = mean(estHR, na.rm = T)*0.5,
                     fitbit = mean(fitbit.hr, na.rm = T)*0.5,
                     polar = mean(polar.hr, na.rm = T)*0.5) %>% 
    tidyr::gather(metric, hb30to60, 2:6)
  
  
  dat <- pdat.crf.15to30 %>% 
    dplyr::full_join(pdat.crf.30to60) %>% 
    dplyr::ungroup() %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(recordId = recordIdIn) %>% 
    dplyr::mutate(externalId = externalIdIn) %>% 
    dplyr::mutate(age = ageIn) %>% 
    dplyr::mutate(gender = genderIn) %>% 
    dplyr::mutate(weight = weightIn) %>% 
    dplyr::mutate(vo2Max.Milligan1 = vo2MaxMilligan1(hb30to60, age = ageIn)) %>% 
    dplyr::mutate(vo2Max.Milligan2 = vo2MaxMilligan2(hb30to60, age = ageIn, gender = genderIn)) %>% 
    dplyr::mutate(vo2Max.Sharkey = vo2MaxSharkey(hb15to30, weight = weightIn, gender = genderIn))
  
  return(dat)
}


#############
# Download processes Synapse tables and excel files
#############
# Metadata containing V02 max values, weights etc.,
metadata.id = 'syn12257142'
pmi.metadata <- readxl::read_xlsx(synapser::synGet(metadata.id)$path) %>%
  dplyr::rename('externalId' = 'CRF User name') %>% 
  dplyr::mutate('inClinic' = TRUE)
all.used.ids <- c(metadata.id)

# merged table containing polar, fitbit and crf heart rate values
merged.stair.tableId = 'syn12612345'
merged.stair.tbl <- read.csv(synapser::synGet(merged.stair.tableId)$path, stringsAsFactors = F) %>% 
  dplyr::select(-X)
all.used.ids <- c(all.used.ids, merged.stair.tableId)

# Stair step test start and stop times
stair.times.tableId = 'syn12673572'
stair.times.tbl <- read.csv(synapser::synGet(stair.times.tableId)$path, stringsAsFactors = F) %>% 
  dplyr::select(-X)
all.used.ids <- c(all.used.ids, stair.times.tableId)

# Get vo2 max tbl with all the required variables
vo2.tbl <- merged.stair.tbl %>%
  dplyr::select(recordId, healthCode, externalId, 
                samplingRate, redHR, redConf,
                greenHR, greenConf, blueHR, blueConf, Assay,
                window, startTime, stopTime,
                fitbit.timestamp, fitbit.hr,
                polar.timestamp, polar.hr) %>% 
  dplyr::left_join(stair.times.tbl) %>%
  dplyr::inner_join(pmi.metadata %>%
                      dplyr::select('externalId','Sex','Age','Field Day Wt (kg)')) %>% 
  dplyr::rename('Gender' = 'Sex',
                'Wt (kg)' = 'Field Day Wt (kg)')

# Add a createdDate column
vo2.tbl$createdDate <- as.Date.character(vo2.tbl$startTime)

# To merge on the dates the experiment was done in clinic to the crf app
pmi.metadata$createdDate <- as.Date.POSIXct(pmi.metadata$`Field Date`)

# Add the inClinic column to see if the test was done in the clinic
vo2.tbl <- vo2.tbl %>% 
  dplyr::left_join(pmi.metadata %>% 
                     dplyr::select('createdDate', 'externalId','inClinic')) 
vo2.tbl$inClinic[is.na(vo2.tbl$inClinic)] <- FALSE

# Get estimated HR and confidence per row
vo2.tbl <- vo2.tbl %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(redConfN = if(redConf > 0.5 && !is.na(redConf)) redConf else NA,
                greenConfN = if(greenConf > 0.5 && !is.na(greenConf)) greenConf else NA) %>% 
  dplyr::mutate(estHR = if(!is.na(redConfN) && !is.na(greenConfN)){
    if(redConfN > greenConfN){
      redHR
    }else{
      greenHR
    }
  }else if(!is.na(redConfN)){
    redHR
  }else if(!is.na(greenConfN)){
    greenHR
  }else{
    NA
  },
  estConf = max(redConfN, greenConfN, na.rm = T)) %>% 
  dplyr::mutate(estConf = if(is.infinite(estConf)) NA else estConf) 


vo2.estiamtes.tbl <- vo2.tbl %>%
  dplyr::filter(Assay == 'after') %>%
  dplyr::mutate_at(.vars = c('redHR','greenHR','blueHR','redConf','greenConf','blueConf'),
                   .funs = as.numeric) %>% 
  dplyr::group_by(recordId) %>%
  do(estimateVo2(.)) %>% 
  ungroup() %>% 
  as.data.frame() %>% 
  dplyr::left_join(vo2.tbl %>% 
                     dplyr::select('recordId','createdDate', 'externalId','inClinic')) %>% 
  unique() 

# Github link
gtToken = 'github_token.txt';
githubr::setGithubToken(as.character(read.table(gtToken)$V1))
thisFileName <- 'analysis/crf_vo2.R'
thisRepo <- getRepo(repository = "itismeghasyam/CRF_validation_analysis", ref="branch", refName='sagebio_master')
thisFile <- getPermlink(repository = thisRepo, repositoryPath=thisFileName)

# Write to Synapse
write.csv(vo2.estiamtes.tbl,file = paste0('tecumesh_vo2_estimates','.csv'),na="")
obj = File(paste0('tecumesh_vo2_estimates','.csv'), 
           name = paste0('tecumesh_vo2_estimates','.csv'), 
           parentId = 'syn12435196')
obj = synStore(obj,  used = all.used.ids, executed = thisFile)
