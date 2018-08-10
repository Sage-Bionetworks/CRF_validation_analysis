########################################################################
# CRF Project 
# Purpose: To estimate V02 max from hr30 (heart rate after 30sec) after completing the 12 Min Run in CRF Module
# Author: Meghasyam Tummalacherla
# email: meghasyam@sagebase.org
########################################################################

# The formula for Vo2 Max using Milligan equation is wrong, check page 191 of the actual thesis at
# https://researchportal.port.ac.uk/portal/files/5926188/Ph.D._Thesis_Gemma_Milligan_April_2013.pdf


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
library(readxl)
synapseLogin()


#############
# Download processes Synapse tables and excel files
#############

# Metadata containing V02 max values, weights etc.,
metadata.id = 'syn12257142'
pmi.metadata <- readxl::read_xlsx(synapseClient::synGet(metadata.id)@filePath) %>%
  dplyr::rename('externalId' = 'PMI ID')

# merged table containing fitbit and crf heart rate values
merged.stair.tableId = 'syn12612345'
merged.stair.tbl <- CovariateAnalysis::downloadFile(merged.stair.tableId) %>% dplyr::select(-V1)

# CRF table containing CRF HR estimates (for reference, not actually needed)
crf.stair.tableId = 'syn12010132'
crf.stair.tbl <- CovariateAnalysis::downloadFile(crf.stair.tableId) %>% dplyr::select(-V1)

# Fitbit table containing fitbit HR estimates (for reference, not actually needed)
fitbit.stair.tableId = 'syn12550818'
fitbit.stair.tbl <- CovariateAnalysis::downloadFile(fitbit.stair.tableId) %>% dplyr::select(-V1)

# Stair step test start and stop times
stair.times.tableId = 'syn12673572'
stair.times.tbl <- CovariateAnalysis::downloadFile(stair.times.tableId) %>% dplyr::select(-V1)

vo2.tbl <- merged.stair.tbl %>%
  dplyr::select(recordId, healthCode, externalId, createdOn,
                createdOnTimeZone, samplingRate, redHR, redConf,
                greenHR, greenConf, blueHR, blueConf, Assay,
                window, startTime, stopTime, fitbit.timestamp, fitbit.hr) %>% 
  dplyr::left_join(stair.times.tbl) %>% 
  dplyr::inner_join(pmi.metadata %>%
                      dplyr::select('externalId','Gender','Age','Wt (kg)'))


# Estimate V02 max

estimateVo2 <- function(pdat){
  
  # Milligan Equation for Vo2 Max
  vo2MaxMilligan <- function(hr30, age){
    if(!is.na(hr30) && !(is.na(age))){
    return(84.687-0.722*hr30-0.383*age)
    }else{
      return(NA)
    }
  }
  
  # Astrand / Van Dobeln (AVD) equations for Vo2 Max
  vo2MaxAVD <- function(hr30, weight, gender){
    if(tolower(gender) == 'male' && !(is.na(hr30)) && !(is.na(weight))){
      return(1.29*sqrt(weight/(hr30-60))*exp(-0.0088))
    }
    if(tolower(gender) == 'female' && !(is.na(hr30)) && !(is.na(weight))){
      return(1.18*sqrt(weight/(hr30-60))*exp(-0.0090))
    }else{
      return(NA)
    }
  }
  
  # Shakey Cited Equations for Vo2 Max
  vo2MaxShakey <- function(hr30, weight, gender){
    if(tolower(gender) == 'male' && !(is.na(hr30)) && !(is.na(weight))){
      maxPulse <- 64.83 + 0.662*hr30
      return(3.744*((weight+5)/(maxPulse-62)))
    }
    if(tolower(gender) == 'female' && !(is.na(hr30)) && !(is.na(weight))){
      maxPulse <- 51.33 + 0.75*hr30
      return(3.75*((weight-3)/(maxPulse-65)))
    }else{
      return(NA)
    }
  }
  
  completeCases <- function(data, desiredCols) {
    completeVec <- complete.cases(data[, desiredCols])
    return(data[completeVec, ])
  }
  
  pdat <- pdat %>% dplyr::ungroup() %>% as.data.frame()
  pdat$startTime[which(pdat$startTime == '')] <- NA
  pdat$stopTime[which(pdat$stopTime == '')] <- NA
  pdat$stairStartTime[which(pdat$stairStartTime == '')] <- NA
  pdat$stairStopTime[which(pdat$stairStopTime == '')] <- NA
  
  pdat.crf <- completeCases(pdat,c('startTime','stopTime',
                               'stairStartTime','stairStopTime','createdOn'))
  
  pdat.crf <- tryCatch({pdat.crf %>% dplyr::mutate_at(.vars = c('startTime','stopTime',
                                                                    'stairStartTime','stairStopTime','createdOn'),
                                                          .funs = as.POSIXct)
  }, error = function(e){
    pdat.crf <- data.frame()
  })
    
  if(nrow(pdat.crf)!= 0){
  hr30.timestamp <- as.POSIXct(pdat$stairStopTime[1]) + 30 #30s after the completion of the stair test
  
  # hr30.distance is the difference between the times (in abs) between the hr30.timestamp and the window
  # start and stop time, helps us decide how close hr30.timestamp is to the midpoint of the window, 
  # the lower, the better. We will choose the window with the lowest hr30.distance for the Vo2 estimates from
  # CRF HRs
  
  pdat.crf <- pdat.crf %>% dplyr::filter(startTime <= hr30.timestamp,
                                 stopTime >= hr30.timestamp) %>% 
    dplyr::mutate(hr30.distance = abs(abs(hr30.timestamp-startTime) - abs(stopTime-hr30.timestamp))) 
  
  hr30.distance.opt = min(pdat.crf$hr30.distance)
  
  pdat.crf <- pdat.crf %>% dplyr::filter(hr30.distance == hr30.distance.opt)

  redHR30 <- mean(pdat.crf$redHR %>% as.numeric())
  greenHR30 <- mean(pdat.crf$greenHR %>% as.numeric())
  blueHR30 <- mean(pdat.crf$blueHR %>% as.numeric())
  
  redHR30.conf <- mean(pdat.crf$redConf %>% as.numeric())
  greenHR30.conf <- mean(pdat.crf$greenConf %>% as.numeric())
  blueHR30.conf <- mean(pdat.crf$blueConf %>% as.numeric())
  
  }else{
    redHR30 <- NA
    greenHR30 <- NA
    blueHR30 <- NA
    redHR30.conf <- NA
    greenHR30.conf <- NA
    blueHR30.conf <- NA
  }
  pdat.fitbit <- pdat %>%
    dplyr::select(fitbit.hr, fitbit.timestamp) %>% 
    na.omit()
  
  if(nrow(pdat.fitbit) != 0){
    pdat.fitbit <- pdat.fitbit %>%
      dplyr::mutate_at(.vars = c('fitbit.timestamp'),
                       .funs = as.POSIXct) %>% 
      dplyr::mutate(hr30.distance.fitbit = abs(hr30.timestamp - fitbit.timestamp))
    hr30.distance.opt.fitbit <- min(pdat.fitbit$hr30.distance.fitbit)
    pdat.fitbit <- pdat.fitbit %>% dplyr::filter(hr30.distance.fitbit == hr30.distance.opt.fitbit)
    fitbitHR30 <- mean(pdat.fitbit$fitbit.hr %>% as.numeric())
  }else{
    fitbitHR30 <- NA
  }
  
  ageIn <- pdat$Age[1]
  genderIn <- pdat$Gender[1]
  weightIn <- pdat$`Wt (kg)`[1]
  recordIdIn <- pdat$recordId[1]
  externalIdIn <- pdat$externalId[1]
  
  dat <- data.frame(metric = c('red','green','blue','fitbit'),
                    hr30 = c(redHR30, greenHR30, blueHR30, fitbitHR30),
                    conf = c(redHR30.conf,greenHR30.conf,blueHR30.conf,NA))
  
  dat <- dat %>%
    dplyr::mutate(recordId = recordIdIn) %>% 
    dplyr::mutate(externalId = externalIdIn) %>% 
    dplyr::mutate(age = ageIn) %>% 
    dplyr::mutate(vo2Max.Milligan = vo2MaxMilligan(hr30, age = ageIn)) %>% 
    dplyr::mutate(vo2Max.AVD = vo2MaxAVD(hr30,weight = weightIn, gender = genderIn)) %>% 
    dplyr::mutate(vo2Max.Shakey = vo2MaxShakey(hr30, weight = weightIn, gender = genderIn)) 
 
  return(dat)
}

vo2.estiamtes.tbl <- vo2.tbl %>%
  dplyr::filter(Assay == 'after') %>%
  dplyr::mutate_at(.vars = c('redHR','greenHR','blueHR','redConf','greenConf','blueConf'),
                   .funs = as.numeric) %>% 
  dplyr::group_by(recordId) %>%
  do(estimateVo2(.)) %>% 
  ungroup() %>% 
  as.data.frame()
