########################################################################
# CRF Project 
# Purpose: To estimate V02 max from hr30 (heart rate after 30sec) after completing the 12 Min Run in CRF Module
# Author: Meghasyam Tummalacherla
# email: meghasyam@sagebase.org
########################################################################

# The formula for Vo2 Max using Milligan equations is from 191 of the actual thesis at
# https://researchportal.port.ac.uk/portal/files/5926188/Ph.D._Thesis_Gemma_Milligan_April_2013.pdf
# * The name of the person is Sharkey not Shakey, did not change this so that it does not affect
# * down stream analysis


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
library(seewave)
library(mhealthtools) 
library(synapser)
library(githubr)
library(ggplot2)
library(parsedate)
library(lubridate)
library(readxl)
synLogin()


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
merged.stair.tbl <- read.csv(synapser::synGet(merged.stair.tableId)$path) %>% 
  dplyr::select(-X)
all.used.ids <- c(all.used.ids, merged.stair.tableId)

# Stair step test start and stop times
stair.times.tableId = 'syn12673572'
stair.times.tbl <- read.csv(synapser::synGet(stair.times.tableId)$path) %>% 
  dplyr::select(-X)
all.used.ids <- c(all.used.ids, stair.times.tableId)

# Get vo2 max tbl with all the required variables
vo2.tbl <- merged.stair.tbl %>%
  dplyr::select(recordId, healthCode, externalId, createdOn,
                createdOnTimeZone, samplingRate, redHR, redConf,
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
vo2.tbl$createdDate <- as.Date.character(vo2.tbl$createdOn)


# To merge on the dates the experiment was done in clinic to the crf app
pmi.metadata$createdDate <- as.Date.POSIXct(pmi.metadata$`Field Date`)

# Add the inClinic column to see if the test was done in the clinic
vo2.tbl <- vo2.tbl %>% 
  dplyr::left_join(pmi.metadata %>% 
                     dplyr::select('createdDate', 'externalId','inClinic')) 
vo2.tbl$inClinic[is.na(vo2.tbl$inClinic)] <- FALSE

# Estimate V02 max
estimateVo2 <- function(pdat){
  ageIn <- pdat$Age[1]
  genderIn <- pdat$Gender[1]
  weightIn <- pdat$`Wt (kg)`[1]
  recordIdIn <- pdat$recordId[1]
  externalIdIn <- pdat$externalId[1]
  
  # Milligan Equations for Vo2 Max
  vo2MaxMilligan1 <- function(hb30to60, age){
    if(!is.na(hb30to60) && !(is.na(age))){
      if(hb30to60>=0){
        return(84.687-0.722*hb30to60-0.383*age)
      }else{
        return(NA)
      }
    }else{
      return(NA)
    }
  }
  
  vo2MaxMilligan2 <- function(hb30to60, age, gender){
    
    if(tolower(gender) == 'female'){
      gender = 1
    }else if(tolower(gender) == 'male'){
      gender = 0
    }else{
      gender = FALSE
    }
    
    if(!is.na(hb30to60) && !(is.na(age)) && (is.numeric(gender))){
      if(hb30to60>=0){
        return(83.477-0.586*hb30to60-0.404*age-7.030*gender)
      }else{
        return(NA)
      }
    }else{
      return(NA)
    }
  }
  
  # Astrand / Van Dobeln (AVD) equations for Vo2 Max
  vo2MaxAVD <- function(hr, load_kiloPondMeter, gender, age){
    if(tolower(gender) == 'male' && !(is.na(hr)) && !(is.na(load_kiloPondMeter)) && !(is.na(age))){
      return(1.29*sqrt(load_kiloPondMeter/(hr-60))*exp(-0.00884*age))
    }else if(tolower(gender) == 'female' && !(is.na(hr)) && !(is.na(load_kiloPondMeter)) && !(is.na(age))){
      return(1.18*sqrt(load_kiloPondMeter/(hr-60))*exp(-0.0090*age))
    }else{
      return(NA)
    }
  }
  
  # Shakey Cited Equations for Vo2 Max
  vo2MaxShakey <- function(hb15to30, weight, gender){
    if(tolower(gender) == 'male' && !(is.na(hb15to30)) && !(is.na(weight))){
      if(hb15to30>=0){
        maxPulse <- 64.83 + 0.662*hb15to30
        return(3.744*((weight+5)/(maxPulse-62)))
      }else{
        return(NA)
      }
    }else if(tolower(gender) == 'female' && !(is.na(hb15to30)) && !(is.na(weight))){
      if(hb15to30>=0){
        maxPulse <- 51.33 + 0.75*hb15to30
        return(3.75*((weight-3)/(maxPulse-65)))
      }else{
        return(NA)
      }
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
  
  # hr15.timestamp <- as.POSIXct(pdat$stairStopTime[1]) + 15 #15s after the completion of the stair test
  # hr30.timestamp <- as.POSIXct(pdat$stairStopTime[1]) + 30 #30s after the completion of the stair test
  # hr60.timestamp <- as.POSIXct(pdat$stairStopTime[1]) + 60 #60s after the completion of the stair test
  
  # hrX.distance is the difference between the times (in abs) between the hrX.timestamp and the window
  # start and stop time, helps us decide how close hrX.timestamp is to the midpoint of the window, 
  # the lower, the better. We will choose the window with the lowest hrX.distance for the Vo2 estimates
  # from CRF HRs
  
  # CRF heartrate data
  pdat.crf <- completeCases(pdat,c('startTime','stopTime',
                                   'stairStartTime','stairStopTime','createdOn'))
  
  pdat.crf <- tryCatch({pdat.crf %>%
      dplyr::mutate_at(.vars = c('startTime','stopTime',
                                 'stairStartTime','stairStopTime','createdOn'),
                       .funs = as.POSIXct)
  }, error = function(e){
    pdat.crf <- data.frame()
  })
  
  getCrfHeartRate <- function(dat.crf, hr.timestamp){
    dat.crf <- dat.crf %>% dplyr::filter(startTime <= hr.timestamp,
                                         stopTime >= hr.timestamp) %>% 
      dplyr::mutate(hr.distance = abs(abs(hr.timestamp-startTime) - abs(stopTime-hr.timestamp))) 
    
    hr.distance.opt = min(dat.crf$hr.distance)
    
    dat.crf <- dat.crf %>% dplyr::filter(hr.distance == hr.distance.opt)
    
    redHRt <- tryCatch({mean(dat.crf$redHR %>% as.numeric())}
                       ,error = function(e){return(NA)})
    greenHRt <- tryCatch({mean(dat.crf$greenHR %>% as.numeric())}
                         ,error = function(e){return(NA)})
    blueHRt <- tryCatch({mean(dat.crf$blueHR %>% as.numeric())}
                        ,error = function(e){return(NA)})
    
    redHRt.conf <- tryCatch({mean(dat.crf$redConf %>% as.numeric())}
                            ,error = function(e){return(NA)})
    greenHRt.conf <- tryCatch({mean(dat.crf$greenConf %>% as.numeric())}
                              ,error = function(e){return(NA)})
    blueHRt.conf <- tryCatch({mean(dat.crf$blueConf %>% as.numeric())}
                             ,error = function(e){return(NA)})
    
    dat.crf <- data.frame(metric = c('red','green','blue'),
                          hr = c(redHRt, greenHRt, blueHRt),
                          conf = c(redHRt.conf,greenHRt.conf,blueHRt.conf))
    
    # Estimated Heartrate is currently the most confident of 
    # red and blue channels
    # Sage-Bionetworks/CardiorespiratoryFitness-iOS/CardiorespiratoryFitness/CardiorespiratoryFitness/iOS/CRFHeartRateSampleProcessor.swift
    est.crf.dat <- dat.crf %>% 
      dplyr::filter(metric != 'blue') %>% 
      na.omit() %>% 
      dplyr::filter(conf == max(.$conf))
    
    estHRt <- tryCatch({mean(est.crf.dat$hr %>% as.numeric())}
                       ,error = function(e){return(NA)})  
    estHRt.conf <- tryCatch({mean(est.crf.dat$conf %>% as.numeric())}
                       ,error = function(e){return(NA)})  
    
    est.hr.dat <- data.frame(metric = 'camerahr',
                             hr = estHRt,
                             conf = estHRt.conf)
    
    dat.crf <- dat.crf %>% 
      dplyr::full_join(est.hr.dat)
    
    return(dat.crf)
  }
  
  if(nrow(pdat.crf)!= 0){
    # hr15.dat <- getCrfHeartRate(pdat.crf, hr15.timestamp) %>% 
    #   dplyr::rename('hr15' = 'hr',
    #                 'conf15' = 'conf')
    # hr30.dat <- getCrfHeartRate(pdat.crf, hr30.timestamp) %>% 
    #   dplyr::rename('hr30' = 'hr',
    #                 'conf30' = 'conf')
    # hr60.dat <- getCrfHeartRate(pdat.crf, hr60.timestamp) %>% 
    #   dplyr::rename('hr60' = 'hr',
    #                 'conf60' = 'conf')
    # hr.crf.dat <- hr15.dat %>%
    #   dplyr::left_join(hr30.dat) %>% 
    #   dplyr::left_join(hr60.dat)
    # }else{
    # hr15.dat <- data.frame(metric = c('red','green','blue'),
    #                        hr15 = rep(NA,3),
    #                        conf15 = rep(NA,3))
    # hr30.dat <- data.frame(metric = c('red','green','blue'),
    #                        hr30 = rep(NA,3),
    #                        conf30 = rep(NA,3))
    # hr60.dat <- data.frame(metric = c('red','green','blue'),
    #                        hr60 = rep(NA,3),
    #                        conf60 = rep(NA,3))
    # hr.crf.dat <- hr15.dat %>%
    #   dplyr::left_join(hr30.dat) %>% 
    #   dplyr::left_join(hr60.dat)
    
    hr.crf.dat <- lapply(seq(15,60), function(itime){ # Only need HR data for times 15-60s post jog
      getCrfHeartRate(pdat.crf, as.POSIXct(pdat$stairStopTime[1]) + itime) %>% 
        dplyr::mutate(time = itime)
    }) %>% data.table::rbindlist()
    
  }else{
    hr.crf.dat <- data.frame(hr = rep(NA, length(seq(15,60))),
                             time = seq(15,60),
                             metric = rep('red', length(seq(15,60))),
                             conf = rep(NA, length(seq(15,60)))) %>% 
      rbind(data.frame(hr = rep(NA, length(seq(15,60))),
                       time = seq(15,60),
                       metric = rep('green', length(seq(15,60))),
                       conf = rep(NA, length(seq(15,60))))) %>% 
      rbind(data.frame(hr = rep(NA, length(seq(15,60))),
                       time = seq(15,60),
                       metric = rep('blue', length(seq(15,60))),
                       conf = rep(NA, length(seq(15,60))))) %>% 
      rbind(data.frame(hr = rep(NA, length(seq(15,60))),
                       time = seq(15,60),
                       metric = rep('camerahr', length(seq(15,60))),
                       conf = rep(NA, length(seq(15,60))))) 
  }
  
  # Fitbit heartrate data
  pdat.fitbit <- pdat %>%
    dplyr::select(fitbit.hr, fitbit.timestamp) %>% 
    na.omit()
  
  getFitbitHeartRate <- function(dat.fitbit, hr.timestamp){
    dat.fitbit <- dat.fitbit %>% 
      dplyr::mutate(hr.distance.fitbit = abs(hr.timestamp - fitbit.timestamp))
    hr.distance.opt.fitbit <- min(dat.fitbit$hr.distance.fitbit)
    dat.fitbit <- dat.fitbit %>% dplyr::filter(hr.distance.fitbit == hr.distance.opt.fitbit)
    fitbitHRt <- tryCatch({mean(dat.fitbit$fitbit.hr %>% as.numeric())}
                          ,error = function(e){return(NA)})
    return(fitbitHRt)  
  }
  
  if(nrow(pdat.fitbit) != 0){
    pdat.fitbit <- pdat.fitbit %>%
      dplyr::mutate_at(.vars = c('fitbit.timestamp'),
                       .funs = as.POSIXct)
    
    # fitbit.dat <- data.frame(hr15 = getFitbitHeartRate(pdat.fitbit, hr15.timestamp),
    #                          hr30 = getFitbitHeartRate(pdat.fitbit, hr30.timestamp),
    #                          hr60 = getFitbitHeartRate(pdat.fitbit, hr60.timestamp),
    #                          conf15 = NA,
    #                          conf30 = NA,
    #                          conf60 = NA) %>% 
    #   dplyr::mutate(metric = 'fitbit')
    
    fitbit.dat <- lapply(seq(15,60), function(itime){ # Only need HR data for times 15-60s post jog
      getFitbitHeartRate(pdat.fitbit, as.POSIXct(pdat$stairStopTime[1]) + itime) %>% 
        as.data.frame() %>% 
        `colnames<-`(c('hr')) %>% 
        dplyr::mutate(time = itime) %>%
        dplyr::mutate(metric = 'fitbit')
    }) %>% data.table::rbindlist() %>% 
      dplyr::mutate(conf = NA)
    
    
  }else{
    # fitbit.dat <- data.frame(hr15 = NA,
    #                          hr30 = NA,
    #                          hr60 = NA,
    #                          conf15 = NA,
    #                          conf30 = NA,
    #                          conf60 = NA) %>% 
    #   dplyr::mutate(metric = 'fitbit')
    fitbit.dat <- data.frame(hr = rep(NA, length(seq(15,60))),
                             time = seq(15,60),
                             metric = rep('fitbit', length(seq(15,60))),
                             conf = rep(NA, length(seq(15,60))))
  }
  
  # Polar heartrate data
  pdat.polar <- pdat %>%
    dplyr::select(polar.hr, polar.timestamp) %>% 
    na.omit()
  
  getPolarHeartRate <- function(dat.polar, hr.timestamp){
    dat.polar <- dat.polar %>% 
      dplyr::mutate(hr.distance.polar = abs(hr.timestamp - polar.timestamp))
    hr.distance.opt.polar <- min(dat.polar$hr.distance.polar)
    dat.polar <- dat.polar %>% dplyr::filter(hr.distance.polar == hr.distance.opt.polar)
    polarHRt <- tryCatch({mean(dat.polar$polar.hr %>% as.numeric())}
                         ,error = function(e){return(NA)})
    return(polarHRt)  
  }
  
  if(nrow(pdat.polar) != 0){
    pdat.polar <- pdat.polar %>%
      dplyr::mutate_at(.vars = c('polar.timestamp'),
                       .funs = as.POSIXct)
    
    # polar.dat <- data.frame(hr15 = getPolarHeartRate(pdat.polar, hr15.timestamp),
    #                          hr30 = getPolarHeartRate(pdat.polar, hr30.timestamp),
    #                          hr60 = getPolarHeartRate(pdat.polar, hr60.timestamp),
    #                          conf15 = NA,
    #                          conf30 = NA,
    #                          conf60 = NA) %>% 
    #   dplyr::mutate(metric = 'polar')
    
    polar.dat <- lapply(seq(15,60), function(itime){ # Only need HR data for times 15-60s post jog
      getPolarHeartRate(pdat.polar, as.POSIXct(pdat$stairStopTime[1]) + itime) %>% 
        as.data.frame() %>% 
        `colnames<-`(c('hr')) %>% 
        dplyr::mutate(time = itime) %>%
        dplyr::mutate(metric = 'polar')
    }) %>% data.table::rbindlist() %>% 
      dplyr::mutate(conf = NA)
    
  }else{
    # polar.dat <- data.frame(hr15 = NA,
    #                          hr30 = NA,
    #                          hr60 = NA,
    #                          conf15 = NA,
    #                          conf30 = NA,
    #                          conf60 = NA) %>% 
    #   dplyr::mutate(metric = 'polar')
    polar.dat <- data.frame(hr = rep(NA, length(seq(15,60))),
                            time = seq(15,60),
                            metric = rep('polar', length(seq(15,60))),
                            conf = rep(NA, length(seq(15,60))))
  }
  
  
  # hr.crf.dat <- hr.crf.dat %>% 
  #   dplyr::mutate_at(.vars = c('hr15','conf15','hr30','conf30','hr60','conf60'),
  #                    .funs = as.numeric)
  # fitbit.dat <- fitbit.dat %>% 
  #   dplyr::mutate_at(.vars = c('hr15','conf15','hr30','conf30','hr60','conf60'),
  #                    .funs = as.numeric)
  # polar.dat <- polar.dat %>% 
  #   dplyr::mutate_at(.vars = c('hr15','conf15','hr30','conf30','hr60','conf60'),
  #                    .funs = as.numeric)
  
  # print(pdat$recordId)
  
  # Pick the hr values that have conf >= 0.5
  hr.crf.dat.minconf <- hr.crf.dat %>% 
    dplyr::filter(conf >= 0.5)
  
  
  dat1530 <- rbind(hr.crf.dat.minconf, fitbit.dat, polar.dat) %>%
    dplyr::filter(time < 31) %>% 
    dplyr::group_by(metric) %>% 
    dplyr::summarise(hb15to30 = 0.25*mean(hr, na.rm = T),
                     conf15 = mean(conf, na.rm = T))
  dat3060 <- rbind(hr.crf.dat.minconf, fitbit.dat, polar.dat) %>%
    dplyr::filter(time > 30) %>% 
    dplyr::group_by(metric) %>% 
    dplyr::summarise(hb30to60 = 0.5*mean(hr, na.rm = T),
                     conf30 = mean(conf, na.rm = T))
  
  dat <- dat1530 %>% dplyr::left_join(dat3060)
  
  dat <- dat %>% dplyr::ungroup() %>% 
    dplyr::mutate(recordId = recordIdIn) %>% 
    dplyr::mutate(externalId = externalIdIn) %>% 
    dplyr::mutate(age = ageIn) %>% 
    dplyr::mutate(gender = genderIn) %>% 
    dplyr::mutate(weight = weightIn) %>% 
    dplyr::mutate(vo2Max.Milligan1 = vo2MaxMilligan1(hb30to60, age = ageIn)) %>% 
    dplyr::mutate(vo2Max.Milligan2 = vo2MaxMilligan2(hb30to60, age = ageIn, gender = genderIn)) %>% 
    dplyr::mutate(vo2Max.Shakey = vo2MaxShakey(hb15to30, weight = weightIn, gender = genderIn))
  
  
  return(dat)
}

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



# ## Refine our data to high confidence scores
# aa.crf <- vo2.estiamtes.tbl %>% 
#   dplyr::left_join(pmi.metadata) %>% 
#   dplyr::filter(conf15 > 0.5, conf30 > 0.5, conf60 > 0.5) %>% 
#   dplyr::select(metric, externalId, age, gender, recordId,
#                 vo2Max.Milligan1, vo2Max.Milligan2,vo2Max.Shakey,
#                 `Measured VO2max (mL/kg/min)`,`Cooper VO2`,inClinic)
# aa.fitbit <- vo2.estiamtes.tbl %>% 
#   dplyr::filter(recordId %in% aa.crf$recordId,
#                 metric == 'fitbit') %>% 
#   dplyr::left_join(pmi.metadata) %>% 
#   dplyr::select(metric, externalId, age, gender, recordId,
#                 vo2Max.Milligan1, vo2Max.Milligan2,vo2Max.Shakey,
#                 `Measured VO2max (mL/kg/min)`,`Cooper VO2`,inClinic) %>% 
#   na.omit()

# Github link
gtToken = 'github_token.txt';
githubr::setGithubToken(as.character(read.table(gtToken)$V1))
thisFileName <- 'crf_vo2.R'
thisRepo <- getRepo(repository = "Sage-Bionetworks/CRF_validation_analysis", ref="branch", refName='master')
thisFile <- getPermlink(repository = thisRepo, repositoryPath=thisFileName)

# Write to Synapse
write.csv(vo2.estiamtes.tbl,file = paste0('tecumesh_vo2_estimates','.csv'),na="")
obj = File(paste0('tecumesh_vo2_estimates','.csv'), 
           name = paste0('tecumesh_vo2_estimates','.csv'), 
           parentId = 'syn12435196')
obj = synStore(obj,  used = all.used.ids, executed = thisFile)
