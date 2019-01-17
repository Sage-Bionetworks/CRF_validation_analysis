########################################################################
# CRF Project 
# Purpose: To extract and integrate CRF Nonin data (Done at UCSD)
# Author: Meghasyam Tummalacherla
# email: meghasyam@sagebase.org
########################################################################

rm(list=ls())
gc()
devtools::install_github("Sage-Bionetworks/mhealthtools")
source('noninRead.R')

##############
# Required libraries
##############
library(data.table)
library(tidyr)
library(plyr)
library(dplyr)
library(seewave)
library(mhealthtools) 
library(synapseClient)
library(githubr)
library(ggplot2)
library(parsedate)
library(lubridate)

synapseLogin()

##############
# Required functions
##############
noninReadError <- function(phone_){
  return(
    data.frame(
      TIMESTAMP = NA,
      HR.D = NA,
      PLETHY = NA,
      SpO2.D = NA,
      TAG = NA,
      TAG.VALUE = NA,
      ChkSumValid = NA,
      SNSD = NA,
      ARTF = NA,
      OOF = NA,
      SNSA = NA,
      COLOR = NA,
      phone = phone_
    )
  )
}

#######################################
# Download Synapse Table, and select and download required columns, figure out filepath locations
#######################################

######
## CRF Validation Data (Nonin and the SpectroColorimeter readings)
######
tableId = 'syn17009128'
name = 'CRF_HR_validation_questions'

all.used.ids = tableId
columnsToDownload = c('iPhone SE Nonin File','iPhone 8+ Nonin file','iPhone XS Nonin File',
                      'Samsung Galaxy J7 Nonin File','Moto G6 Play Nonin File',
                      'Huawei Mate SE Nonin File','LG Stylo 4 Nonin File',
                      'Samsung Galaxy S9+ Nonin File') # For Cardio 12MT
crf.validation.tbl = synTableQuery(paste('select * from', tableId))
crf.validation.tbl@values <- crf.validation.tbl@values %>%
  dplyr::filter(`Participant ID` > 1200)

nonin.json.loc = lapply(columnsToDownload, function(col.name){
  tbl.files = synDownloadTableColumns(crf.validation.tbl, col.name) %>%
    lapply(function(x) data.frame(V1 = x)) %>% 
    data.table::rbindlist(idcol = col.name) %>% 
    plyr::rename(c('V1' = gsub('Nonin','fileLocation', col.name)))
})

crf.validation.table.meta = data.table::rbindlist(list(crf.validation.tbl@values %>%
                                                         left_join(do.call(cbind, nonin.json.loc[1]))),
                                                  use.names = T, fill = T)%>%as.data.frame

for(i in seq(2,length(columnsToDownload))){
  crf.validation.table.meta = data.table::rbindlist(list(crf.validation.table.meta %>%
                                                           left_join(do.call(cbind, nonin.json.loc[i]))),
                                                    use.names = T, fill = T) %>% as.data.frame()
}

######
## Phone Camera Json data
######
tableId = 'syn17007713'
name = 'HeartRate Measurement-v8'

all.used.ids <- c(all.used.ids, tableId)
hr.tbl <- synTableQuery(paste("select * from ", tableId))
hr.tbl@values <- hr.tbl@values %>%
  dplyr::filter(answers.participantID %in% crf.validation.table.meta$`Participant ID`) %>% 
  dplyr::filter(answers.participantID > 1200) # our participantsIDs are 12xx, 34xx and 56xx

columnsToDownload = c('rawData')
columnsToSelect = c('recordId', 'healthCode','rawData','phoneInfo','createdOn', 'createdOnTimeZone') 

hr.json.loc = lapply(columnsToDownload, function(col.name){
  tbl.files = synDownloadTableColumns(hr.tbl, col.name) %>%
    lapply(function(x) data.frame(V1 = x)) %>% 
    data.table::rbindlist(idcol = col.name) %>% 
    plyr::rename(c('V1' = gsub('Data','.fileLocation', col.name)))
})

hr.table.meta = data.table::rbindlist(list(hr.tbl@values %>%
                                             left_join(do.call(cbind, hr.json.loc[1]))),
                                      use.names = T, fill = T)%>%as.data.frame



#######################################
# Extract Nonin Data for each participant and Collate all to form one Nonin data file 
#######################################
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
      dat.iPhone8Plus <- tryCatch({noninRead(as.character(x['iPhone 8+ fileLocation File'])) %>% 
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
        dplyr::mutate(participantID = x['Participant ID']) %>% 
        dplyr::select(TIMESTAMP, HR.D, phone, participantID) %>% 
        dplyr::rename(timestamp = TIMESTAMP,
                      noninHR = HR.D)
    },
    error = function(e){ NA }
  )
}) %>% plyr::ldply(data.frame)

#######################################
# Upload Nonin Data to Synapse 
#######################################
# Github link
gtToken = 'github_token.txt';
githubr::setGithubToken(as.character(read.table(gtToken)$V1))
thisFileName <- 'crf_nonin.R'
thisRepo <- getRepo(repository = "itismeghasyam/CRF_validation_analysis", ref="branch", refName='master')
thisFile <- getPermlink(repository = thisRepo, repositoryPath=thisFileName)

# Write to Synapse
write.csv(nonin.hr.tbl,file = paste0('nonin','.csv'),na="")
obj = File(paste0('nonin','.csv'), 
           name = paste0('nonin','.csv'), 
           parentId = 'syn11968320')
obj = synStore(obj,  used = all.used.ids, executed = thisFile)

