########################################################################
# CRF Project 
# Purpose: To encode filter parameter values for the CRF algo, so that it can 
#          be ported into another environment, for eg., iOS, android etc.,
# Author: Meghasyam Tummalacherla
# email: meghasyam@sagebase.org
########################################################################
rm(list=ls())
gc()
options(digits.secs = 10)

##############
# Required libraries 
##############
library(tidyverse)
library(synapser)
library(signal)
library(githubr)
synapser::synLogin()

##############
# Required functions 
##############
getFilterCoeff <- function(sampling_rate){
  sampling_rate_rounded <- round(sampling_rate)
  # Filter the signal based on fiters designed
  if(sampling_rate_rounded > 15){
    bf_low <- signal::butter(7, 5/(sampling_rate_rounded/2), type = 'low')
    bf_high <- signal::butter(7, 0.5/(sampling_rate_rounded/2), type = 'high')
  }else{
    bf_low <- signal::butter(7, 4/(sampling_rate_rounded/2), type = 'low')
    bf_high <- signal::butter(7, 0.5/(sampling_rate_rounded/2), type = 'high')
  }
  
  return(list(low = unlist(bf_low),
              high = unlist(bf_high)))
}

getMeanFilterOrder <- function(sampling_rate){
  mean_filter_order <- 65
  if(sampling_rate <= 32){
    mean_filter_order <- 33
  }
  if(sampling_rate <= 18){
    mean_filter_order <- 19
  }
  if(sampling_rate <= 15){
    mean_filter_order <- 15
  }
  return(data.frame(mean_filter_order = mean_filter_order,
                    sampling_rate = sampling_rate))
}

##############
# Calculate the filter parameters for various integer(rounded) values of sampling rates
##############
sampling_rates <- seq(10,65) # the various range of expected sampling rates (rounded)

# Get Lowpass and Highpass filter coefficients for various sampling rates
filter_parameters <- lapply(sampling_rates, function(fs){
  filter_coeff <- getFilterCoeff(fs) %>%
    as.data.frame() %>% t() %>% as.data.frame() %>%
    dplyr::mutate(filter_type = rownames(.),
                  sampling_rate = fs)
  return(filter_coeff)
}) %>% data.table::rbindlist()

lowpass_filter_params <- filter_parameters %>% 
  dplyr::filter(filter_type == 'low')

highpass_filter_params <- filter_parameters %>% 
  dplyr::filter(filter_type == 'high')

# Get Mean centering filter orders
mean_centering_filter_params <- lapply(sampling_rates, getMeanFilterOrder) %>% 
  data.table::rbindlist()

#######################################
# Upload Data to Synapse 
#######################################
# Github link
gtToken = 'github_token.txt';
githubr::setGithubToken(as.character(read.table(gtToken)$V1))
thisFileName <- 'crf_filter_parameters.R'
thisRepo <- getRepo(repository = "itismeghasyam/CRF_validation_analysis", ref="branch", refName='sagebio_master')
thisFile <- getPermlink(repository = thisRepo, repositoryPath=thisFileName)

# Write parameters data to Synapse
# lowpass filter parameters
write.csv(lowpass_filter_params,file = paste0('lowpass_filter_params','.csv'),na="")
obj = File(paste0('lowpass_filter_params','.csv'), 
           name = paste0('lowpass_filter_params','.csv'), 
           parentId = 'syn18497861')
obj = synStore(obj, executed = thisFile)

# highpass filter parameters
write.csv(highpass_filter_params,file = paste0('highpass_filter_params','.csv'),na="")
obj = File(paste0('highpass_filter_params','.csv'), 
           name = paste0('highpass_filter_params','.csv'), 
           parentId = 'syn18497861')
obj = synStore(obj, executed = thisFile)

# mean centering filter parameters
write.csv(mean_centering_filter_params,file = paste0('mean_centering_filter_params','.csv'),na="")
obj = File(paste0('mean_centering_filter_params','.csv'), 
           name = paste0('mean_centering_filter_params','.csv'), 
           parentId = 'syn18497861')
obj = synStore(obj, executed = thisFile)
