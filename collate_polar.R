## Merging the Polar CSV data files

# Download all the polar files intothe current directory and run this code to generate a data frame 
# with all the polar data merged together

# Make sure you have only this file and the polar csv files in the current directory

# Required libraries
library(dplyr)
library(synapser)

rm(list=ls())
gc()

all.files <- list.files()
all.files <- all.files[grep('.csv',all.files)]

polar_data <- NULL

for(file.i in all.files){
  a1 <- read.csv(file.i, skip = 2) %>% 
    dplyr::select('Time', 'HR..bpm.') %>% 
    dplyr::rename('time' = 'Time',
                  'hr' = 'HR..bpm.')
    
  a2 <- read.csv(file.i, nrows = 1) %>% 
    dplyr::select('Sport', 'Date', 'Start.time', 'Duration') %>% 
    dplyr::select('sport' = 'Sport',
                  'date' = 'Date',
                  'start.time' = 'Start.time',
                  'duration' = 'Duration') %>% 
    dplyr::mutate(externalId = substring(file.i,4,6)) 
  polar_data <- rbind(polar_data, cbind(a1,a2))
}


# Upload to Synapse
synapser::synLogin()
write.csv(polar_data,file = paste0('polar','.csv'),na="")
obj = File(paste0('polar','.csv'), 
           name = paste0('polar','.csv'), 
           parentId = 'syn16805789')
obj = synStore(obj,  used = 'syn16805791')


