## Merging the Polar CSV data files

# Download all the polar files intothe current directory and run this code to generate a data frame 
# with all the polar data merged together

# Make sure you have only this file and the polar csv files in the current directory

rm(list=ls())
gc()

# Required libraries
library(dplyr)
library(synapseClient)

# Synapse Login
synapseClient::synapseLogin()

## Download ref (crf) table (needed for createdOnTimeZone)

## Reference Table
# ref.tableId = 'syn11665074'
# ref.name = 'Cardio 12MT-v5'

# ref.tableId = 'syn11580624'
# ref.name = 'Cardio Stress Test-v1'

ref.tableId = 'syn11432994'
ref.name = 'Cardio Stair Step-v1'

ref.tbl <- synTableQuery(paste('select * from', ref.tableId))@values
ref.tbl <- ref.tbl %>% dplyr::select(recordId, healthCode, externalId, createdOn, createdOnTimeZone) %>% 
  dplyr::mutate(createdDate = as.character(as.Date.character(createdOn))) %>% unique()

all.used.ids = ref.tableId

# HealthCode <-> TimeZone data
hc.timezone.tbl <- ref.tbl %>% 
  dplyr::select(healthCode, createdOnTimeZone, externalId) %>%
  unique()

# Let us first consider healthCodes that have only one time zone, we will deal with healthCode having
# multiple timezones later

a <- hc.timezone.tbl %>%
  dplyr::group_by(healthCode) %>% 
  dplyr::count() %>% 
  dplyr::filter(n == 1)

b <- hc.timezone.tbl %>%
  dplyr::group_by(healthCode) %>% 
  dplyr::count() %>% 
  dplyr::filter(n > 1)

# Subset healthCodes to healthCodes in one timezone
hc.timezone.tbl.monoTimeZone  <- hc.timezone.tbl %>%
  dplyr::filter(healthCode %in% a$healthCode) %>% 
  unique()

# Subset healthCodes to healthCodes in multiple timezones
hc.timezone.tbl.multTimeZone <- ref.tbl %>%
  dplyr::select(healthCode, createdOnTimeZone, createdDate, externalId) %>% 
  dplyr::filter(healthCode %in% b$healthCode) %>% 
  unique() %>% 
  dplyr::rename('date' = 'createdDate')

all.files <- list.files()
all.files <- all.files[grep('PMI',all.files)]

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
    dplyr::mutate(externalId = substring(file.i,1,6))
  polar_data <- rbind(polar_data, cbind(a1,a2)) 
}

# Keep a copy of original polar data
polar_data_orig <- data.table::copy(polar_data)

# Change date format in polar data to ymd from dmy
polar_data$date <- as.character(as.Date.character(polar_data$date, format = '%d-%m-%Y'))

# Get merged data for healthCodes in single timezone and multiple timezones
polar_data_monoTimeZone <- polar_data %>% dplyr::inner_join(hc.timezone.tbl.monoTimeZone)
polar_data_multTimeZone <- polar_data %>% dplyr::inner_join(hc.timezone.tbl.multTimeZone)

# Get the updated polar data that has timezone
polar_data <- rbind(polar_data_monoTimeZone, polar_data_multTimeZone)

# Correct the times for the timezone
polar_data$start.timestamp <- apply(polar_data[,c('date','start.time')],1,paste,collapse ='')
polar_data$start.timestamp <- strptime(polar_data$start.timestamp, format = '%Y-%m-%d %H:%M:%S')
polar_data$timestamp <- polar_data$start.timestamp + (as.numeric(polar_data$time)-1)
polar_data$timestamp <- polar_data$timestamp - 60*60*as.numeric(polar_data$createdOnTimeZone)/100
polar_data$start.timestamp <- polar_data$start.timestamp - 60*60*as.numeric(polar_data$createdOnTimeZone)/100

# Upload to Synapse
write.csv(polar_data,file = paste0('polar ', ref.name,'.csv'),na="")
obj = File(paste0('polar ', ref.name, '.csv'), 
           name = paste0('polar', ref.name, '.csv'), 
           parentId = 'syn16805789')
obj = synStore(obj,  used = c(all.used.ids,'syn16805791'))


