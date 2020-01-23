###############################################################################################
### Data format #7
###############################################################################################
# All the files are of dataformat #7, a short overview is presented below
# A frame consists of 5 BYTES, a packet consists of 25 frames and three packets (75 frames) 
# are transmitted each second

###############################################################################################
### Definition of each byte in dataformat #7
###############################################################################################
# BYTE1 - Status byte (all frames)
# BYTE2 - Pleth MSB (all frames)
# BYTE3 - Pleth LSB (all frames)
# BYTE4 - [PR MSB, PR LSB, SpO2, SREV, reserved, reserved, reserved] - (1,2,3,4,5,6,7) frames
# BYTE4 - [STAT2, SpO2-D, SpO2 Fast, SpO2 B-B, reserved, reserved] - (8,9,10,11,12,13) frames
# BYTE4 - [E-PR MSB, E-PR LSB, E-SpO2, E-SpO2-D, reserved, reserved] - (14,15,16,17,18,19) frames
# BYTE4 - [PR-D MSB, PR-D LSB, E-PR-D MSB, E-PR-D LSB, reserved, reserved] - (20,21,22,23,24,25) frames
# BYTE5 - Check byte (all frames) 

###############################################################################################
### Elaborate specifics of each byte (in bits)
###############################################################################################
## BYTE1 - Always 128 or greater (128-255 range)
## BYTE1 - [BIT7, BIT6, BIT5, BIT4, BIT3, BIT2, BIT1, BIT0] (All bits are active high)
# BIT7 - 1 (Always)
# BIT6 - SNSD; Sensor Disconnect; Sensor is not connected to oximeter or sensor is inoperable.
# BIT5 - ARTF; Artifact; Indicates artifact condition of each pulse (occurs only during pulse).
# BIT4 - OOT; Out Of Track; An absence of consecutive good pulse signals.
# BIT3 - SNSA; Sensor Alarm; Device is providing unusable data for analysis.
# BIT2 and BIT1 - YPRF, RPRF, GPRF; 
#               - Yellow perfusion, Red perfusion, Green perfusion;
#               - Amplitude representation of low/marginal signal quality (YPRF)
#                 Amplitude representation of low/no pulse signal (RPRF)
#                 Amplitude representation of high signal quality (GPRF)
#               - (BIT2, BIT1):
#                 (1,1) - Yellow - low/marginal pulse signal
#                 (1,0) - Red - low/no pulse signal
#                 (0,1) - Green - high pulse signal
# * the oximeter reports each pulse by setting/clearing the RPRF and GPRF bits for a period of 12 frames (160ms)
# BIT0 - SYNC; Frame Sync; = 1 in Frame 1 (=0 in frames 2 through 25)

## BYTE2 and BYTE3 - Pleth Byte
## Range : (0-65535) (MSB:LSB), individually each byte has a range (0-255)
## BYTE2 - MSB; BYTE3 - LSB

## BYTE4 - Always 127 or less (0-127 range)
## BYTE4 - Used for SpO2, Pulse Rate, and information that can be processed at a rate of 1/3 of a second (3 packets in a second)
# BYTE4 - SREV, Oximeter Firmware Revision level
#         STAT2, Status Byte (occurs 1 of 25, in frame 8) 
#                [BIT7, BIT6, BIT5, BIT4, BIT3, BIT2, BIT1, BIT0]
#                [0,   R,    SPA,  R,    R,    R,    R,    R]
#                SPA - High Quality SmartPoint Measurement (Only valid if SREV >= 16)
#                SPA - SmartPoint Algorithm qualifies the data for recording purposes and eliminates the guesswork
#                      of determining when the patient measurement is qualified for recording purposes. When the
#                      SPA indicates the reading is high quality, the SPA bit will be set
#                R - Reserved, for future use
# BYTE4 - Standard Mode:  
#         PR: 4-beat Pulse Rate Average
#         E-PR: 8-beat Pulse Rate Extended Average
#         SpO2: 4-beat SpO2 Average
#         E-SpO2: 8-beat SpO2 Average
#         SpO2 Fast: 4-beat Average optimized for fast responding
#         SpO2 B-B: Beat to Beat value - No Average
# BYTE4 - Display Mode:
#         PR-D: 4-beat Pulse Rate Average
#         E-PR-D: 8-beat Pulse Rate Extended Average
#         SpO2-D: 4-beat SpO2 Average
#         E-SpO2-D: 8-beat SpO2 Average
# BYTE4 - Error handling:
#         When SpO2 and PR cannot be computed, the system will send a missing data indicator. For missing data,
#         the PR equals 511 and the SpO2 equals 127. The missing data could be because of 
#         1. Sensor is positioned improperly
#         2. Sensor was removed prior to a reading
#         3. Signal at the sensor site is not discernable, Warm the site or choose a different site
# BYTE4 - PR format: (PRk- Pulse Rate bit k, R- Reserved)
#         PR MSB - [BIT7, BIT6, BIT5, BIT4, BIT3, BIT2, BIT1, BIT0]
#                - [0,    R,    R,    R,    R,    R,    PR8,  PR7]
#         PR LSB - [BIT7, BIT6, BIT5, BIT4, BIT3, BIT2, BIT1, BIT0]
#                - [0,    PR6,  PR5,  PR4,  PR3,  PR2,  PR1,  PR0]
# BYTE4 - SpO2 format: (SPk - SpO2 bit k)
#         SpO2 - [BIT7, BIT6, BIT5, BIT4, BIT3, BIT2, BIT1, BIT0]
#              - [0,    SP6,  SP5,  SP4,  SP3,  SP2,  SP1,  SP0]

## BYTE5 - Can be any number between 0 and 255
## BYTE5 - Used for checksum of bytes 1 through 4
## BYTE5 - Range (0-255)
#        - CHK : Checksum = (BYTE1 + BYTE2 + BYTE3 + BYTE4) modulo 256


###############################################################################################
### Required Processing Sub - Functions
###############################################################################################
# Function to extract color from bits 2 and 1 of byte1 of the nonin output
whatColor <- function(BITS2and1){
  # A string '00' or '01' or '10' or '11' of BITS 2 and 1 of BYTE1 (Status BYTE) of Nonin Dataformat #7
  strBits <- paste0(BITS2and1, collapse = '')
  if(strBits == '00'){
    return('None')
  }else if(strBits == '01'){
    return('Green')
  }else if(strBits == '10'){
    return('Red')
  }else if(strBits == '11'){
    return('Yellow')
  }else{
    return('ERROR')
  }
}

whatTag <- function(frameNo){
  # Given Frame number decide what tag to assign for BYTE4
  # BYTE4 - [PR MSB, PR LSB, SpO2, SREV, reserved, reserved, reserved] - (1,2,3,4,5,6,7) frames
  # BYTE4 - [STAT2, SpO2-D, SpO2 Fast, SpO2 B-B, reserved, reserved] - (8,9,10,11,12,13) frames
  # BYTE4 - [E-PR MSB, E-PR LSB, E-SpO2, E-SpO2-D, reserved, reserved] - (14,15,16,17,18,19) frames
  # BYTE4 - [PR-D MSB, PR-D LSB, E-PR-D MSB, E-PR-D LSB, reserved, reserved] - (20,21,22,23,24,25) frames
  frameNo <- as.character(frameNo)
  frameTag <- switch(frameNo,
                     '1' = 'PR MSB',
                     '2' = 'PR LSB',
                     '3' = 'SpO2',
                     '4' = 'SREV',
                     '5' = 'reserved',
                     '6' = 'reserved',
                     '7' = 'reserved',
                     '8' = 'STAT2',
                     '9' = 'SpO2-D',
                     '10' = 'SpO2 Fast',
                     '11' = 'SpO2 B-B',
                     '12' = 'reserved',
                     '13' = 'reserved',
                     '14' = 'E-PR MSB',
                     '15' = 'E-PR LSB',
                     '16' = 'E-SpO2',
                     '17' = 'E-SpO2-D',
                     '18' = 'reserved',
                     '19' = 'reserved',
                     '20' = 'PR-D MSB',
                     '21' = 'PR-D LSB',
                     '22' = 'E-PR-D MSB',
                     '23' = 'E-PR-D LSB',
                     '24' = 'reserved',
                     '25' = 'reserved')
  return(frameTag)
}

buildTimeStamp <- function(TIME, TimeStampFrame){
  # TIME is in seconds like 09:33:59, TimeStampFrame is a number from 1-75
  # We will convert TimeStampFrame into milliseconds, and append it to TIME
  # We will have 3 digit precision for the milliseconds, i.e 09:33:59.200
  # Frame 1 ~ 0 milliseconds, Frame 2 ~ 1/75 milliseconds and so on....
  TimeStampFrame = round((TimeStampFrame-1)/75, 3) %>% 
    as.character() %>% 
    strsplit(.,'') %>% 
    unlist()
  if(length(TimeStampFrame) < 5){
    # length 5 because: '0', '.', '*', '*', '*',
    TimeStampFrame[(length(TimeStampFrame)+1):5] = 0
  } 
  
  TIME.MILLI = paste0(TimeStampFrame[3:5], collapse = '')
  TIME.NEW = paste0(TIME, '.', TIME.MILLI)
  return(TIME.NEW)
}
###############################################################################################
### Code to read the csv file in dataformat #7 and demystify (process) it
###############################################################################################
# Load required libraries
# library(tidyverse)


#### MAIN FUNCTION ####
noninRead <- function(nonin.path){

## Load the file
nonin <- read.csv(as.character(nonin.path), stringsAsFactors = F) %>% 
  dplyr::select(-X,-X.1) # For the two empty columns between BYTE5 and SpO2

## Checksum (BYTE5) and Pleth waveform (BYTE2,3 (MSB,LSB respectively))
# Get CHK sum (sum of BYTES 1-4, and cross check with BYTE5 to see if they match, then replace those columns
# with a variable CHK, CHK is 0 is everything is valid, non-zero if checksum fails)
# Plethysmograph waveform is already provided as PLETHY(= 256*BYTE2 + BYTE3), so we will just use that
nonin <- nonin %>% 
  dplyr::mutate(CHKSUM = (BYTE1 + BYTE2 + BYTE3 + BYTE4) %% 256) %>% 
  dplyr::mutate(CHK = abs(CHKSUM - BYTE5)) %>% 
  dplyr::select(-CHKSUM, -BYTE5, -BYTE2, -BYTE3) 

## Status byte processing (BYTE1)
nonin <- nonin %>% 
  dplyr::mutate(BYTE1.bin = strsplit(R.utils::intToBin(BYTE1),'')) %>% 
  dplyr::mutate(SNSD = NA) %>% #BIT 6
  dplyr::mutate(ARTF = NA) %>% #BIT 5
  dplyr::mutate(OOF = NA) %>% #BIT 4
  dplyr::mutate(SNSA = NA) %>% #BIT 3
  dplyr::mutate(COLOR = NA) %>%  #BITS 2 and 1
  dplyr::mutate(SYNC = 0) 

# Process Multiple signals (BYTE4)
nonin <- nonin %>% 
  dplyr::mutate(TAG = NA)

# Correct the timestamp to milliseconds from seconds, using TIME and FRAME
# I assume that the moment a new second value of time starts i,e 9:33:01 to say 9:33:02, the first frame 
# that touches 9:33:02 will be recorded as 9:33:02 and the next frame is 1/75 seconds away from this, and we
# will work backwords to fill the timestamps before such an occurrence
nonin <- nonin %>% 
  dplyr::mutate(TimeStampFrame = NA) %>% 
  dplyr::mutate(TIMESTAMP = NA)

# Find that row (refFrame) that we can assume to be a reference to build a timestamp
# We are bound to find a row in the first 76 frames since the sampling rate is 75 FPS
for (refFrame in seq(75)){
  if(nonin$TIME[refFrame] != nonin$TIME[refFrame+1]){
    break
  }
}

# Fix the TimeStampFrame reference as 1 for the refFrame
nonin$TimeStampFrame[refFrame+1] = 1

# Build the TimeStampFrame from the reference frame onwards
for (i in seq(refFrame+2, nrow(nonin))){
  if(nonin$TimeStampFrame[i-1] < 75){
    nonin$TimeStampFrame[i] = nonin$TimeStampFrame[i-1] + 1
  }else{
    nonin$TimeStampFrame[i] = 1
  }
}

# Build the TimeStampFrame from the reference frame backwards
for (i in seq(refFrame, 1, -1)){
  if(nonin$TimeStampFrame[i+1] > 1){
    nonin$TimeStampFrame[i] = nonin$TimeStampFrame[i+1] - 1
  }else{
    nonin$TimeStampFrame[i] = 75
  }
}

# Update the Status BYTE Bits based on the extracted bits
# Also in the loop update CHKSumDiff
# Also update Tag for BYTE4
# Also Build a TIMESTAMP using TIME and TimeStampFrame
for (i in seq(nrow(nonin))){
  nonin$SNSD[i] = as.logical(as.numeric(nonin$BYTE1.bin[[i]][2]))
  nonin$ARTF[i] = as.logical(as.numeric(nonin$BYTE1.bin[[i]][3]))
  nonin$OOF[i] = as.logical(as.numeric(nonin$BYTE1.bin[[i]][4]))
  nonin$SNSA[i] = as.logical(as.numeric(nonin$BYTE1.bin[[i]][5]))
  nonin$COLOR[i] = whatColor(nonin$BYTE1.bin[[i]][6:7])
  nonin$SYNC[i] = as.logical(as.numeric(nonin$BYTE1.bin[[i]][8]))
  nonin$TAG[i] = whatTag(nonin$FRAME[i])
  nonin$TIMESTAMP[i] = buildTimeStamp(nonin$TIME[i], nonin$TimeStampFrame[i])
  if(nonin$CHK[i] == 0){
    nonin$CHK[i] = TRUE
  }else{
    nonin$CHK[i] = FALSE
  }
}

# Remove columns related to unprocessed BYTE1 data, i.e BYTE1 and BYTE1.bin
# Rename BYTE4 to TAG.VALUE
nonin <- nonin %>% 
  dplyr::select(-BYTE1, -BYTE1.bin) %>% 
  dplyr::rename(TAG.VALUE = BYTE4,
                ChkSumValid = CHK) %>% 
  dplyr::select(-TIME, -FRAME, -TimeStampFrame, -SYNC)

# No need for time since, we can get that using TIMESTAMP
# No need for FRAME or SYNC since we can get that using TAG (BYTE4 is indexed according to FRAME), 
# and SYNC is just FRAME == 1
# No need for TimeStampFrame since we can also get that using TIMESTAMP]

nonin$ChkSumValid <- as.logical(nonin$ChkSumValid)

# Reorder columns so that it reads well
nonin <- nonin[, c(12,3,4,2,11,1,5,6,7,8,9,10)]

return(nonin)
}
