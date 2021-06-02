![heartSnapshotCRFappIcon](https://user-images.githubusercontent.com/3682103/120533529-aaeaf380-c395-11eb-93d7-521a6882df30.png)

# Heart Snapshot validation analysis
This project contains code and results from our manuscript titled 'Smartphone-Based VO2max Measurement With Heart Snapshot in Clinical and Real-world Settings With a Diverse Population: Validation Study' (Preprint available [here](https://www.biorxiv.org/content/10.1101/2020.07.02.185314v1) until publication in JMU). VO2max is a measure of cardiorespiratory fitness (CRF), and thus the original repository name maintains that nomenclature.

# Data availability
Data from this study is available at the [dHealth Digital Health Knowledge Portal](https://dhealth.synapse.org/)

# Smartphone apps
The code and demo apps for the [iOS](https://github.com/Sage-Bionetworks/CardiorespiratoryFitness-iOS) and [Android](https://github.com/Sage-Bionetworks/CardiorespiratoryFitness-Android) versions of Heart Snapshot are also publicly available.

# File Descriptions
## Feature Extraction
All output files are stored in the Featues folder ('syn22268519'). The output files for crf_polar.R are stored in the Polar Data folder ('syn22125127').

### crf_extractHR.R
Run this file to extract features for all participantIDs for the activities: 

1. Cardio 12MT or 12-MRT ('syn22254983', the 12 minute run test)
2. Cardio Stress Test or gold standard treadmill-based test ('syn22119505', the stress test)
3. Cardio Stair Step or 3-MST ('syn22254980', the 3-minute step test)

### crf_fitbit.R
Run this file to extract features on Fitbit Heart rate Intraday data ('syn22254943') for contributing participants

### crf_nonin.R
Run this file to extract the heart rate data from Nonin pulseOx ('syn22268058') for the Calibration study

### crf_polar.R
Run this file to extract the heart rate data from the Polar chest strap ('syn22125134')

### noninRead.R
File to parse output data from Nonin PulseOx into a digestible format

## Analysis

### crf_stairStep_start_stop.R
Run this file to find out start and stop times for step test using stair step accelerometer / gyrscope data from 3-MST ('syn22254980'). The output files are stores in Analysis folder ('syn22268520')

### crf_mergeCompare.R
Run this file to  merge fitbit, polar and crf app data for all participantIDs for the follwing activities. The output files are stored in the Featues folder ('syn22268519').

1. Cardio 12MT or 12-MRT ('syn22254983', the 12 minute run test)
2. Cardio Stress Test or gold standard treadmill-based test ('syn22119505', the stress test)
3. Cardio Stair Step or 3-MST ('syn22254980', the 3-minute step test)

### crf_vo2.R
Run this file to estimate V02 max after completing the Tecumseh Stair Step test (3-MST) in CRF Module. Run this file after you have completed running all the feature extraction files and the rest of the analysis files (except crf_nonin_metrics.R). The results are stored in the Analysis folder ('syn22268520')


