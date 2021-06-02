![heartSnapshotCRFappIcon](https://user-images.githubusercontent.com/3682103/120533529-aaeaf380-c395-11eb-93d7-521a6882df30.png)

# Heart Snapshot validation analysis
This project contains code and results from our manuscript titled 'Smartphone-Based VO2max Measurement With Heart Snapshot in Clinical and Real-world Settings With a Diverse Population: Validation Study' (Preprint available [here](https://www.biorxiv.org/content/10.1101/2020.07.02.185314v1) until publication in JMU). VO2max is a measure of cardiorespiratory fitness (CRF), and thus the original repository name maintains that nomenclature.

# Data availability
Data from this study is available at the [dHealth Digital Health Knowledge Portal](https://dhealth.synapse.org/)

# Smartphone apps
The code and demo apps for the [iOS](https://github.com/Sage-Bionetworks/CardiorespiratoryFitness-iOS) and [Android](https://github.com/Sage-Bionetworks/CardiorespiratoryFitness-Android) versions of Heart Snapshot are also publicly available.

## crf_extractHR.R
Run this file to extract features for all participantIDs for the activities: 

1. Cardio 12MT-v5 or 12-MRT ('syn11665074', the 12 minute run test)
2. Cardio Stress Test-v1 or gold standard treadmill-based test ('syn11580624', the stress test)
3. Cardio Stair Step-v1 or 3-MST ('syn11432994', the 3-minute step test)

Uncomment the activity for which you want to run the analysis. 
The results are stored in the Features folder ('syn11968320') of the CRF_ModuleProject 

## crf_analysis.Rmd
Run this file to extract summary parameters of the algorithm's performance on the heart rate recordings.
The summary parameters include confidence values, R2, absolute error etc.,
