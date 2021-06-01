# Heart Snapshot validation analysis
This project contains code and results from our manuscript titled 'Smartphone-Based VO2max Measurement With Heart Snapshot in Clinical and Real-world Settings With a Diverse Population: Validation Study'. VO2max is a measure of cardiorespiratory fitness (CRF), and thus the original repository name maintains that nomenclature.

See: https://www.synapse.org/CRF_module for data and output

## crf_extractHR.R
Run this file to extract features for all PMI healthcodes for the activities: 

1. Cardio 12MT-v5 ('syn11665074', the 12 minute run test)
2. Cardio Stress Test-v1 ('syn11580624', the stress test)
3. Cardio Stair Step-v1 ('syn11432994', the stair test)

Uncomment the activity for which you want to run the analysis for. 
The results are stored in the Features folder ('syn11968320') of the CRF_ModuleProject 

## crf_analysis.Rmd
Run this file to extract summary parameters of the algorithm's performance on the HR recordings.
The summary parameters include Confidence values, R2, absolute error etc.,
