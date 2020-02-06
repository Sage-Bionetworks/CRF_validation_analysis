########################################################################
# CRF Project 
# Purpose: To test different ways of combining Red, Green and Blue
#          channel HR values to get an estimated HR value per time window
# Author: Meghasyam Tummalacherla
# email: meghasyam@sagebase.org
########################################################################
rm(list = ls())
gc()

##############
# Required libraries 
##############
library(tidyverse)
library(signal)
library(mhealthtools)
# devtools::install_github('itismeghasyam/mhealthtools@crfAppVersion')
library(synapser)
library(githubr)
# devtools::install_github("brian-bot/githubr")

##############
# Data
##############
synapser::synLogin()

# Download Data from Synapser
est.fitz.tbl <- read.csv(synGet('syn18082209')$path, header = T, stringsAsFactors = F) %>% 
  dplyr::select(-X)
all.used.ids <- 'syn18082209'

merged.tbl <- read.csv(synGet('syn17973172')$path, header = T, stringsAsFactors = F) %>% 
  dplyr::select(-X)
all.used.ids <- c(all.used.ids, 'syn17973172')

# Convert participantID columns into character format
merged.tbl$participantID <- as.character(merged.tbl$participantID)
est.fitz.tbl$`Participant.ID` <- as.character(est.fitz.tbl$`Participant.ID`)

# Merge both dataframes
aa <- merged.tbl %>%
  dplyr::left_join(est.fitz.tbl %>%
                     dplyr::rename(participantID = `Participant.ID`))

# Filter values to those that contain Nonin HR and have a postive confidence
aa <- aa[!is.na(aa$noninHR),] %>%
  dplyr::filter(method == 'acf',
                redConf > 0,
                greenConf > 0,
                blueConf > 0)

total_rows <- nrow(aa)
total_participant <- length(unique(aa$participantID))

##############
# Get an estimated HR, which is a composite of Red, Green and Blue channels
# (Test zone for algorithms for estimated HR)
##############

aa$window <- as.numeric(gsub('Window','',aa$window))
aa$estHR <- NA

for(i in seq(total_rows)){
  
  hr_vec <- as.numeric(c(aa$redHR[i], aa$greenHR[i], aa$blueHR[i]))
  hr_vec[hr_vec > 240] <- NA
  hr_vec[hr_vec < 44] <- NA
  na_index <- is.na(hr_vec)
  hr_vec <- na.omit(hr_vec)
  conf_vec <- as.numeric(c(aa$redConf[i], aa$greenConf[i], aa$blueConf[i]))
  conf_vec <- conf_vec[!na_index]
  
  max_conf_hr <- hr_vec[which.max(conf_vec)]
  max_conf <- max(conf_vec)
  
  
  # est_hr <- as.numeric(hr_vec[which.max(conf_vec)])
  # est_conf <- max(conf_vec)
  # est_hr <- hr_vec*conf_vec/conf_vec
 #  if(length(hr_vec) == 3){
 #    diff_vec <- abs(c(hr_vec[2]-hr_vec[1],
 #                      hr_vec[3]-hr_vec[2],
 #                      hr_vec[1]-hr_vec[3]))
 #    est_hr_vec <- c(hr_vec[1:2][which.max(conf_vec[1:2])],
 #                    hr_vec[2:3][which.max(conf_vec[2:3])],
 #                    hr_vec[-2][which.max(conf_vec[-2])])
 #    est_conf_vec <- c(which.max(conf_vec[1:2]),
 #                      which.max(conf_vec[2:3]),
 #                      which.max(conf_vec[-2]))
 #    non_considered_hr <- c(hr_vec[3], hr_vec[1], hr_vec[2])
 #    non_considered_conf <- c(conf_vec[3], conf_vec[1], conf_vec[2])
 #    tag <- which.min(diff_vec)
 #    non_considered_hr <- non_considered_hr[tag]
 #    non_considered_conf <- non_considered_conf[tag]
 #    # 1 mapped to 3, 1 to 2, 2 to 3 in
 #    # non_considered_hr index position to
 #    # hr_vec index position
 #    est_hr <- est_hr_vec[tag]
 #    est_conf <- est_conf_vec[tag]
 # }
 #  
 # if(length(hr_vec) == 2){
 #   est_hr <- as.numeric(hr_vec[which.max(conf_vec)])
 #   est_conf <- max(conf_vec)
 #   non_considered_hr <- as.numeric(hr_vec[which.min(conf_vec)])
 #   non_considered_conf <- min(conf_vec)
 # }
 #  
 #  if(length(hr_vec) == 1){
 #    est_hr <- hr_vec
 #    non_considered_hr <- hr_vec
 #    est_conf <- conf_vec
 #    non_considered_conf <- conf_vec
 #  }  
  
  mean_hr <- sum(hr_vec*conf_vec)/sum(conf_vec)
  diff_vec <- hr_vec - mean_hr
  tag_hr <- which.min(abs(diff_vec))
  est_hr <- hr_vec[tag_hr]
  est_conf <- conf_vec[tag_hr]

  # tag_non <- which.max(abs(diff_vec))
  # non_considered_hr <- hr_vec[tag_non]
  # non_considered_conf <- conf_vec[tag_non]
  # 
  # ratio_ <- est_hr/non_considered_hr
  # 
  # if(est_hr < 90 & ratio_ < 0.55 &
  #    est_conf > 0.35 & (abs(est_conf - non_considered_conf)<0.3)){
  #   est_hr <- non_considered_hr
  #   est_conf <- non_considered_conf
  # }
  # 
  # if(est_hr > 90 & ratio_ > 1.6 &
  #    est_conf > 0.35 & (abs(est_conf - non_considered_conf)<0.3)){
  #   est_hr <- non_considered_hr
  #   est_conf <- non_considered_conf
  # }
  # 
  # if(max_conf/est_conf > 2){
  #   est_conf <- max_conf
  #   est_hr <- max_conf_hr
  # }
  
  
  if(is.numeric(est_hr) & !purrr::is_empty(est_hr)){
    aa$estHR[i] <- est_hr
    aa$estConf[i] <- est_conf
  }
  
  # if(i > 1){
  #   if(aa$participantID[i] == aa$participantID[i-1]){
  #     time_error <- aa$estHR[i] - aa$estHR[i-1]
  #     if((abs(time_error) > 20) && aa$estConf[i] < 0.4){
  #       aa$estHR[i] <- aa$estHR[i-1]
  #       aa$estConf[i] <- aa$estConf[i-1]
  #     }
  #   }
  # }
}

aa_all <- data.table::copy(aa)
  
# Further filtering to retain rows that contain finite values of estimated HR
aa <- aa[!is.infinite(aa$estHR),] %>%
  dplyr::filter(method == 'acf',
                redConf > 0,
                greenConf > 0,
                blueConf > 0,
                estConf > 0)

# Add error column based on estimatedHR and NoninHR (pulse ox)
aa$error <- aa$estHR - as.numeric(aa$noninHR)

percent_retain <- nrow(aa)/total_rows*100
percent_participant <- 100*length(unique(aa$participantID))/total_participant

# print to see how many records and participants we are retaining
percent_retain
percent_participant

##############
# Plots and comparison of estimatedHR and NoninHR (pulse ox)
##############
# Summarize error metrics grouped by face fitzpatrick scales (1-6)
aa_est_face <- aa %>% 
  dplyr::mutate(abs_error = abs(as.numeric(estHR) - as.numeric(noninHR))) %>% 
  dplyr::group_by(face.fitzpatrick, phone) %>% 
  dplyr::summarise(mean_err = mean(abs_error, na.rm = T),
                   max_err = max(abs_error, na.rm = T),
                   min_err = min(abs_error, na.rm = T),
                   sd_err = stats::sd(abs_error, na.rm = T),
                   ccc = DescTools::CCC(x = as.numeric(estHR),
                                        y = as.numeric(noninHR))$rho.c$est,
                   rho = cor(as.numeric(estHR), as.numeric(noninHR), method = 'pearson')) %>% 
  dplyr::ungroup()

# hist(err_mat$avg_err)

# Violin plot of CCC vs Fitzpatrick scale
library(ggplot2)
aa_est_face$face.fitzpatrick <- factor(aa_est_face$face.fitzpatrick)
aa_est_face$phone <- factor(aa_est_face$phone)
# ggplot(aa_est_face, aes(x = face.fitzpatrick, y = ccc)) + geom_violin(trim = F) + ylim(c(0,1))
# ggplot(aa_est_face, aes(x = face.fitzpatrick, y = rho)) + geom_violin(trim = F) + ylim(c(0,1))
# ggplot(aa_est_face, aes(x = phone, y = ccc)) + geom_violin(trim = F) + ylim(c(0,1))
# ggplot(aa_est_face, aes(x = phone, y = rho)) + geom_violin(trim = F) + ylim(c(0,1))

# heatmap of phone vs face.fitzpatrick, with CCC as the variable
ggplot(aa_est_face, aes(x = face.fitzpatrick, y= phone)) +
  geom_tile(aes(fill = ccc)) + 
  geom_text(aes(label = round(ccc, 2))) +
  scale_fill_gradient(low = "red", high = "green") 

#ggplot(aa_est_face, aes(x = phone, y = face.fitzpatrick)) +
#  geom_tile(aes(fill = rho)) +
#  geom_text(aes(label = round(rho, 2))) +
#  scale_fill_gradient(low = "red", high = "green")

ggplot(aa_est_face, aes(x = face.fitzpatrick, y = phone)) +
 geom_tile(aes(fill = mean_err)) +
 geom_text(aes(label = round(mean_err, 2))) +
 scale_fill_gradient(low = "green", high = "red")
 
# Concordance score between estimatedHR and NoninHR using Lin's concordance
DescTools::CCC(aa$estHR, aa$noninHR)$rho.c$est
mean(abs(aa$error))

#######################################
# Upload Data to Synapse 
#######################################
# Github link
gtToken = 'github_token.txt';
githubr::setGithubToken(as.character(read.table(gtToken)$V1))
thisFileName <- 'test/crf_nonin_ccc.R'
thisRepo <- getRepo(repository = "itismeghasyam/CRF_validation_analysis", ref="branch", refName='sagebio_master')
thisFile <- getPermlink(repository = thisRepo, repositoryPath=thisFileName)

# Write to Synapse
write.csv(aa_all,file = paste0('merged_crf_nonin_acf_esthr','.csv'),na="")
obj = File(paste0('merged_crf_nonin_acf_esthr','.csv'), 
           name = paste0('merged_crf_nonin_acf_esthr','.csv'), 
           parentId = 'syn12435196')
obj = synStore(obj, used = all.used.ids, executed = thisFile)
