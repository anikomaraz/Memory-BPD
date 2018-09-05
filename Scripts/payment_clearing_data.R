# SET WORKING DIRECTORY
#setwd("~/Downloads")
# setwd("~/Google Drive/_BPD_memory_Humboldt/Data_analysis")
library(tidyverse)
library(lubridate)
library(jsonlite)
library(httr)

source("./Scripts/not_approved_screening_sessions.R")
source("./Scripts/ipstack_key.R")

## READ DATA
# raw data from formr
data_screen <- read.csv2("./Data/Raw_data/screen_MTurk_180822.csv", sep=",", header = T)
#data_screen <- as.data.frame(jsonlite::fromJSON("./Data/Raw_data/screen_MTurk_180821.json"))

# raw time data downloaded from formr
#data_screen_time_raw <- read.csv2("./Data/Raw_data/screen_MTurk_itemdisplay_180815.csv", sep=",", header = T)
data_screen_time_raw <- jsonlite::fromJSON("./Data/Raw_data/screen_MTurk_itemdisplay_180822.json")
data_screen_time_raw$answered = as.POSIXct(strptime(data_screen_time_raw$answered, "%Y-%m-%d %H:%M:%S"))

#get rid of test sessions 
data_screen_time_raw <- data_screen_time_raw[str_length(data_screen_time_raw$session) > 1,]
data_screen_time_raw <- data_screen_time_raw[!is.na(data_screen_time_raw$session), ]
data_screen_time_raw <- data_screen_time_raw[!(data_screen_time_raw$session %in% remove_test_sessions), ]

# get relevant MTurk data
mturk1 <- read.csv2("./Data/MTurk_batch/batch1_20_180724.csv", sep=",", stringAsFactors=F)
mturk2 <- read.csv2("./Data/MTurk_batch/batch2_50_180725.csv", sep=",", stringAsFactors=F)
mturk3 <- read.csv2("./Data/MTurk_batch/batch3_500_180807.csv", sep=",", stringAsFactors=F)
mturk4 <- read.csv2("./Data/MTurk_batch/batch4_200_180813.csv", sep=",", stringAsFactors=F)
mturk5 <- read.csv2("./Data/MTurk_batch/batch5_200_180815.csv", sep=",", header = T, stringAsFactors=F)
mturk6 <- read.csv2("./Data/MTurk_batch/batch6_200_180815b.csv", sep=",", header = T, stringAsFactors=F)
# from here onwards: no non-US IP address allowed & warning not to submit when attention check failed
mturk7 <- read.csv2("./Data/MTurk_batch/batch7_200_180816.csv", sep=",", header=T, stringAsFactors=F) 
mturk7b <- read.csv2("./Data/MTurk_batch/batch7b_229_180817.csv", sep=",", header = T, stringAsFactors=F)
mturk8 <- read.csv2("./Data/MTurk_batch/batch8_200_180816b.csv", sep=",", header=T, stringAsFactors=F)
mturk9 <- read.csv2("./Data/MTurk_batch/batch9_200_180817.csv", sep=",", header=T, stringAsFactors=F)
# from here onwards: previous features + survey is invisible to workers who have previously filled out
mturk10 <- read.csv2("./Data/MTurk_batch/batch10_282_180818.csv", sep=",", header=T)
mturk11 <- read.csv2("./Data/MTurk_batch/batch11_500_180818b.csv", sep=",", header=T)
mturk12 <- read.csv2("./Data/MTurk_batch/batch12_500_180820.csv", sep=",", header=T)
mturk13 <- read.csv2("./Data/MTurk_batch/batch13_500_180820b.csv", sep=",", header=T)
mturk14 <- read.csv2("./Data/MTurk_batch/batch14_500_180821.csv", sep=",", header=T)

# assign mturk data and extract relevant information 
mturk_data <- mturk14
mturk_data <- subset(mturk_data, select=c("WorkerId",   # their individual MTurk worker ID to preserve their anonymity
                                          "WorkTimeInSeconds", # as measured by MTurk. Time between "accepting" the HIT and submitting the survey code
                                          "Answer.surveycode",  # this latter is the code Workers had to submit to complete the HIT
                                          "LifetimeApprovalRate"))   # if other than "0% (0/0)", then the Worker have already completed the survey in another batch, therefore has to be rejected (which they are warned about)

# select relevant-only data from formr (raw data + time)
# extract the first 8 characters of the session, which should have been entered as survey code
data_screen$session_short <- substr(data_screen$session, start=1, stop=8)
data_screen <- data_screen[data_screen$session_short %in% mturk_data$Answer.surveycode,]
data_screen_time_raw <- data_screen_time_raw[data_screen_time_raw$session %in% data_screen$session, ]


# CHECK DATA  
  
# calculate the speed of filling out
data_screen_time <- data_screen_time_raw[data_screen_time_raw$item_name == "screen_attention_check", ]

#plot time needed for the screening
ggplot(data=data_screen_time, aes(answered_relative)) + 
  geom_histogram(aes(y=..density..), fill="grey", colour="black", binwidth = 3) +
  geom_vline(aes(xintercept = median(data_screen_time$answered_relative, na.rm = T)), col="red", size=1) +
  geom_text(aes(label=paste0("Median = ", round(mean(data_screen_time$answered_relative, na.rm = T), 1)), 
                y=-0.001, x=median(data_screen_time$answered_relative, na.rm=T))) +
  #geom_density(alpha=.2, fill="#FF6666") +
  ggtitle("Time neededto fill out the screening items (reading consent excluded)") +
  xlab("time (seconds)") +
  theme_linedraw()

# data_screen_time_too_long[data_screen_time_too_long$screen_difftime == "522869",]
# data_screen_time[data_screen_time$session == "2GzEhl10bEkJqCkqyXW2IHk8U5GxYlwkYJkMg2naPNT164N3EnarlPwVc7vzwFRx",]


#plot minimum time screeners
ggplot(data=data_screen_time[data_screen_time$answered_relative < 50000,], aes(answered_relative /1000)) + 
  geom_histogram(fill="darkgrey", colour="black", binwidth = 1) +
  #geom_density(alpha=.2, fill="#FF6666") +
  scale_x_continuous(limits=c(0, 50)) +
  ggtitle("Critically low times for filling out") +
  xlab("time (seconds)") +
  theme_linedraw()


## EVALUATION

# was the response time too short? 
data_screen_time$screen_difftime_too_short <- ifelse(data_screen_time$answered_relative > 30000, "passed", "failed")


# select "virgins", i.e. not filled out the survey before0
mturk_data$survey_virgin <- ifelse(mturk_data$LifetimeApprovalRate == "0% (0/0)", "passed", "failed")

# ERRORS  
data_screen$screen_attention_check_error <- ifelse(data_screen$screen_attention_check == "2", "passed", "failed")

# passed screening?
bpd_items <- c("bpd_s1", "bpd_s2", "bpd_s3", "bpd_s4", "bpd_s5", 
               "bpd_s6", "bpd_s7", "bpd_s8", "bpd_s9", "bpd_s10")
data_screen$screen_bpd <- rowSums(data_screen[, bpd_items]) 
data_screen$screen_bpd_posNeg <- data_screen$screen_bpd > 15
data_screen$screen_positive <- ifelse(data_screen$screen_bpd_posNeg & 
                                       data_screen$age != "under_18" &  
                                       data_screen$age < 65 &  
                                       data_screen$residence == "United_States" &  
                                       data_screen$screen_attention_check == "2", "passed", "failed")  


# ip_address
# my_access_key3 ="please_enter_your_own"

# access the geolocation via ip address (using ipstatic.com, a free service)
get_geo_ip <- function(ip) {
  url <- paste(c("http://api.ipstack.com/", ip, "?access_key=", my_access_key3), collapse="")
  geolocation <- fromJSON(readLines(url, warn=FALSE))
  #geolocation <- readLines(url, warn = F)
  return(geolocation$country_code)
}

get_geo_ip("201.247.43.210")

data_screen$ip_address <- as.character(data_screen$ip_address)
data_screen$ip_country <- sapply(data_screen$ip_address, get_geo_ip)

table(data_screen$ip_country)


## FINAL DATA FOR SUBMISSION TO MTURK
data_screen_final <- merge(data_screen[, c("session", "screen_attention_check_error", "screen_positive", "session_short", "ip_country", "ip_address")], 
                               data_screen_time[, c("session", "answered_relative", "screen_difftime_too_short")], by = "session")
data_screen_final <- merge(mturk_data[, c("Answer.surveycode", "WorkerId", "survey_virgin")], 
                           data_screen_final, by.x = "Answer.surveycode", by.y="session_short")
# approve submission?
data_screen_final$approve <- ifelse(data_screen_final$screen_attention_check_error == "passed" &  
                                      data_screen_final$survey_virgin == "passed" &
                                      data_screen_final$ip_country == "US", "x", "") 

# feedback
data_screen_final$feedback <- ifelse(data_screen_final$screen_attention_check_error == "failed", "You have failed the attention check, therefore I cannot approve your HIT.", 
                                     ifelse(data_screen_final$survey_virgin == "failed", "You have already filled out this HIT before. As detailed in the survey instructions (and just before submitting the HIT), Workers can only be approved if they have not yet filled out this survey. Therefore I cannot approve your HIT. Sorry!", 
                                            ifelse(data_screen_final$ip_country != "US",  "Our algorithms identified this submission as invalid.", "")
                                     )
                                )
                                     
                                     
# save data to submit to MTurk
mturk14_for_submission <- left_join(mturk14, data_screen_final[, c("approve", "feedback", "Answer.surveycode")], 
                                   by="Answer.surveycode")

# save data
write.csv2(data_screen_final, file="./Data/MTurk_batch/mturk14_fulldata_180821.csv")
write.csv2(mturk14_for_submission, file="./Data/MTurk_batch/mturk14_forFeedback_180821.csv")



# CHECKING FUNCTIONS
# for checking session by session (=survey code)
data_screen_final[data_screen_final$session_short == "VgkoLpwL",]
data_screen_final[data_screen_final$session == 
                    "4163ZhbDc-coVedQK7kmHctfOjFM49kDGfn36MBdTvBrqGmGZlCtNPzeSJHLIXw0",]
data_screen_final[data_screen_final$WorkerId == "A2196WCNDZULFS",]


# LOAD FINAL (FULL) DATA
mturk1_check <- read.csv2("./Data/MTurk_batch/mturk1_fulldata_180724.csv", sep=";", header = T, check.names=F)
mturk2_check <- read.csv2("./Data/MTurk_batch/mturk2_fulldata_180725.csv", sep=";", header = T, check.names=F)
mturk3_check <- read.csv2("./Data/MTurk_batch/mturk3_fulldata_180807.csv", sep=";", header = T, check.names=F)
mturk4_check <- read.csv2("./Data/MTurk_batch/mturk4_fulldata_180813.csv", sep=";", header = T, check.names=F)
mturk5_check <- read.csv2("./Data/MTurk_batch/mturk5_fulldata_180815.csv", sep=";", header = T, check.names=F)
mturk6_check <- read.csv2("./Data/MTurk_batch/mturk6_fulldata_180815b.csv", sep=";", header = T, check.names=F)
#mturk7_check <- read.csv2("./Data/MTurk_batch/mturk7_fulldata_180816.csv", sep=";", header = T, check.names=F)
mturk7b_check <- read.csv2("./Data/MTurk_batch/mturk7b_fulldata_180817.csv", sep=";", header = T, check.names=F)
mturk8_check <- read.csv2("./Data/MTurk_batch/mturk8_fulldata_180816.csv", sep=";", header = T, check.names=F)
mturk9_check <- read.csv2("./Data/MTurk_batch/mturk9_fulldata_180817.csv", sep=";", header = T, check.names=F)
mturk10_check <- read.csv2("./Data/MTurk_batch/mturk10_fulldata_180818.csv", sep=";", header = T, check.names=F)
mturk11_check <- read.csv2("./Data/MTurk_batch/mturk11_fulldata_180818b.csv", sep=";", header = T, check.names=F)
mturk12_check <- read.csv2("./Data/MTurk_batch/mturk12_fulldata_180820.csv", sep=";", header = T, check.names=F)
mturk13_check <- read.csv2("./Data/MTurk_batch/mturk13_fulldata_180820b.csv", sep=";", header = T, check.names=F)
mturk14_check <- read.csv2("./Data/MTurk_batch/mturk14_forFeedback_180821.csv", sep=";", header = T, check.names = F)


# bind mturk full data and preserve the source
data_mturk <- c("mturk1_check", "mturk2_check", "mturk3_check", "mturk4_check", "mturk5_check", "mturk6_check",
                "mturk7b_check", "mturk8_check", "mturk9_check", "mturk10_check", "mturk11_check", "mturk12_check", 
                "mturk13_check", "mturk14_check")

append_source <- function(data_mturk) {
  do.call(bind_rows, lapply(data_mturk, function(x) {
    cbind(get(x), source = x)
  }))
}

mturk_all_data <- append_source(data_mturk)

# save the final data without sensitive information (ip_address)
nrow(mturk_all_data)
write.csv2(mturk_all_data, file="./Data/Raw_data/mturk_full_data_withIP_180821.csv")
write.csv2(mturk_all_data[, !colnames(mturk_all_data) == "ip_address"], file="./Data/mturk_full_data_180821.csv")



## checking the reason for rejection for Workers who inquire despite feedback
mturk_all_data[mturk_all_data$WorkerId == "AYCGL8UG3ZVVK",]$Answer.surveycode


## FULL DATA CHECK AND VISUALISE
sort(mturk_all_data$screen_difftime, decreasing=T)[1:30]
sort(mturk_all_data$screen_difftime, decreasing=F)[1:30]
mturk_all_data_difftime <- mturk_all_data[(mturk_all_data$screen_difftime > 0) & (mturk_all_data$screen_difftime < 2000),]
nrow(mturk_all_data_difftime)

# plot time of filling out on the full database
ggplot(data=mturk_all_data_difftime, aes(screen_difftime)) + 
  geom_histogram(aes(y=..density..), fill="grey", colour="black", binwidth = 3) +
  geom_vline(aes(xintercept = median(mturk_all_data_difftime$screen_difftime, na.rm = T)), col="red", size=1) +
  geom_text(aes(label=paste0("Median = ", round(mean(mturk_all_data_difftime$screen_difftime, na.rm = T), 1)), 
                y=-0.001, x=median(mturk_all_data_difftime$screen_difftime, na.rm=T))) +
  #geom_density(alpha=.2, fill="#FF6666") +
  ggtitle("Time neededto fill out the screening items (reading consent excluded)") +
  xlab("time (seconds)") +
  theme_linedraw()




