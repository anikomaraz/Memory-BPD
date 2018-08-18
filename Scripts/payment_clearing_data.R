# SET WORKING DIRECTORY
#setwd("~/Downloads")
# setwd("~/Google Drive/_BPD_memory_Humboldt/Data_analysis")
library(tidyverse)
library(lubridate)
library(jsonlite)
library(httr)

source("./Scripts/not_approved_screening_sessions.R")

# raw data, as in formr
data_screen <- read.csv2("./Data/Raw_data/screen_MTurk_180818.csv", sep=",", header = T)
#data_screen <- as.data.frame(jsonlite::fromJSON("./Data/Raw_data/screen_MTurk_180818.json"), stringAsFactors=F)

# raw time data downloaded from formr
#data_screen_time_raw <- read.csv2("./Data/Raw_data/screen_MTurk_itemdisplay_180815.csv", sep=",", header = T)
data_screen_time_raw <- as.data.frame(jsonlite::fromJSON("./Data/Raw_data/screen_MTurk_itemdisplay_180818.json"), stringAsFactors=F)
data_screen_time_raw$created = as.POSIXct(strptime(data_screen_time_raw$created, "%Y-%m-%d %H:%M:%S"))
data_screen_time_raw <- data_screen_time_raw[str_length(data_screen_time_raw$session) > 1,]
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
# from here onwards: previous features + survey is invisible to workers who have previously filled out
mturk9 <- read.csv2("./Data/MTurk_batch/batch9_200_180817.csv", sep=",", header=T, stringAsFactors=F)
mturk10 <- read.csv2("./Data/MTurk_batch/batch10_282_180818.csv", sep=",", header=T)
mturk11 <- read.csv2("./Data/MTurk_batch/batch11_500_180818b.csv", sep=",", header=T)

mturk_data <- mturk11
mturk_data <- subset(mturk_data, select=c("WorkerId",   # their individual MTurk worker ID to preserve their anonymity
                                          "WorkTimeInSeconds", # as measured by MTurk. Time between "accepting" the HIT and submitting the survey code
                                          "Answer.surveycode",  # this latter is the code Workers had to submit to complete the HIT
                                          "LifetimeApprovalRate"))   # if other than "0% (0/0)", then the Worker have already completed the survey in another batch, therefore has to be rejected (which they are warned about)

# select relevant-only data from formr (raw data + time)
# extract the first 8 characters of the session, which should have been entered as survey code
data_screen$session_short <- substr(data_screen$session, start=1, stop=8)
data_screen <- data_screen[data_screen$session_short %in% mturk_data$Answer.surveycode,]
data_screen_time_raw <- data_screen_time_raw[data_screen_time_raw$session %in% data_screen$session, ]
  
  
# calculate the speed of filling out
data_screen_time <- data_screen_time_raw %>%
  select(session, item_name, created) %>% 
  filter(item_name %in% c("gender", #which is the first item to display after consent
                          "note_mturk_code")) %>%  #which is the first item to display after submitting the last aswer
  spread(item_name, created) %>%
  mutate(screen_difftime = as.numeric(difftime(note_mturk_code, gender, unit = "secs"))) 


## PLOT
data_screen_time_too_long <-  data_screen_time[data_screen_time$screen_difftime > 2000,]
# table(data_screen_time_too_long$session, data_screen_time_too_long$screen_difftime)
data_screen_time_plot <- data_screen_time[data_screen_time$screen_difftime < 2000,]
nrow(data_screen_time[data_screen_time$screen_difftime >2000,])
nrow(data_screen_time[data_screen_time$screen_difftime <0,])

#plot time needed for the screening
ggplot(data=data_screen_time_plot, aes(screen_difftime)) + 
  geom_histogram(aes(y=..density..), fill="grey", colour="black", binwidth = 3) +
  geom_vline(aes(xintercept = median(data_screen_time_plot$screen_difftime, na.rm = T)), col="red", size=1) +
  geom_text(aes(label=paste0("Median = ", round(mean(data_screen_time_plot$screen_difftime, na.rm = T), 1)), 
                y=-0.001, x=median(data_screen_time_plot$screen_difftime, na.rm=T))) +
  #geom_density(alpha=.2, fill="#FF6666") +
  ggtitle("Time neededto fill out the screening items (reading consent excluded)") +
  xlab("time (seconds)") +
  theme_linedraw()

# data_screen_time_too_long[data_screen_time_too_long$screen_difftime == "522869",]
# data_screen_time[data_screen_time$session == "2GzEhl10bEkJqCkqyXW2IHk8U5GxYlwkYJkMg2naPNT164N3EnarlPwVc7vzwFRx",]


#plot173.26.103.49 minimum time screeners
ggplot(data=data_screen_time[is.na(data_screen_time$screen_difftime) < 40,], aes(screen_difftime)) + 
  geom_histogram(fill="darkgrey", colour="black", binwidth = 1) +
  # geom_density(alpha=.2, fill="#FF6666") +
  scale_x_continuous(limits=c(0, 40)) +
  ggtitle("Critically low times for filling out") +
  xlab("time (seconds)") +
  theme_linedraw()


## EVALUATION

# was the response time too short? 
data_screen_time$screen_difftime_too_short <- ifelse(data_screen_time$screen_difftime > 30, "passed", "failed")


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
my_access_key3 ="please_enter_your_own"

# access the geolocation via ip address (using ipstatic.com, a free service)
get_geo_ip <- function(ip) {
  url <- paste(c("http://api.ipstack.com/", ip, "?access_key=", my_access_key3), collapse="")
  geolocation <- fromJSON(readLines(url, warn=FALSE))
  #geolocation <- readLines(url, warn = F)
  return(geolocation$country_code)
}

get_geo_ip("201.247.43.210")

data_screen$ip_address <- as.character(data_screen$ip_address)
#data_screen_ip <- data_screen[str_length(data_screen$ip_address >0L),]
data_screen$ip_country <- sapply(data_screen$ip_address, get_geo_ip)

table(data_screen$ip_country)


## FINAL DATA FOR SUBMISSION TO MTURK
data_screen_final <- merge(data_screen[, c("session", "screen_attention_check_error", "screen_positive", "session_short", "ip_country", "ip_address")], 
                               data_screen_time[, c("session", "screen_difftime", "screen_difftime_too_short")], by = "session")
data_screen_final <- merge(mturk_data[, c("Answer.surveycode", "WorkerId", "survey_virgin")], 
                           data_screen_final, by.x = "Answer.surveycode", by.y="session_short")
# approve submission?
data_screen_final$approve <- ifelse(data_screen_final$screen_attention_check_error == "passed" &  
                                      data_screen_final$survey_virgin == "passed" &
                                      data_screen_final$ip_country == "US", "x", "REJECT") 

# feedback
data_screen_final$feedback <- ifelse(data_screen_final$screen_attention_check_error == "failed", "You have failed the attention check, therefore I cannot approve your HIT.", 
                                     ifelse(data_screen_final$survey_virgin == "failed", "You have already filled out this HIT before. As detailed in the survey instructions (and just before submitting the HIT), Workers can only be approved if they have not yet filled out this survey. Therefore I cannot approve your HIT. Sorry!", 
                                            ifelse(data_screen_final$ip_country != "US",  "Our algorithms identified this submission as invalid.", "")
                                     )
                                )
                                     
                                     
# save data to submit to MTurk
mturk11_for_submission <- left_join(mturk11, data_screen_final[, c("approve", "feedback", "Answer.surveycode")], 
                                   by="Answer.surveycode")

# save data
write.csv2(data_screen_final, file="./Data/MTurk_batch/mturk11_fulldata_180818b.csv")
write.csv2(mturk11_for_submission, file="./Data/MTurk_batch/mturk11_forFeedback_180818b.csv")
table(data_screen_final$screen_attention_check_error)




# CHECKING FUNCTIONS
# for checking session by session (=survey code)
data_screen_final[data_screen_final$session_short == "A21LWU8RK1ZKC6",]
data_screen_final[data_screen_final$session == 
                    "4163ZhbDc-coVedQK7kmHctfOjFM49kDGfn36MBdTvBrqGmGZlCtNPzeSJHLIXw0",]
data_screen_final[data_screen_final$WorkerId == "A2HSFJ1STZZGM5",]

## checking the reason for rejection for Workers who inquire
mturk7b_check[mturk7b_check$WorkerId == "A1ZDHI15519WJ7",]


# LOAD FINAL (FULL) DATA
mturk1_check <- read.csv2("./Data/MTurk_batch/mturk1_fulldata_180724.csv", sep=";", header = T)
mturk2_check <- read.csv2("./Data/MTurk_batch/mturk2_fulldata_180725.csv", sep=";", header = T)
mturk3_check <- read.csv2("./Data/MTurk_batch/mturk3_fulldata_180807.csv", sep=";", header = T)
mturk4_check <- read.csv2("./Data/MTurk_batch/mturk4_fulldata_180813.csv", sep=";", header = T)
mturk5_check <- read.csv2("./Data/MTurk_batch/mturk5_fulldata_180815.csv", sep=";", header = T)
mturk6_check <- read.csv2("./Data/MTurk_batch/mturk6_fulldata_180815b.csv", sep=";", header = T)
mturk7_check <- read.csv2("./Data/MTurk_batch/mturk7_fulldata_180816.csv", sep=";", header = T)
mturk7b_check <- read.csv2("./Data/MTurk_batch/mturk7b_fulldata_180817.csv", sep=";", header = T)
mturk8_check <- read.csv2("./Data/MTurk_batch/mturk8_fulldata_180816.csv", sep=";", header = T)
mturk9_check <- read.csv2("./Data/MTurk_batch/mturk9_fulldata_180817.csv", sep=";", header = T)
mturk10_check <- read.csv2("./Data/MTurk_batch/mturk10_fulldata_180818.csv", sep=";", header = T)
mturk11_check <- read.csv2("./Data/MTurk_batch/mturk11_fulldata_180818b.csv", sep=";", header = T)


# save the full data
mturk_all_data <- bind_rows(mturk1_check, mturk2_check, mturk3_check, mturk4_check, mturk5_check, mturk6_check, 
                            mturk7_check, mturk8_check, mturk9_check, mturk10_check, mturk11_check)
nrow(mturk_all_data)
write.csv2(mturk_all_data[, !colnames(mturk_all_data) == "ip_address"], file="./Data/mturk_full_data_180818.csv")

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

