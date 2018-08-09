# SET WORKING DIRECTORY
#setwd("~/Downloads")
# setwd("~/Google Drive/_BPD_memory_Humboldt/Data_analysis")
library(tidyverse)
library(lubridate)
#library(data.table)
#library(scales)

setwd("/home/aniko/R/Memory-and-BPD")


# GET AND GATHER DATA

# raw data, as in formr
data_screen <- read.csv2("./Data/Raw_data/screen_MTurk_180808.csv")

# raw time data downloaded from formr
data_screen_time_raw <- read.csv2("./Data/Raw_data/screen_MTurk_itemdisplay_180808.csv")
data_screen_time_raw$created = as.POSIXct(strptime(data_screen_time_raw$created, "%Y-%m-%d %H:%M:%S"))

# get relevant MTurk data
mturk_data <- as.data.frame(read.csv2("./Data/MTurk_batch/batch_180808.csv", sep=","))
mturk_data <- subset(mturk_data, select=c("WorkerId",   # their individual MTurk worker ID to preserve their anonymity
                                          "WorkTimeInSeconds", # as measured by MTurk. Time between "accepting" the HIT and submitting the survey code
                                          "Answer.surveycode",  # this latter is the code Workers had to submit to complete the HIT
                                          "LifetimeApprovalRate"))   # if other than "0% (0/0)", then the Worker have already completed the survey in another batch, therefore has to be rejected (which they are warned about)


# calculate the speed of filling out
data_screen_time <- data_screen_time_raw %>%
    select(session, item_name, created) %>% filter(item_name %in% c("gender", #which is the first item to display after consent
                                                                  "note_mturk_code")) %>%  #which is the first item to display after submitting the last aswer
    spread(item_name, created) %>%
    mutate(screen_difftime = as.numeric(difftime(note_mturk_code, gender, unit = "secs"))) 


## PLOT

#plot time needed for the screening
ggplot(data=data_screen_time, aes(screen_difftime)) + 
  geom_histogram(aes(y=..density..), fill="white", colour="black", binwidth = 3) +
  geom_vline(aes(xintercept = median(data_screen_time$screen_difftime, na.rm = T)), col="red", size=1) +
  geom_text(aes(label=paste0("Median = ", round(mean(data_screen_time$screen_difftime, na.rm = T), 1)), 
                y=-0.001, x=mean(data_screen_time$screen_difftime, na.rm=T))) +
  geom_density(alpha=.2, fill="#FF6666") +
  ggtitle("Time neededto fill out the screening items (reading consent excluded)") +
  xlab("time (seconds)") +
  theme_linedraw()


#plot minimum time screeners
ggplot(data=data_screen_time[is.na(data_screen_time$screen_difftime) < 40,], aes(screen_difftime)) + 
  geom_histogram(fill="white", colour="black", binwidth = 1) +
  # geom_density(alpha=.2, fill="#FF6666") +
  scale_x_continuous(limits=c(0, 40)) +
  ggtitle("Critically low times for filling out (seconds)") +
  xlab("time (seconds)") +
  theme_linedraw()


## EVALUATION

# was the response time too short? 
data_screen_time$screen_difftime_too_short <- ifelse(data_screen_time$screen_difftime > 30, "passed", "failed")

# extract the first 8 characters of the session, which should have been entered as survey code
data_screen$session_short <- substr(data_screen$session, start=1, stop=8)
# check that session_short is the same as the Worker entered as survey code
mturk_data$session_identified <- ifelse(mturk_data$Answer.surveycode %in% data_screen$session_short, "passed", "failed")

# select "virgins", i.e. not filled out the survey before0
mturk_data$survey_virgin <- ifelse(mturk_data$LifetimeApprovalRate == "0% (0/0)", "passed", "failed")

# ERRORS  ()
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



## FINAL DATA FOR SCREEN CHECKING
data_screen_final <- merge(data_screen[, c("session", "screen_attention_check_error", "screen_positive", "session_short")], 
                               data_screen_time[, c("session", "screen_difftime", "screen_difftime_too_short")], by = "session")
names(mturk_data)[names(mturk_data) == "Answer.surveycode"] <- "session_short"
data_screen_final <- merge(mturk_data[, c("session_short", "WorkerId", "session_identified", "survey_virgin")], 
                           data_screen_final, by="session_short")

data_screen_final$approve <- ifelse(data_screen_final$screen_attention_check_error == "passed" &  
                                      data_screen_final$session_identified == "passed" & 
                                      data_screen_final$survey_virgin == "passed", "x", "REJECT") 

# save data
write.csv2(data_screen_final, file="./Data/MTurk_batch/mturk_data_full2.csv")



# CHECKING FUNCTIONS
# for checking session by session (=survey code)
data_screen_final[data_screen_final$session == "ZUZvDcOD0TlVuuUbJYlqZMK1GOLbIs_0IRjEYrCI7-KiPw2BCIiq60jhrO_l0qxP",]

## checking the reason for rejection for Workers who inquire
data_screen_final[data_screen_final$WorkerId == "AIZEEJUMWJ0WX",]

