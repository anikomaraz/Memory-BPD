# SET WORKING DIRECTORY
#setwd("~/Downloads")
# setwd("~/Google Drive/_BPD_memory_Humboldt/Data_analysis")
library(tidyverse)
library(lubridate)
#library(data.table)
#library(scales)

setwd("/home/aniko/R/Memory-and-BPD")

# GET AND GATHER DATA
data_screen <- read.csv2("./Data/Raw_data/screen_MTurk_180801.csv")
data_screen_itemdisplay <- read.csv2("./Data/Raw_data/screen_MTurk_itemdisplay_180727.csv")
data_screen_itemdisplay$shown = as.POSIXct(strptime(data_screen_itemdisplay$shown, "%Y-%m-%d %H:%M:%S"))

# get relevant MTurk data
mturk_data <- as.data.frame(read.csv2("./Data/MTurk_batch/batch_180803.csv", sep=","))
mturk_data <- subset(mturk_data, select=c("WorkerId", "WorkTimeInSeconds", "Answer.surveycode"))

getwd()

# SPEED OF FILLING OUT
data_screen_calc <- data_screen_itemdisplay %>%
    select(session, item_name, shown) %>% filter(item_name %in% c("gender", #which is the first item to display after consent
                                                                  "note_mturk_code")) %>%  #which is the first item to display after submitting the last aswer
    spread(item_name, shown) %>%
    mutate(screen_difftime = as.numeric(difftime(note_mturk_code, gender, unit = "secs"))) 

#plot time needed for the screening
ggplot(data=data_screen_calc, aes(screen_difftime)) + 
  geom_histogram(aes(y=..density..), fill="white", colour="black", binwidth = 1) +
  geom_density(alpha=.2, fill="#FF6666") +
  expand_limits(y=0) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 20))

#plot minimum time screeners
ggplot(data=data_screen_calc[data_screen_calc$screen_difftime < 40,], aes(screen_difftime)) + 
  geom_histogram(aes(y=..density..), fill="white", colour="black", binwidth = 1) +
  geom_density(alpha=.2, fill="#FF6666") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 20))

# is it too short? (T=too short/error)
data_screen_calc$screen_difftime_too_short <- ifelse(data_screen_calc$screen_difftime < 30, "error", 0)
# ERRORS  (T=error)
data_screen$screen_attention_check_error <- ifelse(data_screen$screen_attention_check == "2", 0, "error")


# FINAL DATA FOR SCREEN CHECKING
data_screen_final <- merge(data_screen, data_screen_calc[, c("session", "screen_difftime_too_short")], by = "session")
data_screen_final_relevantOnly <- select(data_screen_final, c("session", "screen_difftime_too_short", "screen_attention_check_error"))  
data_screen_final_relevantOnly

# merge with the relevant screen data
data_screen_final_relevantOnly$session_short <- substr(data_screen_final_relevantOnly$session, start=1, stop=8)
mturk_data$session_identified <- ifelse(mturk_data$Answer.surveycode %in% data_screen_final_relevantOnly$session_short, T, F)
names(mturk_data)[names(mturk_data) == "Answer.surveycode"] <- "session_short"
mturk_data_full <- full_join(data_screen_final_relevantOnly, mturk_data, by = "session_short")

# save data
write.csv2(mturk_data_full, file="./Data/MTurk_batch/mturk_data_full.csv")
# write.csv2(data_screen_final_relevantOnly, file="./Data/MTurk_batch/data_screen_final_relevantOnly.csv")


# CHECKING FUNCTION
# for checking session by session (=survey code)
select(mturk_data_full, c("screen_difftime_too_short", "screen_attention_check_error", "session_identified", "session_short")
       )[mturk_data_full$session_short == "Id7RcFmV",]

