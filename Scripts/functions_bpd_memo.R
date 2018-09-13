

library(tidyverse)
library(jsonlite)
library(here)

## LOAD PACKAGES
load_my_packages <- function(package){
  new.package <- packages[!(package %in% installed.packages()[, "Package"])]
  if (length(new.package)) 
    install.packages(new.package, dependencies = TRUE)
  sapply(package, require, character.only = TRUE)
}

## GET AND MERGE DATA
# Task: call 6 datasets (2 screening + 4 surveys) for each data collection (facebook and mturk), 
# insert time indicator (i.e. t1) to variable names, 
# and merge them by session (output: data_row())
#
# these columns have different classes when read.csv is called, so I am going to match their class for merging
cols_to_integer <- c("age.sc", "age.t1", "age.t2", 
                     "lec_1.t4", "lec_1.t1", "lec_3.t1", "lec_10.t1", "lec_12.t1", "lec_17.t1", 
                     "lec_3.t4", "lec_15.t4", "lec_17.t4")

cols_to_character <- c("shown_relative.t1", "answered_relative.t1")

cols_to_numeric <- c("answered_relative.t1")

# get databases from facebook 
get_bpdMemo_raw_data <- function(my_version="180726", dataoutput=mturk) {
  
    # pull databases 
    # data_sc <- read.csv2(paste0(here("Data", "Raw_data", "screen_BPDMemo_"), my_version, ".csv"), sep=";", stringsAsFactors = F)
    # data_scP <- read.csv2(paste0(here("Data", "Raw_data", "screen_positive_BPDMemo_"), my_version, ".csv"), sep=";", stringsAsFactors = F)
    # data_t1 <- read.csv2(paste0(here("Data", "Raw_data", "part1_BPDMemo_"), my_version, ".csv"), sep=";",  stringsAsFactors = F)
    # data_t2 <- read.csv2(paste0(here("Data", "Raw_data", "part2_BPDMemo_"), my_version, ".csv"), sep=";", stringsAsFactors = F)
    # data_t3 <- read.csv2(paste0(here("Data", "Raw_data", "part3_BPDMemo_"), my_version, ".csv"), sep=";", stringsAsFactors = F)
    # data_t4 <- read.csv2(paste0(here("Data", "Raw_data", "part4_BPDMemo_"), my_version, ".csv"), sep=";", stringsAsFactors = F)
  
  data_sc <- read.csv2(paste0("../../Data/Raw_data/screen_BPDMemo_", my_version, ".csv"), sep=";", stringsAsFactors = F)
  data_scP <- read.csv2(paste0("../../Data/Raw_data/screen_positive_BPDMemo_", my_version, ".csv"), sep=";", stringsAsFactors = F)
  data_t1 <- read.csv2(paste0("../../Data/Raw_data/part1_BPDMemo_", my_version, ".csv"), sep=";",  stringsAsFactors = F)
  data_t2 <- read.csv2(paste0("../../Data/Raw_data/part2_BPDMemo_", my_version, ".csv"), sep=";", stringsAsFactors = F)
  data_t3 <- read.csv2(paste0("../../Data/Raw_data/part3_BPDMemo_", my_version, ".csv"), sep=";", stringsAsFactors = F)
  data_t4 <- read.csv2(paste0("../../Data/Raw_data/part4_BPDMemo_", my_version, ".csv"), sep=";", stringsAsFactors = F)
  
  
    # data_t1time <- read.csv2(paste0(here("Data", "Raw_data", "part1_BPDMemo_itemdisplay_"), my_version, ".csv"))
   
    ## add a suffix, so that variables do not mix when merged into one dataframe
    colnames(data_sc) <- paste(colnames(data_sc), "sc", sep=".")
    colnames(data_scP) <- paste(colnames(data_scP), "scP", sep=".")
    colnames(data_t1) <- paste(colnames(data_t1), "t1", sep=".")
    colnames(data_t2) <- paste(colnames(data_t2), "t2", sep=".")
    colnames(data_t3) <- paste(colnames(data_t3), "t3", sep=".")
    colnames(data_t4) <- paste(colnames(data_t4), "t4", sep=".")
    # colnames(data_t1time) <- paste(colnames(data_t1time), "t1", sep=".")
    
    ## the column name 'session' needs to be identical for merging 
    names(data_sc)[names(data_sc) == "session.sc"] <- "session"
    names(data_scP)[names(data_scP) == "session.scP"] <- "session"
    names(data_t1)[names(data_t1) == "session.t1"] <- "session"
    names(data_t2)[names(data_t2) == "session.t2"] <- "session"
    names(data_t3)[names(data_t3) == "session.t3"] <- "session"
    names(data_t4)[names(data_t4) == "session.t4"] <- "session"
    # names(data_t1time)[names(data_t1time) == "session.t1"] <- "session"
    
    # merge databases (except t1time, which comes later)
    data_tmp1 <- data_sc %>% dplyr::left_join(data_scP, all=F, by="session")
    data_tmp2 <- data_tmp1 %>% dplyr::left_join(data_t1, all=F, by="session")
    data_tmp3 <- data_tmp2 %>% dplyr::left_join(data_t2, all=F, by="session")
    data_tmp4 <- data_tmp3 %>% dplyr::left_join(data_t3, all=F, by="session")
    data_raw <- data_tmp4 %>% dplyr::left_join(data_t4, all=F, by="session") 
    
    for (i in cols_to_integer) {
      data_raw[ ,i] <- as.integer(data_raw[, i])
    }
    
    # data_t1time <- data_t1time[str_length(data_t1time$session) > 0L,]
    # data_t1time$answered_relative.t1 <- as.numeric(data_t1time$answered_relative.t1)
    
    data_raw <<- data_raw
    # data_t1time <<- data_t1time
  }


# get databases from mturk

# setwd("./Scripts/Data_analysis")

get_bpdMemo_raw_data_mturk <- function(my_version="180821") {    
  
  # pull databases 
  # data_sc_mturk <- read.csv(paste0(here("Data", "Raw_data", "screen_MTurk_"), my_version, ".csv"), sep=",", stringsAsFactors = F)
  # data_scP_mturk <- read.csv(paste0(here("Data", "Raw_data", "screen_positive_MTurk_"), my_version, ".csv"), sep=",", stringsAsFactors = F)
  # data_t1_mturk <- read.csv(paste0(here("Data", "Raw_data", "part1_MTurk_"), my_version, ".csv"), sep=",", stringsAsFactors = F)
  # data_t2_mturk <- read.csv(paste0(here("Data", "Raw_data", "part2_MTurk_"), my_version, ".csv"), sep=",", stringsAsFactors = F)
  # data_t3_mturk <- read.csv(paste0(here("Data", "Raw_data", "part3_MTurk_"), my_version, ".csv"), sep=",", stringsAsFactors = F)
  # data_t4_mturk <- read.csv(paste0(here("Data", "Raw_data", "part4_MTurk_"), my_version, ".csv"), sep=",", stringsAsFactors = F)
  
  data_sc_mturk <- read.csv(paste0("../../Data/Raw_data/screen_MTurk_", my_version, ".csv"), sep=",", stringsAsFactors = F)
  data_scP_mturk <- read.csv(paste0("../../Data/Raw_data/screen_positive_MTurk_", my_version, ".csv"), sep=",", stringsAsFactors = F)
  data_t1_mturk <- read.csv(paste0("../../Data/Raw_data/part1_MTurk_", my_version, ".csv"), sep=",", stringsAsFactors = F)
  data_t2_mturk <- read.csv(paste0("../../Data/Raw_data/part2_MTurk_", my_version, ".csv"), sep=",", stringsAsFactors = F)
  data_t3_mturk <- read.csv(paste0("../../Data/Raw_data/part3_MTurk_", my_version, ".csv"), sep=",", stringsAsFactors = F)
  data_t4_mturk <- read.csv(paste0("../../Data/Raw_data/part4_MTurk_", my_version, ".csv"), sep=",", stringsAsFactors = F)
  
  #data_t1time_mturk <- jsonlite::fromJSON(paste0(here("Data", "Raw_data", "part1_MTurk_itemdisplay_"), my_version, ".json"))
  
  ## add a suffix, so that variables do not mix when merged into one dataframe
    colnames(data_sc_mturk) <- paste(colnames(data_sc_mturk), "sc", sep=".")
    colnames(data_scP_mturk) <- paste(colnames(data_scP_mturk), "scP", sep=".")
    colnames(data_t1_mturk) <- paste(colnames(data_t1_mturk), "t1", sep=".")
    colnames(data_t2_mturk) <- paste(colnames(data_t2_mturk), "t2", sep=".")
    colnames(data_t3_mturk) <- paste(colnames(data_t3_mturk), "t3", sep=".")
    colnames(data_t4_mturk) <- paste(colnames(data_t4_mturk), "t4", sep=".")
    # colnames(data_t1time_mturk) <- paste(colnames(data_t1time_mturk), "t1", sep=".")

  ## the column name 'session' needs to be identical for merging 
    names(data_sc_mturk)[names(data_sc_mturk) == "session.sc"] <- "session"
    names(data_scP_mturk)[names(data_scP_mturk) == "session.scP"] <- "session"
    names(data_t1_mturk)[names(data_t1_mturk) == "session.t1"] <- "session"
    names(data_t2_mturk)[names(data_t2_mturk) == "session.t2"] <- "session"
    names(data_t3_mturk)[names(data_t3_mturk) == "session.t3"] <- "session"
    names(data_t4_mturk)[names(data_t4_mturk) == "session.t4"] <- "session"
    # names(data_t1time_mturk)[names(data_t1time_mturk) == "session.t1"] <- "session"

  # merge databases (except t1time, which comes later)
    data_tmp1_mturk <- data_sc_mturk %>% dplyr::left_join(data_scP_mturk, all=F, by="session")
    data_tmp2_mturk <- data_tmp1_mturk %>% dplyr::left_join(data_t1_mturk, all=F, by="session")
    data_tmp3_mturk <- data_tmp2_mturk %>% dplyr::left_join(data_t2_mturk, all=F, by="session")
    data_tmp4_mturk <- data_tmp3_mturk %>% dplyr::left_join(data_t3_mturk, all=F, by="session")
    data_raw_mturk <- data_tmp4_mturk %>% dplyr::left_join(data_t4_mturk, all=F, by="session") 
    
    for (i in cols_to_integer) {
      data_raw_mturk[ ,i] <- as.integer(data_raw_mturk[, i])
    }
    
    # data_t1time_mturk <- data_t1time_mturk[str_length(data_t1time_mturk$session) > 0L,]
    
    # for (i in cols_to_character) {
    #   data_t1time_mturk[ ,i] <- as.character(data_t1time_mturk[, i])
    # }
    
    data_raw_mturk <<- data_raw_mturk
    # data_t1time_mturk <<- data_t1time_mturk
  }



## CONVERT VARIABLES
convert_character_to_integer <- function(my_vars, data) {
  data[, my_vars] <- as.integer(as.character(data[, my_vars]))
}

convert_integer_to_character <- function(my_vars, data) {
  data[, my_vars] <- as.character(as.integer(data[, my_vars]))
}


## ANONYMISE


# ATTENTION CHECK
calculate_att_check_item_errors_t1 <- function(data = data, 
                                                 var1 = "att_check1.t1", var2 = "att_check2.t1") 
    {
    att_check_item_errors_t1 <- 
          ifelse(data[, var1] == "5" | is.na(data[, var1]), 0, 1) +
          ifelse(data[, var2] == "3" | is.na(data[, var2]), 0, 1) 
    return(att_check_item_errors_t1)
    }
  
 calculate_att_check_item_errors_t1_t4 <- function(data = data, 
                                                   var1 = "att_check1.t1", var2 = "att_check2.t1", 
                                                  var3 = "att_check1.t2", var4 = "att_check2.t2", 
                                                  var5 = "att_check1.t3", var6 = "att_check2.t3", 
                                                  var7 = "att_check1.t4", var8 = "att_check2.t4", 
                                                  var9 = "att_check3.t4") 
   {
   att_check_item_errors_t1_t4 <-
          ifelse(data[, var1] == "5" | is.na(data[, var1]), 0, 1) +
          ifelse(data[, var2] == "3" | is.na(data[, var2]), 0, 1) +
          ifelse(data[, var3] == "3" | is.na(data[, var3]), 0, 1) +
          ifelse(data[, var4] == "7" | is.na(data[, var4]), 0, 1) +
          ifelse(data[, var5] == "2" | is.na(data[, var5]), 0, 1) +
          ifelse(data[, var6] == "7" | is.na(data[, var6]), 0, 1) +
          ifelse(data[, var7] == "5" | is.na(data[, var7]), 0, 1) +
          ifelse(data[, var8] == "6" | is.na(data[, var8]), 0, 1) +
          ifelse(data[, var9] == "4" | is.na(data[, var9]), 0, 1) 
 return(att_check_item_errors_t1_t4)     
 }
    

calculate_att_check_video_errors <- function(data = data, vars=c( "att_check_videoGr1.t1", 
                                                                  "att_check_videoGr2.t1", 
                                                                  "att_check_videoGr3.t1", 
                                                                  "att_check_videoGr4.t1", 
                                                                  "att_check_videoGr5.t1", 
                                                                  "att_check_videoGr6.t1", 
                                                                  "att_check_videoGr7.t1", 
                                                                  "att_check_videoGr8.t1", 
                                                                  "att_check_videoGr9.t1")) {
  att_check_video <- 
    ifelse(data[, vars] == "2",    # "2" is always the correct solution
                                  FALSE, TRUE)
    att_check_video <- rowSums(att_check_video, na.rm=T)
    return(att_check_video)
  }




## TRIAL "Watch time" is defined as the difference between
# "video_trial" (displaying the video)
# and
# "check_test" (appears automatically following clicking on "finished watching",
# worded as Did you manage to watch the clip from the beginning to the end **with voice**?
calculate_vid1_difftime <-  function(data=data) {
  vid1_difftime <- data %>%
    select(session, item_name.t1, shown.t1) %>% 
    filter(item_name.t1 %in% c("check_test", "video_trial")) %>%
    spread(item_name.t1, shown.t1) %>%
    mutate(video_trial = as_datetime(video_trial), check_test = as_datetime(check_test),
           vid1_difftime = as.numeric(difftime(check_test, video_trial, unit = "secs"))) 
  return(vid1_difftime)
  }

## TEST watch time is defined as the time difference between
  # "prompt_play" (Which appears at the same time as the video)
  # and
  # "seen_before" which is an automatically displayed item following submission after the video
      # (Have you seen this movie before?)

calculate_vid2_difftime <- function (data=data) {
  vid2_difftime <- data %>%
    select(c("session", "item_name.t1", "shown.t1")) %>% 
    filter(item_name.t1 %in% c("seen_before", "prompt_play")) %>%
    spread(item_name.t1, shown.t1) %>%
    mutate(prompt_play = as_datetime(prompt_play), seen_before = as_datetime(seen_before),
           vid2_difftime = as.numeric(difftime(seen_before, prompt_play, unit = "secs")))
  return(vid2_difftime)
  }
 

# check if participants spent less time on the site, than the length of the video (=error)

## trial (Video1)
calculate_vid1_too_short <- function(data=data) {
  vid1_too_short <- data %>%  
     mutate(vid1_too_short = ifelse(vid1_difftime > 240 | is.na(vid1_difftime), 0, 1))
  return(vid1_too_short)
  }

## test (Video2)
calculate_vid2_too_short <- function(data=data) {
  vid2_too_short <-  data %>% 
    mutate(vid2_too_short =
              ifelse(Group.t1 == 1 & vid2_difftime > 126, 0, 
              ifelse(Group.t1 == 2 & vid2_difftime > 249, 0,
              ifelse(Group.t1 == 3 & vid2_difftime > 131, 0,
              ifelse(Group.t1 == 4 & vid2_difftime > 41, 0,
              ifelse(Group.t1 == 5 & vid2_difftime > 26, 0,
              ifelse(Group.t1 == 6 & vid2_difftime > 44, 0,
              ifelse(Group.t1 == 7 & vid2_difftime > 266, 0,
              ifelse(Group.t1 == 8 & vid2_difftime > 156, 0,
              ifelse(Group.t1 == 9 & vid2_difftime > 171, 0, 
              ifelse(is.na(vid2_difftime), 0, 1)))))))))))
  return(vid2_too_short)
  }


### PLOTTING

# baseline error plotting
plot_error_baseline <-  function (data=data) {
  par(mfrow=c(3, 2), mar=c(2, 2, 4, 2))
  with(data, {
    barplot(table(att_check_item_errors_t1), main="Errors on attention check items (T1)")
    barplot(table(vid1_too_short), main="Video1 (test) watched in too short time")
    barplot(table(vid2_too_short), main="Video2 (target) watched in too short time")
    barplot(table(att_check_video_errors), main="Errors on video content related \nattention check items")
    barplot(table(att_check_sum_t1), main="TOTAL NUMBER OF ERRORS, T1")
  title(main="BASELINE DATA", outer=T)
  })
}

# longitudinal error plotting
plot_error_longitudinal <- function (data=data) {
  par(mfrow=c(3, 2), mar=c(2, 2, 4, 2))
with(data, {
  barplot(table(att_check_item_errors_t1_t4), main="Errors on attention check items (T1-T4)")
  barplot(table(vid1_too_short), main="Video1 (test) watched in too short time")
  barplot(table(vid2_too_short), main="Video2 (target) watched in too short time")
  barplot(table(att_check_video_errors), main="Errors on video content related \nattention check items")
  barplot(table(att_check_sum_t1_t4), main="TOTAL NUMBER OF ERRORS, (T1-T4)")
  title(main="LONGITUDINAL DATA", outer=T)
  })
}


## DESCRIPTIVE STATISTICS

##  PLOTS
# plot_memo_bpd <- function (data=data, va1 = BPD, va2 = PANAS.t1) {
#   plot_bpd <-  ggplot(data, aes(x=va1, y=va2, color=Group_affect)) +
#     geom_point() + 
#     #geom_smooth()
#     theme_light() +
#     scale_color_manual(values= c("positive" = "red", "neutral" = "green", "negative" = "blue")) + 
#     labs(color = "Group affect", x="Borderline Personality Disorder", y="Affect (PANAS)")
#   return(plot_bpd)
# }
#   
# plot_memo_bpd()
  
  





