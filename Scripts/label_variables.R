# This script labels variables


cols_to_factor <- c("gender.sc", "gender.t1", "edu_years_compl.t1", "edu_highest.t1", "SES_subj.t1", "national.t1", 
                    "work.t1", "study.t1", "relationship.t1", "relationship_qual.t1", 
                    "English_first.sc", "English_level.sc", "English_first.t1", "English_level.t1", 
                    "medication.t1", "diagn_bpd.t1")
for (i in cols_to_factor) {
  data_final[, i] <- as.factor(data_final[, i]) 
}

levels(data_final_bl$gender.sc) <- c("1"="female", "2"="male", "3"="other", "4"="I don't want to tell")
levels(data_final_bl$gender.t1) <- c("1"="female", "2"="male", "3"="other", "4"="I don't want to tell")
levels(data_final_bl$edu_highest.t1) <- c("0"="No formal education", "1"="Primary school (or equivalent)", 
                                          "2"="Secondary/high school (or equivalent)", "3"="Vocational school (or equivalent)", 
                                          "4"="Undergraduate school/BA degree (or equivalent)", "5"="Graduate school/MA degree (or equivalent)", 
                                          "6"="PhD/MBA or similar")
levels(data_final_bl$edu_years_compl.t1) <- c("1" = "4 years or less",	"2" =	"5-8 years", 	"3" =	"9-12 years", 
                                              "4" = "12-17 years", "5" = "17-20 years", "6" =	"more than 20 years")
levels(data_final_bl$edu_highest.t1) <- c("0" = "No formal education", "1" =	"Primary school (or equivalent)", 
                                          "2" =	"Secondary/high school (or equivalent)", "3" =	"Vocational school (or equivalent)", 
                                          "4" =	"Undergraduate school/BA degree (or equivalent)", "5" =	"Graduate school/MA degree (or equivalent)", 
                                          "6" =	"PhD/MBA or similar")
levels(data_final_bl$SES_subj.t1) <- c("poorest" =	"Among the poorest", "poorer" =	"Poor", "poor" = "Below average", 
                                       "average" = "Average", 
                                       "rich" = "Above the average",  "richer" =	"Wealthy", "richest"	= "Among the wealthiest")
levels(data_final_bl$work.t1) <- c("1" = "yes, full-time", "2" = "yes, part-time", "3" = "yes, less than part time", "4" = "no")
levels(data_final_bl$study.t1) <- c("1" = "no", "2" = "yes")
levels(data_final_bl$relationship.t1) <- c("1" = "yes", "2" = "no", "3" = "other", "4" = "I don't want to tell")
levels(data_final_bl$relationship_qual.t1) <- c("1" = "married", "2" = "living together", "3" = "not living together", "4" = "I don't want to tell")
levels(data_final_bl$English_first.sc) <- c("1" = "yes", "2" = "no")
levels(data_final_bl$English_first.t1) <- c("1" = "yes", "2" = "no")
levels(data_final_bl$English_level.sc) <- c("1" = "very strong (close to native)", "2" = "strong (fluent)", "3" = "medium (I speak okay)", 
                                            "4" = "weak (I only speak a little)", "5" = "very weak" )
levels(data_final_bl$English_level.t1) <- c("1" = "very strong (close to native)", "2" = "strong (fluent)", "3" = "medium (I speak okay)", 
                                            "4" = "weak (I only speak a little)", "5" = "very weak" )
levels(data_final_bl$medication.t1) <- c("1" = "Yes", "2" = "No")
levels(data_final_bl$diagn_bpd.t1) <- c("1" = "Yes", "2" = "No")
