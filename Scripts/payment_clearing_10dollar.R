# load data
source("./Scripts/functions_bpd_memo.R")
source("./Scripts/not_approved_screening_sessions.R")
getwd()
# load packages
data_raw <- read.csv2("./Data/Raw_data/data_raw_full_identifiable_180831.csv")
data_new_sessions <- read.csv2("./Data/Data_payment/payment_new_sessions.csv", sep = ",")

qual_t1 <- grep("qual.t1", colnames(data_raw),value=T)
qual_t2 <- grep("qual.t2", colnames(data_raw),value=T)
qual_t3 <- grep("qual.t3", colnames(data_raw),value=T)
qual_t4 <- grep("qual.t4", colnames(data_raw),value=T)

data_raw_payment <-  data_raw[, c("session", "voucher_yes_no.scP", "email.scP", qual_t1, qual_t2, qual_t3, qual_t4)]
data_new_sessions2 <- left_join(data_new_sessions, data_raw_payment)
write.csv2(data_new_sessions2, "./Data/Data_payment/data_raw_payment_full.csv")

