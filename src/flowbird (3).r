library(readxl)
library(openxlsx)
library(dplyr)
library(tidyr)
library(data.table)

VMH_StartTime = 20190901
VMH_EndTime = 20210201

avail_null_data <- read_excel("data//indygo_nulls_0919_to_0221.xlsx", sheet=1)
trans_dict <- read_excel("data//TransitAuthorityDictionary_CollectedData.xlsx", sheet=1)

dir("data")

get_VMH <- function(VMH_StartTime,VMH_EndTime) {
  require(odbc)
  con <- DBI::dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "REPSQLP01VW", 
                        Database = "TransitAuthority_IndyGo_Reporting", Port = 1433)
  vmh_daily <- tbl(con,sql(paste0(
    "SELECT Time, Vehicle_ID, Route, Trip,Server_time from avl.Vehicle_Message_History WHERE Time >= '",VMH_StartTime,"'
    and Time <=('",VMH_EndTime,"')"))) %>% collect()
  return(vmh_daily)
}

vmh_daily <- get_VMH(VMH_StartTime,VMH_EndTime)

# Clear up more space for the operation
gc()
vmh_daily <- as.data.table(vmh_daily)
avail_null_data <- as.data.table(avail_null_data)

vmh_daily[, DateTest := fifelse(data.table::as.ITime(ClockTime) < Transit_Day_Cutoff, 1, 0) #test the days
][, Transit_Day := fifelse(
  DateTest == 1
  ,data.table::as.IDate(Date)-1
  ,data.table::as.IDate(Date)
)
]

setnames(avail_null_data,"vehicle_id","Vehicle_ID")
avail_null_data$Time <- avail_null_data$generated_at

setkey(vmh_daily,Time,Vehicle_ID)
setkey(avail_null_data,Time,Vehicle_ID)

null_data <- vmh_daily[avail_null_data,roll = T,rollends=c(T,T)]
null_data$weekday <- weekdays(as.Date(null_data$Time))
null_data$diff <- null_data$Time - null_data$generated_at
  
null_data <- as.data.frame(null_data)
null_num <- sum(is.na(null_data$Route))
good_data_percent <- (nrow(null_data)-null_num)/(nrow(null_data))

fwrite(null_data,file="flowbirddata.csv")
