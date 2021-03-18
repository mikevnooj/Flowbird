library(readxl)
library(openxlsx)
library(dplyr)
library(tidyr)
library(data.table)


# import ------------------------------------------------------------------

VMH_StartTime <- 20201116

avail_null_data <- read_excel("data//avail_nulls_20201116.xlsx", sheet=1)
trans_dict <- read_excel("data//TransitAuthorityDictionary_CollectedData.xls", sheet=1)

GetVMH <- function(VMH_StartTime) {
  require(odbc)
  con <- DBI::dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "REPSQLP01VW", 
                        Database = "TransitAuthority_IndyGo_Reporting", Port = 1433)
  
  vmh_daily <- tbl(con,sql(paste0(
    "SELECT Time, Vehicle_ID, Route, Trip,Server_time from avl.Vehicle_Message_History WHERE Time > '",VMH_StartTime,"'
    and Time < DATEADD(day,1,'",VMH_StartTime,"')"))) %>% collect()
  
  return(vmh_daily)
}

vmh_daily <- GetVMH(VMH_StartTime = VMH_StartTime)


# clean -------------------------------------------------------------------

vmh_daily <- tidyr::separate(vmh_daily, Time, c("Date", "ClockTime"), sep=" ", remove=FALSE)
avail_null_data <- tidyr::separate(avail_null_data, generated_at, c("Date", "ClockTime"), sep=" ", remove=FALSE)


vmh_daily <- as.data.table(vmh_daily)
avail_null_data <- as.data.table(avail_null_data)
setnames(avail_null_data, "vehicle_id", "Vehicle_ID")


Transit_Day_Cutoff <- as.ITime("03:30:00")


vmh_daily[, DateTest := fifelse(data.table::as.ITime(ClockTime) < Transit_Day_Cutoff, 1, 0) #test the days
          ][, Transit_Day := fifelse(DateTest == 1, #set the days
                                    lubridate::as_date(Date) - 1,
                                    lubridate::as_date(Date))
            ][, Transit_Day := lubridate::as_date("1970-01-01") + lubridate::days(Transit_Day)#convert the days
              ][
                ,dt_td_test := fifelse(
                  DateTest == 1
                  ,data.table::as.IDate(Date)-1
                  ,data.table::as.IDate(Date)
                )
              ]

all(vmh_daily$Transit_Day == vmh_daily$dt_td_test)



setkey(vmh_daily,join_time,Vehicle_ID)
setkey(avail_null_data,join_time,Vehicle_ID)

null_data <- vmh_daily[avail_null_data,roll = T,rollends=c(T,T)]
