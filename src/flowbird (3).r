library(readxl)
library(openxlsx)
library(dplyr)
library(tidyr)
library(data.table)

VMH_StartTime = 20190901
VMH_EndTime = 20210201

avail_null_data <- fread("data//indygo_nulls_0919_to_0221.csv")
trans_dict <- read_excel("data//TransitAuthorityDictionary_CollectedData.xlsx", sheet=1)

dir("data")

get_VMH <- function(VMH_StartTime,VMH_EndTime) {
  require(odbc)
  con <- DBI::dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "REPSQLP01VW", 
                        Database = "TransitAuthority_IndyGo_Reporting", Port = 1433)
  vmh_daily <- tbl(con,sql(paste0(
    "SELECT Time, Vehicle_ID, Route, Trip,  from avl.Vehicle_Message_History WHERE Time >= '",VMH_StartTime,"'
    and Time <=('",VMH_EndTime,"')"))) %>% collect()
  return(vmh_daily)
}

vmh_daily <- get_VMH(VMH_StartTime,VMH_EndTime)

# Clear up more space for the operation
gc()
vmh_daily <- as.data.table(vmh_daily)
avail_null_data <- as.data.table(avail_null_data)

Transit_Day_Cutoff <- as.ITime("03:30:00")

vmh_daily[,
          DateTest := fifelse(data.table::as.ITime(Time) < Transit_Day_Cutoff, 1, 0) #test the days
          ][
            , Transit_Day := fifelse(DateTest == 1
                           , data.table::as.IDate(Time)-1
                           , data.table::as.IDate(Time)
                           )
            ]

avail_null_data[,
                DateTest := fifelse(data.table::as.ITime(generated_at) < Transit_Day_Cutoff, 1, 0) #test the days
                ][
                  , Transit_Day := fifelse(DateTest == 1
                                           ,data.table::as.IDate(generated_at)-1
                                           ,data.table::as.IDate(generated_at)
                                           )
                  ]

setnames(avail_null_data,"vehicle_external_id","Vehicle_ID")

avail_null_data[,jointime := fasttime::fastPOSIXct(generated_at,tz = "UTC")]
vmh_daily[,jointime := Time]

avail_null_data[Vehicle_ID==1]

setkey(vmh_daily,Vehicle_ID, Transit_Day, jointime)

setkey(avail_null_data, Vehicle_ID,Transit_Day, jointime)

null_data <- vmh_daily[avail_null_data,on = c(Vehicle_ID = "Vehicle_ID", Transit_Day="Transit_Day", jointime = "jointime"),roll = TRUE]

null_data$weekday <- weekdays(as.Date(null_data$Time))

null_data$diff <- null_data$Time - fasttime::fastPOSIXct(null_data$generated_at,tz = "UTC")
  
null_data <- as.data.frame(null_data)
null_num <- sum(is.na(null_data$Route))
good_data_percent <- (nrow(null_data)-null_num)/(nrow(null_data))

null_data[,.N,diff][order(N)]

fwrite(null_data,file="flowbirddata.csv")

null_data[c("diff","jointime","generated_at","Time","Transit_Day","Vehicle_ID")] %>% View()

null_data

%>% View()

