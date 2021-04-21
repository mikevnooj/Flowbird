library(readxl)
library(openxlsx)
library(dplyr)
library(tidyr)
library(data.table)
library(lubridate)

# Make sure to call gc() whenever you're low on memory
# Let's start by increasing our memory size

memory.size(60000)

# Set the date window for the vmh data pull
VMH_StartTime = 20200101
VMH_EndTime = 20200211

# Pull data and dictionary (in case it's needed)
avail_null_data <- read_excel("C:/Users/theodore.galanthay/Downloads/GFI_RevEng/GFI_RevEng/fb_unknown_route.xlsx", sheet=1)
trans_dict <- read_excel("C:/Users/theodore.galanthay/Downloads/GFI_RevEng/GFI_RevEng/TransitAuthorityDictionary_CollectedData.xlsx", sheet=1)

# This function opens and closes a connection, pulling data in between
get_VMH <- function(VMH_StartTime,VMH_EndTime) {
  require(odbc)
  con <- DBI::dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "REPSQLP01VW\\REPSQLP02",
                        Database = "TransitAuthority_IndyGo_Reporting", Port = 1433)
  vmh_daily <- tbl(con,sql(paste0(
    "SELECT Time, Vehicle_ID, Route, Trip,Server_time from avl.Vehicle_Message_History WHERE Time >= '",VMH_StartTime,"'
    and Time <= '",VMH_EndTime,"'"))) %>% collect()
  odbc::dbDisconnect(con)
  return(vmh_daily)
}

# Call the function
vmh_daily <- get_VMH(VMH_StartTime,VMH_EndTime)

# Clear up more space for the operation
gc()

# Transform data into data.tables
vmh_daily <- as.data.table(vmh_daily)
avail_null_data <- as.data.table(avail_null_data)

# Misc operations to add columns and set up date test function
Transit_Day_Cutoff <- as.ITime("03:30:00")
avail_null_data <- tidyr::separate(avail_null_data, "Date (Local TIme)", c("Date", "Time"), sep=" ", remove=FALSE)
avail_null_data$correctTime <- format(avail_null_data$Time,tz="America/Indiana/Indianapolis")
avail_null_data$Vehicle_ID<- avail_null_data$`Vehicle External Id`
avail_null_data$doubletime <- as.numeric(as.POSIXct(avail_null_data$`Date (Local TIme)`))
# Transit Day Date Test Function
avail_null_data[, DateTest := fifelse(data.table::as.ITime(`Time`) < Transit_Day_Cutoff, 1, 0) #test the days
][, Transit_Day := fifelse(
  DateTest == 1
  ,data.table::as.IDate(`Time`)-1
  ,data.table::as.IDate(`Time`)
)
]

# Filter big data.table into smaller data.tables
vmh_daily2 <- filter(vmh_daily, vmh_daily$Time <= "2020-02-01 00:00:00")
vmh_daily3 <- filter(vmh_daily, between(vmh_daily$Time, "2020-02-01 00:00:00", "2020-06-01 00:00:00"))
vmh_daily4 <- filter(vmh_daily, between(vmh_daily$Time, "2020-06-01 00:00:00", "2020-08-02 00:00:00"))
vmh_daily5 <- filter(vmh_daily, between(vmh_daily$Time, "2020-10-01 00:00:00", "2021-02-01 00:00:00"))

# Memory saving call
rm(vmh_daily)

# Preparing to join the VMH and AVAIL data
vmh_daily2$vmhTime <- format(vmh_daily2$Time,tz="America/Indiana/Indianapolis")
vmh_daily3$vmhTime <- format(vmh_daily3$Time,tz="America/Indiana/Indianapolis")
vmh_daily4$vmhTime <- format(vmh_daily4$Time,tz="America/Indiana/Indianapolis")
vmh_daily5$vmhTime <- format(vmh_daily5$Time,tz="America/Indiana/Indianapolis")

vmh_daily5$jointime <- vmh_daily5$vmhTime
vmh_daily2$jointime <- vmh_daily2$vmhTime
vmh_daily3$jointime <- vmh_daily3$vmhTime
vmh_daily4$jointime <- vmh_daily4$vmhTime
avail_null_data$jointime <- avail_null_data$Time

# Run the Transit Day Date Test for the VMH data
vmh_daily5[, DateTest := fifelse(data.table::as.ITime(jointime) < Transit_Day_Cutoff, 1, 0) #test the days
][, Transit_Day := fifelse(
  DateTest == 1
  ,data.table::as.IDate(jointime)-1
  ,data.table::as.IDate(jointime)
)
]
vmh_daily2[, DateTest := fifelse(data.table::as.ITime(jointime) < Transit_Day_Cutoff, 1, 0) #test the days
][, Transit_Day := fifelse(
  DateTest == 1
  ,data.table::as.IDate(jointime)-1
  ,data.table::as.IDate(jointime)
)
]
vmh_daily3[, DateTest := fifelse(data.table::as.ITime(jointime) < Transit_Day_Cutoff, 1, 0) #test the days
][, Transit_Day := fifelse(
  DateTest == 1
  ,data.table::as.IDate(jointime)-1
  ,data.table::as.IDate(jointime)
)
]
vmh_daily4[, DateTest := fifelse(data.table::as.ITime(jointime) < Transit_Day_Cutoff, 1, 0) #test the days
][, Transit_Day := fifelse(
  DateTest == 1
  ,data.table::as.IDate(jointime)-1
  ,data.table::as.IDate(jointime)
)
]

vmh_daily2$doubletime <- as.numeric(as.POSIXct(vmh_daily2$vmhTime))
vmh_daily3$doubletime <- as.numeric(as.POSIXct(vmh_daily3$vmhTime))
#Set keys for data.tables to join on
setkey(vmh_daily5, Vehicle_ID, Transit_Day, jointime)
setkey(vmh_daily2, Vehicle_ID, Transit_Day, doubletime)
setkey(vmh_daily3, Vehicle_ID, Transit_Day, doubletime)
setkey(vmh_daily4, Vehicle_ID, Transit_Day, doubletime)
setkey(avail_null_data, Vehicle_ID, Transit_Day, doubletime)

# Create the joined data sets
five_min <- 300
null_data <- vmh_daily5[avail_null_data, roll = T, rollends = c(T,T)]
null_data2 <- vmh_daily2[avail_null_data, roll = T, rollends = c(T,T)]#, on=c("Vehicle_ID", "Transit_Day", "jointime")]
null_data3 <- vmh_daily3[avail_null_data, roll = T, rollends = c(T,T)]#, on=c("Vehicle_ID", "Transit_Day", "jointime")]
null_data4 <- vmh_daily4[avail_null_data, roll = T, rollends = c(T,T)]#, on=c("Vehicle_ID", "Transit_Day", "jointime")]

# Add helpful columns
null_data$weekday <- weekdays(as.Date(null_data$Transit_Day))
null_data$diff <- as.numeric(as.POSIXct(null_data$vmhTime)) - as.numeric(as.POSIXct(null_data$jointime))
null_data2$weekday <- weekdays(as.Date(null_data2$Transit_Day))
null_data2$diff <- as.numeric(as.POSIXct(null_data2$vmhTime)) - as.numeric(as.POSIXct(null_data2$jointime))
null_data3$weekday <- weekdays(as.Date(null_data3$Transit_Day))
null_data3$diff <- as.numeric(as.POSIXct(null_data3$vmhTime)) - as.numeric(as.POSIXct(null_data3$jointime))
null_data4$weekday <- weekdays(as.Date(null_data4$Transit_Day))
null_data4$diff <- as.numeric(as.POSIXct(null_data4$vmhTime)) - as.numeric(as.POSIXct(null_data4$jointime))

# See how many rows in the joined columns have NAs
null_data$naData <- rowSums(is.na(null_data))
null_data2$naData <- rowSums(is.na(null_data2))
null_data3$naData <- rowSums(is.na(null_data3))
null_data4$naData <- rowSums(is.na(null_data4))

# Sort the joined data into data without nulls
goodData <-null_data[null_data$naData < 6, ]  
goodData2<-null_data2[null_data2$naData < 6, ]  
goodData3<-null_data3[null_data3$naData < 6, ]  
goodData4<-null_data4[null_data4$naData < 6, ]

# Doesn't work for all data.table good data tables yet
null_num <- sum(is.na(null_data$Route))
good_data_percent <- (nrow(null_data)-null_num)/(nrow(null_data))

fwrite(null_data,file="flowbirddata.csv")
fwrite(null_data2,file="flowbirddata2.csv")
fwrite(null_data3,file="flowbirddata3.csv")
fwrite(null_data4,file="flowbirddata4.csv")
