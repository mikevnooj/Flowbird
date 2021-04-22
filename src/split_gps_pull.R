library(data.table)
library(timeDate)
library(magrittr)

x <- fread("data//tableau_validations_20190901_to_20210405.csv")

#get everything that counts as ridership
fb_ridership <- x[
  !is.na(`Media Id`)
  ][
    Result == 1
    ][
        , Time := lubridate::mdy_hms(`Date (Local TIme)`)
        ][order(Time)
            ][!`Device External Id` %like% "ATB"]




#do transit day

Transit_Day_Cutoff <- as.ITime("03:30:00")
  
  
fb_ridership[, DateTest := fifelse(data.table::as.ITime(Time) < Transit_Day_Cutoff
                                  , 1
                                  , 0
                                  ) #end fifelse()
             ][
               , Transit_Day := fifelse(DateTest == 1
                                        , data.table::as.IDate(Time)-1
                                        , data.table::as.IDate(Time)
                                        )
               ]

fb_ridership_2020 <- fb_ridership[Transit_Day >= "2020-01-01" & 
                                    Transit_Day < "2021-01-01"]


#set service type
holidays_sunday_service <- c("USNewYearsDay", "USMemorialDay",
                             "USIndependenceDay", "USLaborDay",
                             "USThanksgivingDay", "USChristmasDay")

holidays_saturday_service <- c("USMLKingsBirthday")

#set sat sun
holidays_sunday <- holiday(2000:2025, holidays_sunday_service)
holidays_saturday <- holiday(2000:2025, holidays_saturday_service)

#set service type column
fb_ridership_2020[
  ,Service_Type := fcase(Transit_Day %in% as.IDate(holidays_saturday@Data)
                         , "Saturday"
                         , Transit_Day %in% as.IDate(holidays_sunday@Data)
                         , "Sunday"
                         , weekdays(Transit_Day) %in% c("Monday"
                                                        , "Tuesday"
                                                        , "Wednesday"
                                                        , "Thursday"
                                                        , "Friday"
                         )#end c
                         , "Weekday"
                         , weekdays(Transit_Day) == "Saturday"
                         , "Saturday"
                         ,weekdays(Transit_Day) == "Sunday"
                         , "Sunday"
  )#end fcase 
  ]



north_lat_boundary <- 39.877512
south_lat_boundary <- 39.709468
garage_boundary <- -86.173321

#get validations on red line vehicles inside the bounds
fb_rl_vehicle_validations <- fb_ridership_2020[`Device External Id` %like% "ABB"
                                               ][Latitude < north_lat_boundary &
                                                   Latitude > south_lat_boundary &
                                                   Longitude > garage_boundary
                                                 ]

#these are the only inaccurate rows
fb_ridership_2020[`Device External Id` %like% "ABB"
             ][is.na(Latitude) | is.na(Longitude)]

fb_rl_platform_validations <- fb_ridership_2020[`Device External Id` %like% "APV"]

fb_mb_unknown_routes <- fb_ridership_2020[!fb_rl_vehicle_validations
                                          , on = "Validation Id"
                                          ][!fb_rl_platform_validations
                                            , on = "Validation Id"
                                            ][`Line Name` == "" | `Line Name` == "Red Line"]


fb_mb_local_ridership <- fb_ridership_2020[!fb_rl_vehicle_validations
                                           , on = "Validation Id"
                                           ][
                                             !fb_rl_platform_validations
                                             , on = "Validation Id"
                                             ][
                                               !fb_ridership_2020[`Line Name` == "Red Line" &
                                                                    Longitude < garage_boundary
                                                                  ]
                                               , on = "Validation Id"
                                               ][
                                                 ,Local_Route := fcase(`Line Name` == "Red Line"
                                                                       ,fcase(Latitude < south_lat_boundary
                                                                              , "902"
                                                                              , Latitude > north_lat_boundary
                                                                              , "901"
                                                                              )
                                                                       , `Line Name` != "Red Line"
                                                                       , `Line Name`
                                                                       )
                                                 ]

gt::gt(
  fb_mb_local_ridership[,.N,.(Local_Route,Service_Type)][order(Local_Route,-Service_Type)]
)

fwrite(fb_mb_unknown_routes,"data//fb_unknown_route.csv")
fwrite(fb_mb_local_ridership,"data//fb_mb_ridership")

fwrite(fb_mb_local_ridership[
  , .N
  , .(Local_Route
      , Service_Type
      )
  ][order(Local_Route
          , -Service_Type
           )
    ]
  ,"data//flowbird_local_by_route_2020.csv"
  )

#let's time match the vmh data

#first we'll grab the flowbird stuff
#add jointime column

start_date <- as.POSIXct("2020/07/02", tz = "UTC")
end_date <- as.POSIXct("2021/01/01", tz = "UTC")


fb_mb_unknown_routes <- fread("data//fb_unknown_route.csv")

fb_mb_unknown_routes <- fb_mb_unknown_routes[Transit_Day >= start_date & Transit_Day <= end_date]

gc()

fb_mb_unknown_routes[,jointime := fasttime::fastPOSIXct(Time, tz = "UTC")]

setnames(fb_mb_unknown_routes, c("Vehicle External Id"), c("Vehicle_ID"))

setkey(fb_mb_unknown_routes, Vehicle_ID, Transit_Day, jointime)
gc()
#now we'll grab vmh
#### VMH_Import ####
con_dw <- DBI::dbConnect(odbc::odbc()
                         , Driver = "SQL Server"
                         , Server = "REPSQLP01VW\\REPSQLP02"
                         , Database = "TransitAuthority_IndyGo_Reporting"
                         , Port = 1433)

#we'll need these for our query of the database
#VMH_StartTime <- start_date + (60*60*3)
# get start time in yyyymmdd with no spaces
start_date <- as.POSIXct("2020/07/02", tz = "UTC")
end_date <- as.POSIXct("2021/01/01", tz = "UTC")

VMH_StartTime <- stringr::str_remove_all(start_date,"-")
VMH_EndTime <- stringr::str_remove_all(end_date,"-")

gc()
#paste0 the query
VMH_Raw <- dplyr::tbl(
  con_dw, dplyr::sql(
    paste0(
      "select a.Time
    ,a.Route
    ,Trip
    ,Vehicle_ID
    ,Stop_Name
    ,Stop_Id
    ,Latitude
    ,Longitude
    ,GPSStatus
    from avl.Vehicle_Message_History a (nolock)
    left join avl.Vehicle_Avl_History b
    on a.Avl_History_Id = b.Avl_History_Id
    where a.Time > '",VMH_StartTime,"'
    and a.Time < DATEADD(day,1,'",VMH_EndTime,"')"
    )#end paste
  )#endsql
) %>% #end tbl
  dplyr::collect()

gc()


#set the DT and keys
setDT(VMH_Raw)

gc()

#### VMH CLEANING ####
# do transit day

VMH_Raw[, DateTest := fifelse(data.table::as.ITime(Time) < Transit_Day_Cutoff
                              , 1
                              , 0
                              ) #end fifelse()
        ][
          , Transit_Day := fifelse(DateTest == 1
                                   , data.table::as.IDate(Time)-1
                                   , data.table::as.IDate(Time)
                                   )
          ]
gc()

VMH_Raw[, jointime := Time]



#clean up the columns to save space
VMH_Raw[,c("Inbound_Outbound",
           "Departure_Time",
           "Boards",
           "Alights",
           "DateTest"):=NULL]

VMH_Raw[,Vehicle_ID := as.integer(Vehicle_ID)]





gc()

setkey(VMH_Raw, Vehicle_ID, Transit_Day, jointime)

gc()

rolljointable <- VMH_Raw[Transit_Day >= start_date & 
                           Transit_Day <= end_date
                         ][fb_mb_unknown_routes[Transit_Day >= start_date & Transit_Day <= end_date]
                           , on = c(Vehicle_ID = "Vehicle_ID"
                                    , Transit_Day="Transit_Day"
                                    , jointime = "jointime"
                                    )
                           , roll=T
                           , rollends = c(T, T)
                           ] %>%
  dplyr::select(VMH_Time = Time,
                fb_Time = `Date (Local TIme)`,
                jointime,
                Vehicle_ID,
                everything())

gc()

#### VALIDATE RESULTS ####
#set max times
#

rolljointable[VMH_Raw[, max(Time)
                      , .(Vehicle_ID,Transit_Day)
                      ]
              , on = .(Vehicle_ID,Transit_Day)
              ,`:=` (maxtime = V1)
              ]


#add timediffs
rolljointable[,timediffs := as.difftime(jointime - VMH_Time
                                        ,units = "secs"
                                        )
              ]

rolljoin_one_each <- rbind(
rolljointable[rolljointable[, .I[which.min(abs(timediffs))], `Validation Id`]$V1]
, rolljointable[is.na(timediffs)]
)

fwrite(rolljoin_one_each,"data//roll_join_one_each_2.csv")

rolljoin_one_each <- rbind(
  fread("data//roll_join_one_each_1.csv")
  ,fread("data//roll_join_one_each_2.csv")
  ,fill = TRUE
)


#find how many TD's report after the final VMH, meaning a transaction after location reporting stopped
fb_After_VMH <- rolljoin_one_each[jointime > maxtime]

#find how many TD's are between start and end but have a large gap, here we've used 1.5 minutes, in seconds
fb_gap_data <- rolljoin_one_each[(jointime < maxtime & 
                                abs(timediffs) > 60*1.5) | is.na(timediffs)]


#find how many TD's had bad GPS data
fb_Bad_GPS <- rolljoin_one_each[GPSStatus != 2 | is.na(GPSStatus)
                            ][jointime < maxtime
                              ][abs(timediffs) < 60*1.5][
                                !is.na(timediffs)
                              ]



fb_Questionable <- rbind(fb_After_VMH,fb_gap_data,fb_Bad_GPS)

fb_Questionable_PCT <- fb_Questionable[,.N/nrow(rolljoin_one_each)]

fb_Questionable_PCT

fb_Good <- rolljointable[!fb_Questionable, on = c("Validation Id")]

fb_Good[,.N,Route]


#investigate no matches
merge.data.table(rolljoin_one_each[Transit_Day >= start_date &
                                     Transit_Day <= end_date
                                   ][is.na(Route)
                                     ][, .N
                                       , .(Vehicle_ID,Transit_Day)
                                       ]
                 ,VMH_Raw[, .N
                          , .(Vehicle_ID
                              , Transit_Day
                              )
                          ]
                 ,by = c("Vehicle_ID","Transit_Day")
                 ,all.x = TRUE
                 )

[,.(min(Transit_Day)
                      , max(Transit_Day)
                      )
                   , Vehicle_ID]
#so these boardings are attributed to vehicles that were disposed before the dates on which 

VMH_Raw[Vehicle_ID == 213]

rolljoin_one_each[Vehicle_ID == 102][order(Transit_Day)]

rolljoin_one_each[,.N,Route]
22272+2199+2832
27303/59934



rolljoin_one_each[Route == 999 | Route == 0][,.N,.(Vehicle_ID,Transit_Day)][,view(.SD)]

#okay let's look at 2710 on 2020-11-12
rolljoin_one_each[Vehicle_ID == 2710 & Transit_Day == "2020-11-12"]

VMH_Raw[Vehicle_ID == 2710 & Transit_Day == "2020-11-12"][,view(.SD)]

#2706 2020-08-17
rolljoin_one_each[Vehicle_ID == 2706 & Transit_Day >= "2020-08-15" & Transit_Day <= "2020-08-18"] %>%
leaflet::leaflet() %>%
leaflet::addCircles() %>%
leaflet::addTiles()

fb_mb_unknown_routes[Vehicle_ID == 2706 & Transit_Day >= "2020-08-15" & Transit_Day <= "2020-08-18"] %>%
  leaflet::leaflet() %>%
  leaflet::addCircles() %>%
  leaflet::addTiles()


x <- fread("data//fb_mb_ridership")

y <- fread("data//tableau_validations_20190901_to_20210405.csv")

media_search <- fb_mb_unknown_routes[Vehicle_ID == 2706 & Transit_Day >= "2020-08-15" & Transit_Day <= "2020-08-18"][,unique(`Media Id`)]

x[`Media Id` %in% media_search] %>%
leaflet::leaflet() %>%
leaflet::addCircles() %>%
leaflet::addTiles()

y[`Vehicle External Id` == 2706][lubridate::mdy_hms(`Date (Local TIme)`) >= lubridate::mdy_hms("08/14/2020 01:00") & 
                                   lubridate::mdy_hms(`Date (Local TIme)`) <= lubridate::mdy_hms("08/18/2020 05:00")] %>% 
leaflet::leaflet() %>%
leaflet::addCircles() %>%
leaflet::addTiles()


VMH_Raw[Vehicle_ID == 2706 & Transit_Day == "2020-08-17"][,View(.SD)]


y[Latitude == 0 | Longitude == 0][,.N,Result]

x[Latitude == 0 | Longitude == 0 | is.na(Latitude) | is.na(Longitude)][,.N]/nrow(x)

y[Latitude == 0 | Longitude == 0 | is.na(Latitude) | is.na(Longitude)][,.N]/nrow(y)

# ips quick and dirty -----------------------------------------------------


ips <- fread("data//fb_mb_ridership")

ips[,.N,.(`Program Id`,`Program Name`)][order(`Program Id`)]

ips[!`Program Id` %in% c(1,2,3,7,29,30,31)][,uniqueN(`Media Id`)][,.N]

ips[`Program Id` %in% c(13
                        , 19
                        , 10
                        , 12
                        , 11
                        , 6
) & 
  Service_Type == "Weekday"
][,uniqueN(`Media Id`)]

ips[`Program Id` %in% c(13
                        , 19
                        , 10
                        , 12
                        , 11
                        , 6
                        ) & 
      Service_Type == "Weekday"
    ] %>% ggplot2::ggplot(ggplot2::aes(x = hour(Time))) +
  ggplot2::geom_histogram(binwidth = 1) +
  ggplot2::facet_wrap(~ `Program Name`)

ips[`Program Id` %in% c(13
                        , 19
                        , 10
                        , 12
                        , 11
                        , 6
                        ) & 
      Service_Type == "Weekday"
    ][,.(boardings = .N)
      ,.(program = `Program Name`
         ,hour = hour(Time)
         ,Transit_Day)
      ][, .(avg_boardings = mean(boardings))
        , .(program
            , hour)
        ] %>%
  ggplot2::ggplot(ggplot2::aes(x = hour
                               , y = avg_boardings
                               )
                  ) +
  ggplot2::geom_col() + 
  ggplot2::facet_wrap(~program) + 
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 270))

merge.data.table(ips[`Program Id` %in% c(13
                                         , 19
                                         , 10
                                         , 12
                                         , 11
                                         ) & 
                       Service_Type == "Weekday"
                     ][,.(boardings = .N)
                       ,.(program = `Program Name`
                          ,hour = paste0(hour(Time),":00")
                          ,Transit_Day)
                       ][order(program
                               , Transit_Day
                               , hour)
                         ]
                 ,hourseq
                 ,all.y = T
                 )




hourseq <- data.table(time = seq(as.POSIXct("2020-01-01 00:00:00")
                          , as.POSIXct("2020-12-31 24:00:00")
                          , by="hour")
                      )

hourseq[, DateTest := fifelse(data.table::as.ITime(time) < Transit_Day_Cutoff
                              , 1
                              , 0
                              ) #end fifelse()
        ][
          , Transit_Day := fifelse(DateTest == 1
                                   , data.table::as.IDate(time)-1
                                   , data.table::as.IDate(time)
                                   )
          ][,
            hour := hour(time)
            ][,.(time := NULL
                 ,DateTest := NULL
                 )
              ][]

ips[,uniqueN()]

merge.data.table(ips[`Program Id` %in% c(13
                                         , 19
                                         , 10
                                         , 12
                                         , 11
) & 
  Service_Type == "Weekday"
][,.(boardings = .N)
  ,.(program = `Program Name`
     ,hour = paste0(hour(Time),":00")
     ,Transit_Day)
  ][order(program
          , Transit_Day
          , hour)
    ]
,hourseq
,all.y = T
,by = c("Transit_Day","hour")
)


# IPS Jan/Feb Analysis ----------------------------------------------------
#get dates
start_date <- as.POSIXct("2020/01/01", tz = "UTC")
end_date <- as.POSIXct("2020/03/01", tz = "UTC")

# find ips programs
fb_ridership[, .N
             , .(`Program Id`
                 , `Program Name`
                 )
             ][order(`Program Id`)]



# okay we're looking for 4,5,6,10,11,12,13,18,19,20,33,34,37,39

ips_programs <- c(4,5,6,10,11,12,13,18,19,20,33,34,37,39)

fb_ridership_ips <- fb_ridership[Transit_Day >= start_date& 
                                    Transit_Day < end_date & 
                                   `Program Id` %in% ips_programs]

fb_ridership_ips[,jointime := fasttime::fastPOSIXct(Time, tz = "UTC")]

setnames(fb_ridership_ips, c("Vehicle External Id","Latitude","Longitude"), c("Vehicle_ID","fb_lat","fb_lon"))

setkey(fb_ridership_ips, Vehicle_ID, Transit_Day, jointime)

#set service type
holidays_sunday_service <- c("USNewYearsDay", "USMemorialDay",
                             "USIndependenceDay", "USLaborDay",
                             "USThanksgivingDay", "USChristmasDay")

holidays_saturday_service <- c("USMLKingsBirthday")

#set sat sun
holidays_sunday <- holiday(2000:2025, holidays_sunday_service)
holidays_saturday <- holiday(2000:2025, holidays_saturday_service)

#set service type column
fb_ridership_ips[
  ,Service_Type := fcase(Transit_Day %in% as.IDate(holidays_saturday@Data)
                         , "Saturday"
                         , Transit_Day %in% as.IDate(holidays_sunday@Data)
                         , "Sunday"
                         , weekdays(Transit_Day) %in% c("Monday"
                                                        , "Tuesday"
                                                        , "Wednesday"
                                                        , "Thursday"
                                                        , "Friday"
                         )#end c
                         , "Weekday"
                         , weekdays(Transit_Day) == "Saturday"
                         , "Saturday"
                         ,weekdays(Transit_Day) == "Sunday"
                         , "Sunday"
  )#end fcase 
  ]


# pull vmh for ips --------------------------------------------------------

con_rep <- DBI::dbConnect(odbc::odbc()
                         , Driver = "SQL Server"
                         , Server = "REPSQLP01VW\\REPSQLP02"
                         , Database = "TransitAuthority_IndyGo_Reporting"
                         , Port = 1433)

#we'll need these for our query of the database
#VMH_StartTime <- start_date + (60*60*3)
# get start time in yyyymmdd with no spaces


VMH_StartTime <- stringr::str_remove_all(start_date,"-")
VMH_EndTime <- stringr::str_remove_all(end_date,"-")

#paste0 the query
VMH_Raw <- dplyr::tbl(
  con_rep, dplyr::sql(
    paste0(
      "select a.Time
    ,a.Route
    ,Trip
    ,Boards
    ,Alights
    ,Onboard
    ,Vehicle_ID
    ,Stop_Name
    ,Stop_Id
    ,Latitude
    ,Longitude
    ,GPSStatus
    ,Previous_Stop_Id
    from avl.Vehicle_Message_History a (nolock)
    left join avl.Vehicle_Avl_History b
    on a.Avl_History_Id = b.Avl_History_Id
    where a.Time > '",VMH_StartTime,"'
    and a.Time < DATEADD(day,1,'",VMH_EndTime,"')"
    )#end paste
  )#endsql
) %>% #end tbl
  dplyr::collect()

gc()


#set the DT and keys
setDT(VMH_Raw)



#### VMH CLEANING ####
# do transit day

VMH_Raw[, DateTest := fifelse(data.table::as.ITime(Time) < Transit_Day_Cutoff
                              , 1
                              , 0
                              ) #end fifelse()
        ][
          , Transit_Day := fifelse(DateTest == 1
                                   , data.table::as.IDate(Time)-1
                                   , data.table::as.IDate(Time)
                                   )
          ]
gc()

VMH_Raw[, jointime := Time]



#clean up the columns to save space
VMH_Raw[,c("DateTest"):=NULL]

VMH_Raw[,Vehicle_ID := as.integer(Vehicle_ID)]





gc()

setkey(VMH_Raw, Vehicle_ID, Transit_Day, jointime)

rolljointable_ips <- VMH_Raw[Transit_Day >= start_date & 
                           Transit_Day <= end_date
                         ][fb_ridership_ips[Transit_Day >= start_date & Transit_Day <= end_date]
                           , on = c(Vehicle_ID = "Vehicle_ID"
                                    , Transit_Day="Transit_Day"
                                    , jointime = "jointime"
                           )
                           , roll=T
                           , rollends = c(T, T)
                           ] %>%
  dplyr::select(VMH_Time = Time,
                fb_Time = `Date (Local TIme)`,
                jointime,
                Vehicle_ID,
                everything())

#set max times
#

rolljointable_ips[VMH_Raw[, max(Time)
                      , .(Vehicle_ID,Transit_Day)
                      ]
              , on = .(Vehicle_ID,Transit_Day)
              ,`:=` (maxtime = V1)
              ]


#add timediffs
rolljointable_ips[,timediffs := as.difftime(jointime - VMH_Time
                                        ,units = "secs"
)
]

rolljoin_one_each_ips <- rbind(
  rolljointable_ips[rolljointable_ips[, .I[which.min(abs(timediffs))], `Validation Id`]$V1]
  , rolljointable_ips[is.na(timediffs)]
)

#find how many TD's report after the final VMH, meaning a transaction after location reporting stopped
fb_After_VMH_ips <- rolljoin_one_each_ips[jointime > maxtime+60*1.5]



#find how many TD's are between start and end but have a large gap, here we've used 1.5 minutes, in seconds
fb_gap_data_ips <- rolljoin_one_each_ips[(jointime < maxtime+60*1.5 & 
                                    abs(timediffs) > 60*1.5) | is.na(timediffs)]


#find how many TD's had bad GPS data
fb_Bad_GPS_ips <- rolljoin_one_each_ips[GPSStatus != 2 | is.na(GPSStatus)
                                ][jointime < maxtime+60*1.5
                                  ][abs(timediffs) < 60*1.5][
                                    !is.na(timediffs)
                                    ]



fb_Questionable_ips <- rbind(fb_After_VMH_ips,fb_gap_data_ips,fb_Bad_GPS_ips)

fb_Questionable_PCT_ips <- fb_Questionable_ips[,.N/nrow(rolljoin_one_each_ips)]

fb_Questionable_PCT_ips

fb_Good_ips <- rolljoin_one_each_ips[!fb_Questionable_ips, on = c("Validation Id")]

fb_Good_ips[,.N,Route]

fb_Good_ips[Route == 0] %>%
leaflet::leaflet() %>%
leaflet::addCircles() %>%
leaflet::addTiles()

#crispus - mlk and 11th
rolljoin_one_each_ips[Stop_Name %ilike% "mlk& 11th"] %>%
leaflet::leaflet() %>%
leaflet::addCircles() %>%
leaflet::addTiles()


# test some gps radius stuff ----------------------------------------------
rolljoin_one_each_ips[Transit_Day == "2020-01-06" & Route == 15][order(jointime)]%>%
leaflet::leaflet() %>%
leaflet::addCircles() %>%
leaflet::addTiles()


# Get DimStop for Lat/Lon -------------------------------------------------

con_dw <- DBI::dbConnect(odbc::odbc()
                         , Driver = "SQL Server"
                         , Server = "AVAILDWHP01VW"
                         , Database = "DW_IndyGo"
                         , Port = 1433)

  
DimStop <- dplyr::tbl(con_dw
               , "DimStop"
               ) %>%
  dplyr::select(StopKey
                ,StopExternalID
                , StopReportLabel
                , StopDesc
                , Latitude
                , Longitude
                ) %>%
  dplyr::collect() %>%
  setDT(key = "StopKey")

DimStop_just_one <- DimStop[DimStop[,.I[which.max(StopKey)],StopExternalID]$V1]

DimStop[StopDesc %ilike% "Meridian St & 11"]
# Crispus Attucks Stops ---------------------------------------------------
#program Id is 13
#weekday service type
#stops are 50902, 51059, 51260

Crispus_stops <- c(50902
                   , 51059
                   , 50901
                   , 51260
                   , 51347
                   , 10143
                   , 50390
                   , 14207
                   , 50905
                   , 51056
                   , 12108
                   , 12112
                   , 50005
                   )


DimStop_just_one[StopExternalID %in% Crispus_stops] %>%
leaflet::leaflet() %>%
leaflet::addCircles() %>%
leaflet::addTiles()

DimStop_just_one[StopExternalID %in% Crispus_stops] %>%
  leaflet::leaflet() %>%
  leaflet::addCircles() %>%
  leaflet::addTiles()


rolljoin_one_each_ips[`Program Id` == 13
                      ][#filter some wacky shit in australia
                        Longitude < -77.03
                        ] %>%
leaflet::leaflet() %>%
leaflet::addCircles() %>%
leaflet::addTiles()
  
  

  



RANN::nn2()
