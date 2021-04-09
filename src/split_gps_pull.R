library(data.table)
library(timeDate)

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
                                            ][is.na(`Line Id`)
                                              ]

fb_mb_local_ridership <- fb_ridership_2020[!fb_rl_vehicle_validations
                                           , on = "Validation Id"
                                           ][!fb_rl_platform_validations
                                             , on = "Validation Id"
                                             ][,Local_Route := fcase(`Line Name` == "Red Line"
                                                                     ,fcase(Latitude < south_lat_boundary
                                                                            , "902"
                                                                            , Latitude > north_lat_boundary
                                                                            , "901"
                                                                            )
                                                                     , `Line Name` != "Red Line"
                                                                     , `Line Name`
                                                                     )
                                               ][]


gt::gt(
  fb_mb_local_ridership[,.N,.(Local_Route,Service_Type)][order(Local_Route,-Service_Type)]
)

fwrite(fb_mb_unknown_routes,"data//fb_unknown_route.csv")
fwrite(fb_mb_local_ridership,"data//fb_mb_ridership")

fwrite(fb_mb_local_ridership[,.N,.(Local_Route,Service_Type)][order(Local_Route,-Service_Type)],"data//flowbird_local_by_route_2020.csv")

#investigate teddy's data
teddy <- rbind(fread("data//flowbirddata2.csv")
               ,fread("data//flowbirddata3.csv"))

teddy
