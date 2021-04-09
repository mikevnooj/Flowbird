library(data.table)

BYD_all <- fread("data//BYD_All.csv")
BYD_Red_Line <- fread("data//BYD_Red_Line.csv")
BYD_Unknown <- fread("data//BYD_unknown.csv")

x <- fread("data//indygo_nulls_0919_to_0331_2.csv")

x[,lubridate::ymd(generated_at)]

x[1,.(generated_at)][,strspl]

x[,.N,as.IDate(generated_at)]


FLOWBIRD_VALIDATIONS_THAT_I_PULLED_MY_DAMN_SELF <- fread("data//tableau_validations_20190901_to_20210405.csv")


fb_ridership <- x[
  !is.na(`Media Id`)
  ][
    Result == 1
    ][
        , Time := lubridate::mdy_hms(`Date (Local TIme)`)
        ][Time >= as.IDate("2020/01/01") & Time < as.IDate("2021/01/01")
          ][order(Time)
            ][`Fare Name` != "Pilot - Public"]

north_lat_boundary <- 39.877512
south_lat_boundary <- 39.709468
garage_boundary <- -86.173321

fb_rl_vehicle_validations <- fb_ridership[`Device External Id` %like% "ABB"
                                          ][Latitude < north_lat_boundary &
                                              Latitude > south_lat_boundary &
                                              Longitude > garage_boundary
                                            ]
fb_ridership[`Device External Id` %like% "ABB"
             ][is.na(Latitude) | is.na(Longitude)]

fb_rl_platform_validations <- fb_ridership[`Device External Id` %like% "APV"]


fb_unknown_routes <- fb_ridership[!fb_rl_vehicle_validations
                                  , on = "Validation Id"
                                  ][!fb_rl_platform_validations
                                    , on = "Validation Id"
                                    ][!`Device External Id` %like% "ATB"
                                      ][is.na(`Line Id`)
                                        ]
fwrite(fb_unknown_routes,"data//fb_unknown_route.csv")





