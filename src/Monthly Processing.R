# step 1 - download all transactions from 
# https://tableau.flowbirdhub.com/#/views/Mike_Test/Sheet1?:iid=1
#
# step 2 - read in and clean those data
# A - filter for ridership
# B - Transit Day
# C - process to find GPS errors
#
# step 3
# get our five sets of vals
# Full
# RB veh only
# MB veh only
# Platform only
# Vehicle only


library(data.table)
# library(timeDate)
library(magrittr)




# read in and clean -------------------------------------------------------

# get date

this_month_start <- lubridate::floor_date(x = Sys.Date(),unit = "month")

# get filepath

flowbird_folder_path <- "data//raw//Full_Downloads//full_download_"

# get raw flowbird vals
 
flowbird_raw <- data.table::fread(paste0(flowbird_folder_path,
                                         format(this_month_start
                                                , "%Y%m"
                                                )
                                         ,".csv"
                                         )
                                  )

# filter for actual ridership validations
fb_ridership <- flowbird_raw[
  #na media ids are failed validations, remove them
  !is.na(`Media Id`)
  ][
    #these are paid fares
    Result == 1
  ][
    #get the time
    , Time := lubridate::mdy_hms(`Date (Local TIme)`)
  ][
    order(Time)
  ][
    #remove test buses
    !`Device External Id` %like% "ATB"
  ]

# do transit day

Transit_Day_Cutoff <- as.ITime("03:30:00")

fb_ridership[
  # do DateTest
  , DateTest := fifelse(data.table::as.ITime(Time) <= Transit_Day_Cutoff
                        , 1
                        , 0
                        )
  ][
    # do transit day
    , Transit_Day := fifelse(DateTest == 1
                             , data.table::as.IDate(Time)-1
                             , data.table::as.idate(Time)
                             )
  ]


# build sets --------------------------------------------------------------


# get Red Line Vehicles only
# there are four prefixes to vehicle numbers:
# APV - Platforms
# ABB - Red Line Buses
# ALB - Local Buses
# ATB - Test Buses
# uncomment next line to see this
# flowbird_raw[,.N,substr(`Device External Id`,1,3)]

fb_rl_vals <- fb_ridership[
  # get just red line vehicles
  `Device External Id` %like% "ABB"
]

fb_mb_vals <- fb_ridership[
  # get just mb vehicles
  `Device External Id` %like% "ALB"
]

fb_vehicle_vals <- fb_ridership[
  # remove platforms
  !`Device External Id` %like% "APV"
]

fb_platform_vals <- fb_ridership[
  # get just platforms
  `Device External Id` %like% "APV"
]

# check for valid, should both be true
nrow(fb_rl_vals)+nrow(fb_mb_vals) == nrow(fb_vehicle_vals)
nrow(fb_vehicle_vals) + nrow(fb_platform_vals) == nrow(fb_ridership)


# okay, so fb_rl_vals has validations that may be on the red line, and nulls
# need to match to vehicle location, then cut by lat long
# do not trust route labels from flowbird
# if the match is not good, count it towards invalid data
# 
# 
# fb_mb_vals has nulls, we know they were not on the red line
# but we still need to try to match to vmh, so we can determine route label
# if the match is not good, count it toward invalid data
# 
# 
# fb_vehicle_vals has the entire null set that we care about
# can skip matching nulls in fb_mb_vals and fb_rl_vals since all in here
#
# fb_platform_vals will all be red line validations, so we do not count those
# 
# essentially we know the following:
# fb_mb_vals with route labels are reportable
# fb_mb_vals with null rte labels are not reportable, but potentially fixable
# fb_rb_vals with route labels are not reportable, but potentially fixable
#   there are 90 labels in the extensions, and extensions in the 90
# fb_rb_vals with null rte labels are not reportable, but potentially fixable
# 
# we need to fix:
# 1. fb_mb_vals with null route labels
# 2. fb_rb_vals with null route labels
# 3. fb_rb_vals with route labels
# 1. and 2. are both contained in the fb_vehicle_vals with null rte labels
# 
# so, for brevity, fix:
# 1. fb_vehicle_vals with null rte labels
# 2. fb_rb_vals with route labels
# 
# then we need a % accuracy
# this will be:
# unmatched validations
# gapped validations, meaning the vmh time and transaction time differ by > 60s
# bad GPS, where GPSStatus != 2 or is.na()
# validation after VMH, where validation time is > last vmh for the transitday
# 
# essentially you're going to need to combine this script with the vmh match one
# that you previously developed