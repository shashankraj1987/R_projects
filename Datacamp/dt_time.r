library(ggplot2)
library(lubridate)
library(dplyr)

# The date R 3.0.0 was released
x <- "2013-04-03"

# Examine structure of x
str(x)

# Use as.Date() to interpret x as a date
x_date <- as.Date(x)

# Examine structure of x_date
str(x_date)

# Store April 10 2014 as a Date
april_10_2014 <- as.Date("2014-04-10")

# Load the anytime package
library(anytime)

# Various ways of writing Sep 10 2009
sep_10_2009 <- c("September 10 2009", "2009-09-10", "10 Sep 2009", "09-10-2009")

# Use anytime() to parse sep_10_2009
anytime(sep_10_2009)

########################################################################
### Date objects are stored as number of days since 1970-01-01
########################################################################

########################################################################
##     Chart operation with Dates 
########################################################################

releases <- read.csv("D:\\Users\\shash\\Documents\\GitHub\\Tutorials_R_PY\\R\\Datacamp\\rversions.csv")

releases$date <- as.Date(releases$date,"%m/%d/%y" )

colnames(releases) <- c("major","minor","patch","date","datetime","time","type")

head(releases)

# Set the x axis to the date column
ggplot(releases, aes(x = date, y = type)) +
  geom_line(aes(group = 1, color = factor(major)))

# Limit the axis to between 2010-01-01 and 2014-01-01
ggplot(releases, aes(x = date, y = type)) +
  geom_line(aes(group = 1, color = factor(major))) +
  xlim(as.Date("2010-01-01"), as.Date("2014-01-01"))

# Specify breaks every ten years and labels with "%Y"
ggplot(releases, aes(x = date, y = type)) +
  geom_line(aes(group = 1, color = factor(major))) +
  scale_x_date(date_breaks = "10 years", date_labels = "%Y")

########################################################################
##     Arithmetic operation with Dates 
########################################################################


# Find the largest date
last_release_date <- max(releases$date)

# Filter row for last release
last_release <- filter(releases, date == last_release_date)

# Print last_release
last_release

# How long since last release?
Sys.Date() - last_release_date



# Use as.POSIXct to enter the datetime 
as.POSIXct("2010-10-01 12:12:00")

# Use as.POSIXct again but set the timezone to `"America/Los_Angeles"`
as.POSIXct("2010-10-01 12:12:00", tz = "America/Los_Angeles")

# Use read_csv to import rversions.csv
releases <- read_csv("rversions.csv")

# Examine structure of datetime column
str(releases$datetime)



# Import "cran-logs_2015-04-17.csv" with read_csv()
logs <- read_csv("cran-logs_2015-04-17.csv")

# Print logs
logs

# Store the release time as a POSIXct object
release_time <- as.POSIXct("2015-04-16 07:13:33", tz = "UTC")

# When is the first download of 3.2.0?
logs %>% 
  filter(datetime > release_time,
         r_version == "3.2.0")

# Examine histograms of downloads by version
ggplot(logs, aes(x = datetime)) +
  geom_histogram() +
  geom_vline(aes(xintercept = as.numeric(release_time)))+
  facet_wrap(~ r_version, ncol = 1)


############################################################
## Using lubridate 
############################################################

?parse_date_time

library(lubridate)

# Parse x 
x <- "2010 September 20th" # 2010-09-20
ymd(x)

# Parse y 
y <- "02.01.2010"  # 2010-01-02
dmy(y)

# Parse z 
z <- "Sep, 12th 2010 14:00"  # 2010-09-12T14:00
mdy_hm(z)


# Formatting Date Characters 

# d - Numeric day of the Month 
# m - Month of the year 
# y - Year with Century - 2021
# Y - year without century - 21
# H - Hours (24 hour)
# M - Minutes
# S - Seconds
# a - Abbreviated weekday 
# A - Full Weekday 
# b - Abbreviated month name 
# B - Full Month Name
# I - Hours (12 hour)
# p - Am/PM
# z - TimeZone, offset from UTC 

library(lubridate)

# Parse x 
x <- "2010 September 20th" # 2010-09-20
ymd(x)

# Parse y 
y <- "02.01.2010"  # 2010-01-02
dmy(y)

# Parse z 
z <- "Sep, 12th 2010 14:00"  # 2010-09-12T14:00
mdy_hm(z)

x <- "Monday June 1st 2010 at 4pm"
parse_date_time(x, orders = "amdyIp")

# Specify order to include both "mdy" and "dmy"
two_orders <- c("October 7, 2001", "October 13, 2002", "April 13, 2003", 
                "17 April 2005", "23 April 2017")
parse_date_time(two_orders, orders = c("mdy","dmy"))

# Specify order to include "dOmY", "OmY" and "Y"
short_dates <- c("11 December 1282", "May 1372", "1253")
parse_date_time(short_dates, orders = c("dOmY", "OmY", "Y"))


akl_hourly_raw <- read.csv("https://assets.datacamp.com/production/course_5348/datasets/akl_weather_hourly_2016.csv")

str(akl_hourly_raw)
head(akl_hourly_raw)


akl_daily <- akl_hourly_raw %>% mutate(date = as.Date(date_utc))



akl_hourly_raw <- read.csv("https://assets.datacamp.com/production/course_5348/datasets/akl_weather_hourly_2016.csv")


head(akl_hourly_raw)

####################################################################################
#### Combine Date, month and Year into make_date function to make this into a date.
####################################################################################

# Use make_date() to combine year, month and mday 
akl_hourly  <- akl_hourly_raw  %>% mutate(date = make_date(year = year, month = month, day = mday))

# Parse datetime_string 
akl_hourly <- akl_hourly  %>% 
  mutate(datetime_string = paste(date, time, sep = "T"), datetime = as.Date(datetime_string))

# Print date, time and datetime columns of akl_hourly
akl_hourly %>% select(date, time, datetime)

# Plot to check work
ggplot(akl_hourly, aes(x = date, y = temperature)) + geom_line()

####################################################################################
###### Extracting Parts of a Datetime
####################################################################################

# year() <- Extracts the year with Century 
# month() <- Extracts the month (1-12)
# day() <- Extracts the Day of the month (1-31)
# hour() <- Hour (0-23)
# min() <- Minutes (0-59)
# second() <- Second (0-59)
# wday() <- Weekday (1-7)
# yday() <- Day of the Year (1-366)
# tz() <- Timezone
# leap_year() <- Whether it is a Leap year 
# am() <- Morning?
# pm() <- evening
# dst() <- Daylight Savings True or False?
# quarter() <- Quarter of the year (1-4)
# Semester() <- Half of the year (1-2)


head(akl_daily)
akl_time <- akl_daily[,"date"]

head(akl_time)

month(akl_time) %>% table()
year(akl_time) %>% table()

am(akl_time)

### Get the month Name 
head(month(akl_time, label = TRUE, abbr = FALSE))

### Get the Day Name 
wday(akl_time,label = TRUE, abbr = FALSE)


############################################################
######### Extracting, Filtering and Summarizing Data 
############################################################

## Extracting the observations for Weekdays only. 

# Create new columns hour, month and rainy
akl_hourly <- akl_hourly %>%
  mutate(
    hour = hour(datetime),
    month = month(datetime, label = TRUE),
    rainy = weather == "Precipitation"
  )

# Filter for hours between 8am and 10pm (inclusive)
akl_day <- akl_hourly %>% 
  filter(hour >= 8 & hour <= 22)

# Summarise for each date if there is any rain
rainy_days <- akl_day %>% 
  group_by(month, date) %>%
  summarise(
    any_rain = any(rainy)
  )

# Summarise for each month, the number of days with rain
rainy_days %>% 
  summarise(
    days_rainy = group_by(any_rain)
  )

############################################################
### Rounding Off and Extracting Dates
############################################################

#round_date() rounds a date to the nearest value
#floor_date() rounds down
#ceiling_date() rounds up.

r_3_4_1 <- ymd_hms("2016-05-03 07:13:28 UTC")

# Round down to day
floor_date(r_3_4_1, unit = "day")

# Round to nearest 5 minutes
round_date(r_3_4_1, unit = "5 minutes")

# Round up to week 
ceiling_date(r_3_4_1, unit = "week")

# Subtract r_3_4_1 rounded down to day
r_3_4_1 - floor_date(r_3_4_1, unit = "day")


# When is rounding useful? In a lot of the same situations extracting date components is useful. 
# The advantage of rounding over extracting is that it maintains the context of the unit. 
# For example, extracting the hour gives you the hour the datetime occurred, 
# but you lose the day that hour occurred on (unless you extract that too), 
# on the other hand, rounding to the nearest hour maintains the day, month and year.



############################################################
######### Parsing Complex Dates 
############################################################

# Specify an order string to parse x
x <- "Monday June 1st 2010 at 4pm"
parse_date_time(x, orders = "amdyIp")

# Specify order to include both "mdy" and "dmy"
two_orders <- c("October 7, 2001", "October 13, 2002", "April 13, 2003", 
                "17 April 2005", "23 April 2017")
parse_date_time(two_orders, orders = c("mdy","dmy"))

# Specify order to include "dOmY", "OmY" and "Y"
short_dates <- c("11 December 1282", "May 1372", "1253")
parse_date_time(short_dates, orders = c("dOmY", "OmY", "Y"))



############################################################
###### Adding and Substracting Timestamp from Date
############################################################

# ddays() = 86400 seconds << Duration of 1 day >>   Duration of 
# days() = 1 day  << 1 Day in current time zone.>>  Period of 


# Add a period of one week to mon_2pm
mon_2pm <- dmy_hm("27 Aug 2018 14:00")
mon_2pm + days(7)

# Add a duration of 81 hours to tue_9am
tue_9am <- dmy_hm("28 Aug 2018 9:00")
tue_9am + dhours(81)

# Subtract a period of five years from today()
today() - years(5)

# Subtract a duration of five years from today()
today() - dyears(5)

############################################################
###### Arithmetic with Time Stamps
############################################################

# Time of North American Eclipse 2017
eclipse_2017 <- ymd_hms("2017-08-21 18:26:40")

# Duration of 29 days, 12 hours, 44 mins and 3 secs
synodic <- ddays(29)+dhours(12)+dminutes(44)+dseconds(3)

# 223 synodic months
saros <- 223*synodic

# Add saros to eclipse_2017
eclipse_2017 + saros

############################################################
###### Generating sequences of datetimes
############################################################

# generate a sequence of periods from 1 day up to 10 days
1:10 * days(1)

#Then by adding this sequence to a specific datetime, 
#you can construct a sequence of datetimes from 1 day up to 10 days into the future

today() + 1:10 * days(1)


# Add a period of 8 hours to today
today_8am <- today() + dhours(8)

# Sequence of *two* weeks from 1 to 26
every_two_weeks <- 1:26 * weeks(2)

# Create datetime for every two weeks for a year
today_8am + every_two_weeks


## Sequence of Months 

jan_31 = ymd("2021-01-31")

# A sequence of 1 to 12 periods of 1 month
month_seq <- 1:12 * months(1)

# Add 1 to 12 months to jan_31
jan_31 + month_seq

## For months where 31 is not present, NA is printed. 


# Replace + with %m+%
jan_31 %m+% month_seq

### %m+% adds the last day of the month regardless of the 
## number of days in each month.


# Replace + with %m-%
jan_31 %m-% month_seq


## unlike + and -, you might not get x back from x %m+% months(1) %m-% months(1). 
#If you'd prefer that the date was rolled forward check out add_with_rollback() 
#which has roll_to_first argument.


############################################################
###### Calculating Time Interval 
############################################################

# Time Span 
# Interval -- When we have a Start and End Date
# Periods -- When we are interested in Human units 
# Duration -- When we are interested in Seconds elapsed. 


# You can create an interval by using the operator %--% with two datetimes. 
# For example ymd("2001-01-01") %--% ymd("2001-12-31") creates an interval for the year of 2001.
# once you have an interval you can find out certain properties like its start, end and length with 
# int_start(), int_end() and int_length() respectively.



#############################################

# Print monarchs
monarchs

# Create an interval for reign
monarchs <- monarchs %>%
  mutate(reign = from %--% to) 

# Find the length of reign, and arrange
monarchs %>%
  mutate(length = int_length(reign)) %>% 
  arrange(desc(length)) %>%
  select(name, length, dominion)

#############################################


#The operator %within% tests if the datetime (or interval) on the left hand side is within the interval of the right hand side. 
#For example, if y2001 is the interval covering the year 2001.

y2001 <- ymd("2001-01-01") %--% ymd("2001-12-31")

ymd("2001-03-30") %within% y2001
ymd("2002-03-30") %within% y2001

#int_overlaps() performs a similar test, but will return true if two intervals overlap at all.

##############################################
# Print halleys
halleys

# New column for interval from start to end date
halleys <- halleys %>% 
  mutate(visible = start_date %--% end_date)

# The visitation of 1066
halleys_1066 <- halleys[14, ] 

# Monarchs in power on perihelion date
monarchs %>% 
  filter(halleys_1066$perihelion_date %within% reign) %>%
  select(name, from, to, dominion)

# Monarchs whose reign overlaps visible time
monarchs %>% 
  filter(int_overlaps(halleys_1066$visible, reign)) %>%
  select(name, from, to, dominion)


# New columns for duration and period
monarchs <- monarchs %>% mutate(duration = as.duration(reign), period = as.period(reign)) 

# Examine results    
monarchs %>% select(name,duration,period)


# as.duration converts an interval into duration 
# as.period converts an interval into period

##############################################


############################################################
###### Reading Date Time Quickly and exporting Date Times.  
############################################################

library(microbenchmark)
library(fasttime)


# The fasttime package provides a single function fastPOSIXct(), 
# designed to read in datetimes formatted according to ISO 8601. 
# Because it only reads in one format, and doesn't have to guess a format, it is really fast!

?fastPOSIXct()
# It only interprets y-m-d hh-mm-ss dataset.


########################## 
#### Example from Datacamp
##########################

library(microbenchmark)
library(fasttime)

# Examine structure of dates
str(dates)

# Use fastPOSIXct() to parse dates
fastPOSIXct(dates) %>% str()

# Compare speed of fastPOSIXct() to ymd_hms()
microbenchmark(
  ymd_hms = ymd_hms(dates),
  fasttime = fastPOSIXct(dates),
  times = 20)

###### Output #######

## Unit: microseconds
## expr       min       lq       mean   median         uq       max neval
## ymd_hms 19669.262 23208.90 25943.1716 23892.07 27856.5155 41849.424    20
## fasttime   671.494   698.98   821.2323   732.24   772.1335  1431.608    20

##########################



### Fast time parsing with lubridate 

dates <- "2015-12-31T11:00:00Z"
fast_strptime(dates,format = "%Y-%m-%dT%I:%M:%SZ")

#### Speed Comparison 

## fast_strptime < fastPOSIXct < ymd_hms


# Head of dates
head(dates)

# Parse dates with fast_strptime
fast_strptime(dates, 
              format = "%Y-%m-%dT%H:%M:%SZ") %>% str()

# Comparse speed to ymd_hms() and fasttime
microbenchmark(
  ymd_hms = ymd_hms(dates),
  fasttime = fastPOSIXct(dates),
  fast_strptime = fast_strptime(dates,format = "%Y-%m-%dT%H:%M:%SZ"),
  times = 20)
##########################



############################################################
###### Creating Custom Date Stamp  
############################################################


# Create a stamp based on "Saturday, Jan 1, 2000"
date_stamp <- stamp("Saturday, Jan 1, 2000")

# Print date_stamp
date_stamp

# Call date_stamp on today()
date_stamp(today())

# Create and call a stamp based on "12/31/1999"
stamp("12/31/1999")(today())

finished <- stamp("I finished 'Dates and Times in R' on Thursday, September 4, 2017!")

# Use string finished for stamp()
finished(today())
