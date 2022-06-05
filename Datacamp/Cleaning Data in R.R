library(dplyr)
library(readxl)
library(stringr)  ## For String Functions 

library(assertive)

bike_share_rides = readRDS("bike_share_rides_ch1_1.rds")
sales <- read.csv("D:/Users/shash/Documents/Learning/R/DataCamp/sales.csv")
glimpse(sales)
sales



## Check if a Value is numeric.
is.numeric(sales$revenue)

assert_is_numeric(sales$revenue) 
## Assert halts the execution of code when the return is false. 

class(sales$revenue)

# Remove the commas
revenue_trimmed <- str_remove(sales$revenue,",")

class(revenue_trimmed)
as.numeric(revenue_trimmed)


################################################################################################
####################################    Datacamp Codes Bike_ride_shares ########################
################################################################################################

# Glimpse at bike_share_rides
glimpse(bike_share_rides)

# Summary of user_birth_year
summary(bike_share_rides$user_birth_year)

# Convert user_birth_year to factor: user_birth_year_fct
bike_share_rides <- bike_share_rides %>% mutate(user_birth_year_fct = factor(user_birth_year))
head(bike_share_rides)

# Assert user_birth_year_fct is a factor
assert_is_factor(bike_share_rides$user_birth_year_fct)

# Summary of user_birth_year_fct
summary(bike_share_rides$user_birth_year_fct)

bike_share_rides <- bike_share_rides %>% 
  # Remove 'minutes' from duration: duration_trimmed
  mutate(duration_trimmed = str_remove(duration,'minutes'),
         # Convert duration_trimmed to numeric: duration_mins
         duration_mins = as.numeric(duration_trimmed))

# Glimpse at bike_share_rides
glimpse(bike_share_rides)

# Assert duration_mins is numeric
assert_is_numeric(bike_share_rides$duration_mins)

# Calculate mean duration
mean(bike_share_rides$duration_mins)


##############  Missing values ############

library(visdat) ## Visualize Missing values 


data("airquality")
airquality
is.na(airquality)

sum(is.na(airquality))


vis_miss(airquality)

