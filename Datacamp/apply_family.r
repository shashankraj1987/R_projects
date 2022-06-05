library(dplyr)
library(tidyverse)

monday <- c(3 ,7, 9,  6,-1)
tuesday <- c(6,9,12,13,5)
wednesday <- c(4,8,3,-1,-3)
thursday <- c(1,4,7,2,-2)
friday <- c(5,7,9,4,2)
saturday <- c(-3,5,8,9,4)
sunday <- c(3,6,9,4,1)

d_temps <- data.frame(monday,tuesday,wednesday, thursday, friday,saturday, sunday)
l_temps <- list(monday,tuesday,wednesday, thursday, friday,saturday, sunday)
l_temps


# Definition of below_zero()
below_zero <- function(x) {
  return(x[x < 0])
}

# Apply below_zero over temp using sapply(): freezing_s
freezing_s <- sapply(l_temps, below_zero)

# Apply below_zero over temp using lapply(): freezing_l
freezing_l <- lapply(l_temps, below_zero)

# Are freezing_s and freezing_l identical?
identical(freezing_l,freezing_s)


### vapply
## We can specify the output format and expected values

## Define a function called Basics ## 
## Definition of basics()

basics <- function(x) {
  c(min = min(x), mean = mean(x), max = max(x))
}

vapply(l_temps, basics, FUN.VALUE = numeric(3), USE.NAMES = TRUE)
summary(d_temps)

## Converting sapply functions to vapply 

## all in one function  #aio 
# Creates anonymous function, assign default value, use vapply 
vapply(l_temps, function(x, y) { mean(x) > y },y = 5,FUN.VALUE = logical(1))


vapply(l_temps, mean,numeric(1))


##############################################################
####### Revisit Apply Functions ####### 

# Apply function mostly works on lists 