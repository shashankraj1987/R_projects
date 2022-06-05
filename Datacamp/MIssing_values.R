library(naniar)  ## Dealing with Missing Values. 
library(visdat)



df_clean <- df[,!sapply(df, function(x) mean(is.na(x)))>0.40]  ## Delete columns more than 40% of Missing Values. 


data("airquality")

# Create x, a vector, with values NA, NaN, Inf, ".", and "missing"
x <- c(NA,NaN, Inf, ".", "missing")

# Use any_na() and are_na() on to explore the missings
any_na(x)
are_na(x)

## NaN and Inf are NOT missing Values. They are Calculation errors.

# Use n_miss() to count the total number of missing values in dat_hw
n_miss(airquality)

# Use n_miss() on dat_hw$weight to count the total number of missing values
n_miss(airquality$Ozone)

# Use n_complete() on dat_hw to count the total number of complete values
n_complete(airquality)

# Use n_complete() on dat_hw$weight to count the total number of complete values
n_complete(airquality$Ozone)

# Use prop_miss() and prop_complete() on dat_hw to count the total number of missing values in each of the variables
prop_miss(airquality)
prop_complete(airquality)


### Basic Summary of Missing Values 

n_miss()
n_complete()

# DataFrame Summary of Missing Values
## This works with dplyr package 

miss_var_summary(airquality)  ## Show the overall missing data by Columns 
miss_case_summary(airquality) ## Shows the row-wise missing data for all columns combined. 

miss_var_table(airquality) ## Shows count of missing values per column with percentage 

airquality %>% group_by(Month) %>% miss_var_summary() ## month-wise summary of missing data. 

## We can group the data and show summary of missing values by a group. 

#Other summaries of missingness
#Some summaries of missingness are particularly useful for different types of data. For example, miss_var_span() and miss_var_run().
#miss_var_span() calculates the number of missing values in a specified variable for a repeating span. This is really useful in time series data, to look for weekly (7 day) patterns of missingness.
#miss_var_run() calculates the number of "runs" or "streaks" of missingness. This is useful to find unusual patterns of missingness, for example, you might find a repeating pattern of 5 complete and 5 missings.
#Both miss_var_span() and miss_var_run() work with the group_by operator from dplyr.


### # Calculate the summaries for each run of missingness for the variable, hourly_counts
miss_var_run(pedestrian, var = hourly_counts)

# Calculate the summaries for each span of missingness, 
# for a span of 4000, for the variable hourly_counts
miss_var_span(pedestrian, var = hourly_counts, span_every = 4000)

# For each `month` variable, calculate the run of missingness for hourly_counts
pedestrian %>% group_by(month) %>% miss_var_run(var=hourly_counts)

# For each `month` variable, calculate the span of missingness 
# of a span of 2000, for the variable hourly_counts
pedestrian %>% group_by(month) %>% miss_var_span(var = hourly_counts, span_every = 2000)

vis_miss(airquality) ## Birds's eye view of Missing Data by Columns 
vis_miss(airquality, cluster = TRUE)  ## Cluster the Data by MIssing Values. 

## Looking for missing values by Rows and Cases
gg_miss_var(airquality)
gg_miss_case(airquality)

gg_miss_var(airquality, facet = Month)  ## Facet the Data by Month. 

gg_miss_upset(airquality) # Shows the number of combinations of Missing Values that co-occur
## This chart shows 35 missing values in Solar_R, 5 in Ozone and  2 in both. 

gg_miss_span(airquality,Month, span = 10) ## Visualization of miss_span function 

# Using the pedestrian dataset, explore the impact of month by faceting by month
# and explore how missingness changes for a span of 1000
gg_miss_span(pedestrian, var = hourly_counts  , span_every = 1000, facet = month)


########## Searching and Replacing Missing Values ##########

score <- c(3,-99,4,-99,7,10,12,16,9)
grade <- c('N/A','E','missing','na', 'n/a',NA,'.',NA,'N/a')
place <- c(-99,97,95,92,-98,'missing',88,'.',86)

chaos <- data.frame(score,grade,place)

## Seacrhing for Actual missing values, not recognized by R

chaos %>% miss_scan_count(search = list("N/A","N/a"))  # Find actual values missing 
miss_scan_count(data=chaos, search=list("N/A","N/a"))
chaos %>% replace_with_na(replace = list(grade=c("N/A","N/a")))  ## Replace the missing values by Column name with N/A.


# Explore the strange missing values "N/A"
miss_scan_count(data = chaos, search = list("N/A"))

# Explore the strange missing values "missing"
miss_scan_count(data = chaos, search = list("missing"))

# Explore the strange missing values "na"
miss_scan_count(data = chaos, search = list("na"))

# Explore the strange missing values " " (a single space)
miss_scan_count(data = chaos, search = list(" "))

# Explore all of the strange missing values, "N/A", "missing", "na", " "
miss_scan_count(data = chaos, search = list("N/A", "missing", "na", " "))


## Replace Missing Values 
# Print the top of the pacman data using `head()`
head(pacman)
# Replace the strange missing values "N/A", "na", and  
# "missing" with `NA` for the variables, year, and score
pacman_clean <- replace_with_na(data = pacman, replace = list(year = c("N/A","na", "missing"),score = c("N/A","na", "missing")))

# Test if `pacman_clean` still has these values in it?
miss_scan_count(data = pacman_clean, search = list("N/A","na", "missing"))

# Use `replace_with_na_at()` to replace with NA
replace_with_na_at(pacman,.vars = c("year", "month","day"), ~.x %in% c("N/A", "missing", "na"," "))

# Use `replace_with_na_if()` to replace with NA the character values using `is.character`
replace_with_na_if(pacman,.predicate = is.character,~.x %in% c("N/A", "missing", "na"," "))

# Use `replace_with_na_all()` to replace with NA
replace_with_na_all(pacman, condition = ~.x %in% c("N/A", "missing", "na"," "))


### Taking Care of Data which should have been recorded but not present in the dataset at all. 
## The values are not recorded and would be necessarily considered missing. 

name <- c("robin","robin","robin","sam","sam","blair","blair","blair")
time <- c("morning","afternoon","evening","morning","afternoon","morning","afternoon","evening")
value <- c(358,534,100,139,177,963,962,929)

tetris <- data.frame(name, time, value)

# In this, the reading for evening for Sam is missing. Though it is not recorded as missing data, it is actually missing. 

tetris %>% tidyr::complete(name, time) 

# this makes the implicit missing as explicit.

tidyr::fill(value) ## Fills the last observation until the next NA, as long as next data value is observed. Last Observation carried Forward

############## Missing Data Dependence ##############

## What to do with the missing data ?



