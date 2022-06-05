# Poker and roulette winnings from Monday to Friday:
poker_vector <- c(140, -50, 20, -120, 240)
roulette_vector <- c(-24, -50, 100, -350, 10)
days_vector <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
names(poker_vector) <- days_vector
names(roulette_vector) <- days_vector

poker_vector > 0

selection_vector <- poker_vector[poker_vector>0]
selection_vector

# Which days did you make money on roulette?
selection_vector <- roulette_vector > 0
  
# Select from roulette_vector these days
roulette_winning_days <- roulette_vector[selection_vector]

# Construct a matrix with 3 rows that contain the numbers 1 up to 9

matrix(1:9,nrow=3)

# Box office Star Wars (in millions!)
new_hope <- c(460.998, 314.4)
empire_strikes <- c(290.475, 247.900)
return_jedi <- c(309.306, 165.8)

# Construct matrix
star_wars_matrix <- matrix(c(new_hope, empire_strikes, return_jedi), nrow = 3, byrow = TRUE)

# Vectors region and titles, used for naming
region <- c("US", "non-US")
titles <- c("A New Hope", "The Empire Strikes Back", "Return of the Jedi")

# Name the columns with region
colnames(star_wars_matrix) <- region

# Name the rows with titles

rownames(star_wars_matrix) <- titles

# Print out star_wars_matrix
star_wars_matrix

# Calculate worldwide box office figures
worldwide_vector <- rowSums(star_wars_matrix) 

# Bind the new variable worldwide_vector as a column to star_wars_matrix
all_wars_matrix <- cbind(star_wars_matrix,worldwide_vector)


## If you want to check out the contents of the workspace, you can type ls() in the console.


## Factors ##

# Animals
animals_vector <- c("Elephant", "Giraffe", "Donkey", "Horse")
factor_animals_vector <- factor(animals_vector)
factor_animals_vector

# Temperature
temperature_vector <- c("High", "Low", "High","Low", "Medium")
factor_temperature_vector <- factor(temperature_vector, order = TRUE, levels = c("Low", "Medium", "High"))
factor_temperature_vector


### Renaming Factor Variables ## 

# Code to build factor_survey_vector
survey_vector <- c("M", "F", "F", "M", "M")
factor_survey_vector <- factor(survey_vector)

# Specify the levels of factor_survey_vector
levels(factor_survey_vector) <- c("Female","Male")

factor_survey_vector


# Build factor_survey_vector with clean levels
survey_vector <- c("M", "F", "F", "M", "M")
factor_survey_vector <- factor(survey_vector)
levels(factor_survey_vector) <- c("Female", "Male")

# Male
male <- factor_survey_vector[1]

# Female
female <- factor_survey_vector[2]

# Battle of the sexes: Male 'larger' than female?
male > female




# Create speed_vector
speed_vector <- c("medium","slow","slow","medium","fast")
# Create speed_vector
speed_vector <- c("medium", "slow", "slow", "medium", "fast")

# Convert speed_vector to ordered factor vector
factor_speed_vector <- factor(speed_vector,ordered=TRUE,levels=c("slow","medium","fast"))

# Print factor_speed_vector
factor_speed_vector
summary(factor_speed_vector)


################################################################################################

# Create factor_speed_vector
speed_vector <- c("medium", "slow", "slow", "medium", "fast")
factor_speed_vector <- factor(speed_vector, ordered = TRUE, levels = c("slow", "medium", "fast"))

# Factor value for second data analyst
da2 <- factor_speed_vector[2]
  
# Factor value for fifth data analyst
da5 <- factor_speed_vector[5]
  
# Is data analyst 2 faster than data analyst 5?
da2>da5

############################################################################################
# Working with Data Frames 

# Investigate the structure of mtcars
str(mtcars)

# Definition of vectors
name <- c("Mercury", "Venus", "Earth","Mars", "Jupiter", "Saturn","Uranus", "Neptune")
type <- c("Terrestrial planet","Terrestrial planet","Terrestrial planet","Terrestrial planet", "Gas giant", "Gas giant", "Gas giant", "Gas giant")
diameter <- c(0.382, 0.949, 1, 0.532, 11.209, 9.449, 4.007, 3.883)
rotation <- c(58.64, -243.02, 1, 1.03,0.41, 0.43, -0.72, 0.67)
rings <- c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE)

# Create a data frame from the vectors
planets_df <- data.frame(name, type,diameter,rotation,rings)
str(planets_df)

# Print out diameter of Mercury (row 1, column 3)
planets_df[1,3]

# Print out data for Mars (entire fourth row)
planets_df[4,]

# Select first 5 values of diameter column
planets_df[1:5,'diameter']
rings_vector <- planets_df$rings

# Print all the planets with Rings
planets_df[rings_vector,]

# Select planets with diameter < 1
subset(planets_df,diameter < 1)

##############################
# order() is a function that gives you the ranked position of each element when it is applied on a variable

a <- c(2,4,1,4,5,12,10)
order(a)
a[order(a)]

# Use order() to create positions
positions <- order(planets_df$diameter)  

# Use positions to sort planets_df
planets_df[positions,]


######################
## Working with Lists

# Vector with numerics from 1 up to 10
my_vector <- 1:10 

# Matrix with numerics from 1 up to 9
my_matrix <- matrix(1:9, ncol = 3)

# First 10 elements of the built-in data frame mtcars
my_df <- mtcars[1:10,]

# Construct list with these different elements:
my_list <- list(my_vector,my_matrix,my_df)
names(my_list) <- c("vec","mat","df")

### Another way to achieve the same result 

my_list <- list(vec = my_vector,mat = my_matrix,df = my_df)

my_list$df[3,3]

## You can also refer to the names of the components, with [[ ]] or with the $ sign. 
#Both will select the data frame representing the reviews:

my_list[['df']][1,2]

########################### Intermediate R ###################################

TRUE < FALSE

## This is because TRUE == 1 and False == 0. 

# The linkedin and last variable are already defined for you
linkedin <- c(16, 9, 13, 5, 2, 17, 14)
last <- tail(linkedin, 1)

# Is last under 5 or above 10?
last < 5 | last > 10 

# Is last between 15 (exclusive) and 20 (inclusive)?
last > 15 & last <= 20 

x <- 5
y <- 7
!(!(x < 4) & !!!(y > 12))


medium <- "LinkedIn"
num_view <- 14

if (medium == "LinkedIn"){
  print("yes")
}



## Checking the While Loop in R 

speed <- 64

while (speed > 30 ){
  print(paste(" Slow Down ! ", " Your Speed is ",speed))
  speed <- speed -7 
}

### Paste command is used to concatenate strings together. 

# Initialize i as 1 
i <- 1

# Code the while loop
while (i <= 10) {
  tpl <- i*3
  if (tpl%%8 == 0) {
    print(tpl)
    break
  }
  print(tpl)
  i <- i + 1
}


# The linkedin vector has already been defined for you
linkedin <- c(16, 9, 13, 5, 2, 17, 14)

# Loop version 1
for (c in linkedin){
  print(c)
}

# Loop version 2
for(i in 1:length(linkedin)){
  print(linkedin[i])
}

############# Using Double Brackets ###################

# The nyc list is already specified
nyc <- list(pop = 8405837, boroughs = c("Manhattan", "Bronx", "Brooklyn", "Queens", "Staten Island"),capital = FALSE)

# Loop version 1
for (i in nyc){
  print(i)
}

# Loop version 2
for (i in 1:length(nyc)){
  print(nyc[[i]])
}

nyc <- matrix(1:9,nrow = 3,byrow = TRUE)

for (i in 1:nrow(nyc)){
  for (j in 1:ncol(nyc)){
    print(paste("On row",i,"and column ",j,"the value is",nyc[[i,j]] ))
  }
}

################## Functions in R ##########################
# The linkedin and facebook vectors have already been created for you
linkedin <- c(16, 9, 13, 5, NA, 17, 14)
facebook <- c(17, NA, 5, 16, 8, 13, 14)

mean(linkedin)
mean(facebook)

mean(linkedin, na.rm = TRUE)
mean(facebook, na.rm = TRUE)

# Calculate the mean absolute deviation
mean(abs(linkedin - facebook), na.rm = TRUE)

# Create a function pow_two()
pow_two <- function(a){
  return(a^2)
  
}

# Use the function
pow_two(12)

# Create a function sum_abs()
sum_abs<- function(a,b){
  sum(abs(a)+abs(b))
}

# Use the function
sum_abs(-2,3)


# The linkedin and facebook vectors have already been created for you
linkedin <- c(16, 9, 13, 5, 2, 17, 14)
facebook <- c(17, 7, 5, 16, 8, 13, 14)

# The interpret() can be used inside interpret_all()
interpret <- function(num_views) {
  if (num_views > 15) {
    print("You're popular!")
    return(num_views)
  } else {
    print("Try to be more visible!")
    return(0)
  }
}


for (v in linkedin){
  print(v)
}

# Define the interpret_all() function
# views: vector with data to interpret
# return_sum: return total number of views on popular days?

interpret_all <- function(views, return_sum = TRUE){
  count <- 0
  for (v in views){
    print(v)
    print(interpret(v))
    count <- count + interpret(v)
  } 
  if (return_sum){
    return(count)
  } else {
    return (NULL)
  }
}


interpret_all(facebook, TRUE)
interpret_all(linkedin)


######## lapply() ##################################
####################################################

chars <- c('abs','brit','caravan')
lapply(chars, nchar)
class(lapply(chars, nchar))

## BY default, it created a list. We can use the below code to turn the list into a vector. 
# lapply always returns a list. To convert it to a vector, we use the below function. 

unlist(lapply(chars, nchar))
class(unlist(lapply(chars, nchar)))

oil_prices <- list(2.36, 2.49,2.32,3.57)
unlist(lapply(oil_prices, function(x){x**3}))  ## All in one code 

## Using Function with Parameters
multiply <- function(x, val){
  return(x**val)
}
unlist(lapply(oil_prices, multiply, val = 3))


#################################### exercise ######################################
# Code from previous exercise:
pioneers <- c("GAUSS:1777", "BAYES:1702", "PASCAL:1623", "PEARSON:1857")
split <- strsplit(pioneers, split = ":")
split_low <- lapply(split, tolower)

# Write function select_first()
select_first <- function(x) {
  x[1]
}

# Apply select_first() over split_low: names
lapply(split_low, select_first)

# Write function select_second()
select_second <- function(x){
  x[2]
}

# Apply select_second() over split_low: years
years <- unlist(lapply(split_low, select_second))

##################### Anonymous function ###########################
############### Definition of split_low ###############
pioneers <- c("GAUSS:1777", "BAYES:1702", "PASCAL:1623", "PEARSON:1857")
split <- strsplit(pioneers, split = ":")
split_low <- lapply(split, tolower)

# Generic select function
select_el <- function(x, index) {
  x[index]
}

# Use lapply() twice on split_low: names and years

names <- lapply(split_low,select_el,index = 1)
years <- lapply(split_low,select_el,index = 2)

##################### sApply  ###########################
############### Simplify Apply ###############

extremes <- function(x) {
  c(min = min(x), max = max(x))
}



