## Generate a Sequence in Decending Order 
seq(8,2,by = -2)

## Generate a Sequence of numbers in regular order 
seq(2,10,by = 3)

## Repeates a List of numbers given times. 
rep(c(10,14,29), times = 2)

## Repeates each number of the list given times.  
rep(c(10,14,29), each = 2)

num <- rep(c(10,14,29), each = 2, times = 2)

sort(num, decreasing = TRUE)

unique(sort(num, decreasing = TRUE))


## convert a list to a vector 
unlist(li)

# where li is a list 
# this returns a vector. 

## Add a value to a list
append()

# Reverse a value of a list 
rev()

###############################
# The errors vector has already been defined for you
errors <- c(1.9, -2.6, 4.0, -9.5, -3.4, 7.3)

# Sum of absolute rounded values of errors
sum(abs(round(errors)))

###############################
# Don't edit these two lines
vec1 <- c(1.5, 2.5, 8.4, 3.7, 6.3)
vec2 <- rev(vec1)

# Fix the error
abs(vec1)
abs(vec2)

#mean() takes a vector of numerical values, 
#so make sure that abs(vec1) and abs(vec2) are elements of a vector: c(abs(vec1), ___).

mean(c(abs(vec1),abs(vec2)))

###############################
# The linkedin and facebook lists have already been created for you
linkedin <- list(16, 9, 13, 5, 2, 17, 14)
facebook <- list(17, 7, 5, 16, 8, 13, 14)

# Convert linkedin and facebook to a vector: li_vec and fb_vec
li_vec <- unlist(linkedin)
fb_vec <- unlist(facebook)

# Append fb_vec to li_vec: social_vec
social_vec <- append(li_vec, fb_vec)

# Sort social_vec
sort(social_vec, decreasing = TRUE)

###############################
# Fix me
seq(rep(1, 7, by = 2), times = 7)

#Answer
rep(seq(1, 7, by = 2), times = 7)


