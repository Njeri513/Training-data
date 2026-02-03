#Writing Functions
library(dplyr)
#simulate a data set
study <- tibble(
  age       = c(32, 30, 32, 29, 24, 38, 25, 24, 48, 29, 22, 29, 24, 28, 24, 25, 
                25, 22, 25, 24, 25, 24, 23, 24, 31, 24, 29, 24, 22, 23, 26, 23, 
                24, 25, 24, 33, 27, 25, 26, 26, 26, 26, 26, 27, 24, 43, 25, 24, 
                27, 28, 29, 24, 26, 28, 25, 24, 26, 24, 26, 31, 24, 26, 31, 34, 
                26, 25, 27, NA),
  age_group = c(2, 2, 2, 1, 1, 2, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 
                1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 2, 
                2, 1, 1, 1, NA),
  gender    = c(2, 1, 1, 2, 1, 1, 1, 2, 2, 2, 1, 1, 2, 1, 1, 1, 1, 2, 2, 1, 1, 
                1, 1, 2, 1, 1, 2, 1, 1, 1, 2, 1, 1, 2, 2, 1, 2, 2, 1, 2, 2, 1, 
                1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 2, 2, 1, 1, 2, 1, 2, 1, 
                1, 1, 2, 1, NA),
  ht_in     = c(70, 63, 62, 67, 67, 58, 64, 69, 65, 68, 63, 68, 69, 66, 67, 65, 
                64, 75, 67, 63, 60, 67, 64, 73, 62, 69, 67, 62, 68, 66, 66, 62, 
                64, 68, NA, 68, 70, 68, 68, 66, 71, 61, 62, 64, 64, 63, 67, 66, 
                69, 76, NA, 63, 64, 65, 65, 71, 66, 65, 65, 71, 64, 71, 60, 62, 
                61, 69, 66, NA),
  wt_lbs    = c(216, 106, 145, 195, 143, 125, 138, 140, 158, 167, 145, 297, 146, 
                125, 111, 125, 130, 182, 170, 121, 98, 150, 132, 250, 137, 124, 
                186, 148, 134, 155, 122, 142, 110, 132, 188, 176, 188, 166, 136, 
                147, 178, 125, 102, 140, 139, 60, 147, 147, 141, 232, 186, 212, 
                110, 110, 115, 154, 140, 150, 130, NA, 171, 156, 92, 122, 102, 
                163, 141, NA),
  bmi       = c(30.99, 18.78, 26.52, 30.54, 22.39, 26.12, 23.69, 20.67, 26.29, 
                25.39, 25.68, 45.15, 21.56, 20.17, 17.38, 20.8, 22.31, 22.75, 
                26.62, 21.43, 19.14, 23.49, 22.66, 32.98, 25.05, 18.31, 29.13, 
                27.07, 20.37, 25.01, 19.69, 25.97, 18.88, 20.07, NA, 26.76, 
                26.97, 25.24, 20.68, 23.72, 24.82, 23.62, 18.65, 24.03, 23.86, 
                10.63, 23.02, 23.72, 20.82, 28.24, NA, 37.55, 18.88, 18.3, 
                19.13, 21.48, 22.59, 24.96, 21.63, NA, 29.35, 21.76, 17.97, 
                22.31, 19.27, 24.07, 22.76, NA),
  bmi_3cat  = c(3, 1, 2, 3, 1, 2, 1, 1, 2, 2, 2, 3, 1, 1, 1, 1, 1, 1, 2, 1, 1, 
                1, 1, 3, 2, 1, 2, 2, 1, 2, 1, 2, 1, 1, NA, 2, 2, 2, 1, 1, 1, 1, 
                1, 1, 1, 1, 1, 1, 1, 2, NA, 3, 1, 1, 1, 1, 1, 1, 1, NA, 2, 1, 
                1, 1, 1, 1, 1, NA)
) %>% 
  mutate(
    age_group = factor(age_group, labels = c("Younger than 30", "30 and Older")),
    gender    = factor(gender, labels = c("Female", "Male")),
    bmi_3cat  = factor(bmi_3cat, labels = c("Normal", "Overweight", "Obese"))
  ) %>% print()

#using the code below to calculate some measures
study %>% 
  summarise(
    n_miss = sum(is.na(age)),
    mean   = mean(age, na.rm = TRUE),
    median = median(age, na.rm = TRUE),
    min    = min(age, na.rm = TRUE),
    max    = max(age, na.rm = TRUE)
  )
#Doing the same for ht_in
study %>% 
  summarise(
    n_miss = sum(is.na(ht_in)),
    mean   = mean(ht_in, na.rm = TRUE),
    median = median(ht_in, na.rm = TRUE),
    min    = min(ht_in, na.rm = TRUE),
    max    = max(ht_in, na.rm = TRUE)
  )
#doing the same for wt_ibs theres an error in this code
study %>% 
  summarise(
    n_miss = sum(is.na(wt_lbs)),
    mean   = mean(wt_lbs, na.rm = TRUE),
    median = median(wt_lbs, na.rm = TRUE),
    min    = min(ht_in, na.rm = TRUE),
    max    = max(wt_lbs, na.rm = TRUE)
  )
#doing the same for bmi
study %>% 
  summarise(
    n_miss = sum(is.na(bmi)),
    mean   = mean(bmi, na.rm = TRUE),
    median = median(bmi, na.rm = TRUE),
    min    = min(bmi, na.rm = TRUE),
    max    = max(bmi, na.rm = TRUE)
  )
#we could have created a function to help us with this tasks
continuous_stats <- function(var) {
  study %>% 
    summarise(
      n_miss = sum(is.na({{ var }})),
      mean   = mean({{ var }}, na.rm = TRUE),
      median = median({{ var }}, na.rm = TRUE),
      min    = min({{ var }}, na.rm = TRUE),
      max    = max({{ var }}, na.rm = TRUE)
    )
}
#then use it for analysis of our variables
continuous_stats(age)
continuous_stats(ht_in)
continuous_stats(wt_lbs)
continuous_stats(bmi)

#the function writing process
#lets simulate some data
people_1 <- tribble(
  ~id_1, ~name_first_1, ~name_last_1, ~street_1,
  1,     "Easton",      NA,           "Alameda",
  2,     "Elias",       "Salazar",    "Crissy Field",
  3,     "Colton",      "Fox",        "San Bruno",
  4,     "Cameron",     "Warren",     "Nottingham",
  5,     "Carson",      "Mills",      "Jersey",
  6,     "Addison",     "Meyer",      "Tingley",
  7,     "Aubrey",      "Rice",       "Buena Vista",
  8,     "Ellie",       "Schmidt",    "Division",
  9,     "Robert",      "Garza",      "Red Rock",
  10,    "Stella",      "Daniels",    "Holland"
) %>% print()

# second dataframe
people_2 <- tribble(
  ~id_2, ~name_first_2, ~name_last_2, ~street_2,
  1,     "Easton",      "Stone",      "Alameda",
  2,     "Elas",        "Salazar",    "Field",
  3,     NA,            "Fox",        NA,
  4,     "Cameron",     "Waren",      "Notingham",
  5,     "Carsen",      "Mills",      "Jersey",
  6,     "Adison",      NA,           NA,
  7,     "Aubrey",      "Rice",       "Buena Vista",
  8,     NA,            "Schmidt",    "Division",
  9,     "Bob",         "Garza",      "Red Rock",
  10,    "Stella",      NA,           "Holland"
) %>%  print()

#comparing whether this two data frames have the same data
people <- people_1 %>% bind_cols(people_2) %>% print()

#attempting to create dummy variables
people %>% 
  mutate(
    name_first_match = name_first_1 == name_first_2,
    name_last_match  = name_last_1 == name_last_2,
    street_match     = street_1 == street_2
  ) %>% 
  # Order like columns next to each other for easier comparison
  select(id_1, starts_with("name_f"), starts_with("name_l"), starts_with("s"))

#if one side was NA R returned NA
result <- "Colton" == NA
result
#telling R that in the event that one side is Na RETURN false
result <- "Colton" == NA
result <- if_else(is.na(result), FALSE, result)
result

#making the above solution into a function
is_match <- function() {
  result <- "Colton" == NA
  result <- if_else(is.na(result), FALSE, result)
  result
}
is_match()
is_match(name = "Easton")#returns an error because the function only works for the name colton
#start by adding arguments to the function
is_match <- function(name) {
  result <- "Colton" == NA
  result <- if_else(is.na(result), FALSE, result)
  result
}
is_match(name = "Easton")
#adding additional arguments to the function
is_match <- function(name_1, name_2) {
  result <- "Colton" == NA
  result <- if_else(is.na(result), FALSE, result)
  result
}
is_match(name_1 = "Easton", name_2 = "Easton")

#generalizing the function
is_match <- function(first_name) {
  result <- first_name == NA
  result <- if_else(is.na(result), FALSE, result)
  result
}
is_match(first_name = "Easton")

#the argument must have a unique name 
is_match <- function(first_name_1, first_name_2) {
  result <- first_name_1 == first_name_2
  result <- if_else(is.na(result), FALSE, result)
  result
}
is_match(first_name_1 = "Easton", first_name_2 = "Colton")
is_match(first_name_1 = "Easton", first_name_2 = "Easton")
#tuuduummm!!!! the function worrks
is_match(first_name_1 = "Easton", first_name_2 = NA)

#changing our function so that we can compare any 2 values
is_match <- function(value_1, value_2) {
  result <- value_1 == value_2  # Don't forget to change the variable names here!!
  result <- if_else(is.na(result), FALSE, result)
  result
}

#trying out our new function to see whether it works for the various comparisons
people %>% 
  mutate(
    name_first_match = is_match(name_first_1, name_first_2),
    name_last_match  = is_match(name_last_1, name_last_2),
    street_match     = is_match(street_1, street_2)
  ) %>% 
  # Order like columns next to each other for easier comparison
  select(id_1, starts_with("name_f"), starts_with("name_l"), starts_with("s"))

#Giving your function arguments default values
increment <- function(x) {
  x + 1
}
increment(2)
increment <- function(x, by) {
  x + by
}
increment(2, 2)
increment(2)#returns an error because the by argument is missing
increment <- function(x, by = 1) {x + by}
# Default value
increment(2)
# Passing the value 1
increment(2, 1)
# Passing a value other than 1
increment(2, 2)
# Passing a vector of numbers to the x argument
increment(c(1, 2, 3), 2)

#the values your function returns is a result of running each line...so where the return function is positioned also matters
is_match <- function(value_1, value_2) {
  result <- value_1 == value_2                     # Do this first
  result <- if_else(is.na(result), FALSE, result)  # Then this
  result                                           # Then this
}
is_match <- function(value_1, value_2) {
  result <- value_1 == value_2 
  result <- if_else(is.na(result), FALSE, result)
  }
is_match("Easton", "Easton")

#if you assign values to the objects inside the function no value is returned 
increment <- function(x, by = 1) {
  x + by # Last line doesn't assign the value to an object
}
increment(2)
increment <- function(x, by = 1) {
  out <- x + by # Now we assign the value to an object
  out           # Type object name on last line of the function body 
}
increment(2)
#using the return function
increment <- function(x, by = 1) {
  out <- x + by 
  return(out)   
}
increment(2)

#position of return() function
increment <- function(x, by = 1) {
  out <- x + by 
  out <- out + 1 # Adding an extra 1
  return(out)    # Return still in the last line
}
increment(2)

#moving the return() function will yield a different answer 
increment <- function(x, by = 1) {
  out <- x + by 
  return(out)    # Return in the second line above adding an extra 1
  out <- out + 1 # Adding an extra 1
}
increment(2)

#can also use the print() function
increment <- function(x, by = 1) {
  out <- x + by 
  print(out)   
}
increment(2)

#Lexical scoping and functions. Objects created inside our function are not stored in our global environment Instead R stores them in one of its multiple environments and uses lexical scoping rules to look  for the objects in my R code

#the code below shows that objects in our function cannot be used outside the function
increment <- function(x, by = 1) {
  out <- x + by # Assign the value to the out object inside the function
  out           
}
x <- increment(2)
x
add <- function(x) {
  x + y
}
add(2)#no value was passed to y hence R returns an error
#lets create y in the global environment
y <- 100
add(2)

#if you assign a value to y inside the function that will be prioritised
add <- function(x) {
  y <- 1
  x + y
  }
y <- 100
add(2)

#Tidy evaluation
#a code to calculate percentages
study %>% count(age_group) %>% mutate(percent = n / sum(n) * 100)
#a function
cat_stats <- function(var) {
  study %>% 
    count(age_group) %>% 
    mutate(percent = n / sum(n) * 100)
}
cat_stats()

#generalizing this function please note the double bracketing at var
cat_stats <- function(var) {
  study %>% 
    count({{ var }}) %>% 
    mutate(percent = n / sum(n) * 100)
}
cat_stats(age_group)
cat_stats(gender)
cat_stats(bmi_3cat)

#generate some additional data
other_study <- tibble(
  id = 1:10,
  age_group = c(rep("Younger", 9), "Older"),
) %>% print()

#attempting to use the function with this new data set
cat_stats(age_group)#yields results relating to the first dataset

#this is what our function currently looks like
cat_stats <- function(var) {
  study %>% 
    count({{ var }}) %>% 
    mutate(percent = n / sum(n) * 100)
}
#adding some new arguments
cat_stats <- function(data, var) {
  study %>% 
    count({{ var }}) %>% 
    mutate(percent = n / sum(n) * 100)
}
#changing and generalizing the study part
cat_stats <- function(data, var) {
  data %>% 
    count({{ var }}) %>% 
    mutate(percent = n / sum(n) * 100)
}
#using the function with the new data frame
cat_stats(other_study, age_group)
#can also be written differently
other_study %>% cat_stats(age_group)
