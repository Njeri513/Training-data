#The tidyr package 
library(tidyr)
library(dplyr)
library(ggplot2)
#Pivoting longer
#simulating some demo data
babies <- tibble(
  id       = 1001:1008,
  sex      = c("F", "F", "M", "F", "M", "M", "M", "F"),
  weight_3  = c(9, 11, 17, 16, 11, 17, 16, 15),
  weight_6  = c(13, 16, 20, 18, 15, 21, 17, 16),
  weight_9  = c(16, 17, 23, 21, 16, 25, 19, 18),
  weight_12 = c(17, 20, 24, 22, 18, 26, 21, 19)
) %>% print()

#using pivot_longer() to change this data into a person_period
babies_long <- babies %>% 
  pivot_longer(
    cols         = starts_with("weight"),
    names_to     = "months",
    names_prefix = "weight_",
    values_to    = "weight"
  ) %>% print()

#The names to argument
babies %>% 
  pivot_longer(
    cols = starts_with("weight")
  )
#names the column names month which isnt very informative change that to month
babies %>% 
  pivot_longer(
    cols     = starts_with("weight"),
    names_to = "months"
  )

#The names Prefix Argument
babies %>% 
  pivot_longer(
    cols         = starts_with("weight"),
    names_to     = "months",
    names_prefix = "weight_"
  )
#you can alternatively do this
babies %>% 
  pivot_longer(
    cols         = starts_with("weight"),
    names_to     = "months",
    names_prefix = "\\w+_"
  )
#The values_to argument
babies %>% 
  pivot_longer(
    cols         = starts_with("weight"),
    names_to     = "months",
    names_prefix = "weight_",
    values_to    = "weight"
  )

#names_transform argument
#coercing type for months from char to integer
babies %>% 
  pivot_longer(
    cols         = starts_with("weight"),
    names_to     = "months",
    names_prefix = "weight_",
    values_to    = "weight"
  ) %>% 
  mutate(months = as.integer(months))
#alternatively use pivot_longer function
babies %>% 
  pivot_longer(
    cols            = starts_with("weight"),
    names_to        = "months",
    names_prefix    = "weight_",
    names_transform = list(months = as.integer),
    values_to       = "weight"
  )
#pivoting multiple sets of columns
#simulate some data
set.seed(123)
babies <- tibble(
  id       = 1001:1008,
  sex      = c("F", "F", "M", "F", "M", "M", "M", "F"),
  weight_3  = c(9, 11, 17, 16, 11, 17, 16, 15),
  weight_6  = c(13, 16, 20, 18, 15, 21, 17, 16),
  weight_9  = c(16, 17, 23, 21, 16, 25, 19, 18),
  weight_12 = c(17, 20, 24, 22, 18, 26, 21, 19),
  length_3  = c(17, 19, 23, 20, 18, 22, 21, 18),
  length_6  = round(length_3 + rnorm(8, 2, 1)),
  length_9  = round(length_6 + rnorm(8, 2, 1)),
  length_12 = round(length_9 + rnorm(8, 2, 1)),
) %>% print()

#The desired result looks like this
babies %>% 
  pivot_longer(
    cols      = c(-id, -sex),
    names_to  = c(".value", "months"),
    names_sep = "_"
  )
#resets the weight level only
babies_long <- babies %>% 
  pivot_longer(
    cols         = starts_with("weight"),
    names_to     = "months",
    names_prefix = "weight_",
    values_to    = "weight"
  ) %>% print()
#this still doesnt give the desired result
babies_long <- babies %>% 
  pivot_longer(
    cols         = c(-id, -sex),
    names_to     = "months",
    names_prefix = "weight_",
    values_to    = "weight"
  ) %>% print()
#attempted combining and passing them to names_prefix gives an error
babies_long <- babies %>% 
  pivot_longer(
    cols         = c(-id, -sex),
    names_to     = "months",
    names_prefix = c("weight_", "length_"),
    values_to    = "weight"
  ) %>%  print()
#This tells R to combine all columns apart from id and sex column
babies_long <- babies %>% 
  pivot_longer(
    cols      = c(-id, -sex),
    names_to  = "months",
    values_to = "weight"
  ) %>% print()
#confusion and mixture in the months columns.drop values 
babies_long <- babies %>% 
  pivot_longer(
    cols     = c(-id, -sex),
    names_to = "months"
  ) %>% print()
#the names_sep argument
babies_long <- babies %>% 
  pivot_longer(
    cols      = c(-id, -sex),
    names_to  = c("measure", "months"),
    names_sep = "_"
  ) %>% print()
#.value special. Tells R that the vector after the underscore is the value
babies_long <- babies %>% 
  pivot_longer(
    cols      = c(-id, -sex),
    names_to  = c(".value", "months"),
    names_sep = "_",
    names_transform = list(months = as.integer)
  ) %>% print()
#The above code results in 3 separate columns

#the code is slightly different and .value results due to order of values passed to name to function
babies %>% 
  pivot_longer(
    cols      = c(-id, -sex),
    names_to  = c("months", ".value"),
    names_sep = "_"
  )
#plotting a graph of weight vs height
babies_long %>% 
  mutate(months = factor(months, c(3, 6, 9, 12))) %>% 
  ggplot() +
  geom_point(aes(weight, length, color = months)) +
  labs(
    x = "Weight (Pounds)",
    y = "Length (Inches)",
    color = "Age (Months)"
  ) +
  theme_classic()
#pivoting wider
#person_period to person_level
babies <- babies_long %>% 
  pivot_wider(
    names_from  = "months",
    values_from = c("weight", "length")
  ) %>% print()

#simulating some additional data to demonstrate this
df <- tribble(
  ~id, ~measure, ~lbs_inches,
  1, "weight", 9,
  1, "length", 17,
  2, "weight", 11,
  2, "length", 19 
) %>%  print()
#pivot wider
df %>% pivot_wider(
  names_from  = "measure",
  values_from = "lbs_inches"
)

#pivoting summary statistics 
#pivoting summary statistics wide to long.Remember to load dplyr and tidyverse if opening a new session
mean_weights <- babies %>% 
  summarise(
    mean(weight_3),
    sd(weight_3),
    mean(weight_6),
    sd(weight_6),
    mean(weight_9),
    sd(weight_9),
    mean(weight_12),
    sd(weight_12),
  ) %>% print()
#the code above presents the data in a wide format.
#the code below addresses that and presents the same data in long structure
mean_weights %>% 
  pivot_longer(
    cols = everything(),
    names_to = c(".value", "measure", "months"),
    names_pattern = "(\\w+)\\((\\w+)_(\\d+)"
  )
#note the regular expression used under names_pattern
#Pivoting summary statistics long to wide
#simulating some data
summary_stats <- tribble(
  ~period, ~behavior, ~value, ~n, ~n_total, ~percent,
  "School Year Weekends", "Long sleeve shirt", "Never", 6, 78,  8,  
  "School Year Weekends", "Long sleeve shirt", "Seldom", 16, 78,    21, 
  "School Year Weekends", "Long sleeve shirt", "Sometimes", 33, 78, 42, 
  "School Year Weekends", "Long sleeve shirt", "Often", 17, 78, 22, 
  "School Year Weekends", "Long sleeve shirt", "Always", 6, 78, 8,  
  "School Year Weekends", "Long Pants", "Never", 5, 79, 6,  
  "School Year Weekends", "Long Pants", "Seldom",   15, 79, 19, 
  "School Year Weekends", "Long Pants", "Sometimes", 32, 79, 41,    
  "School Year Weekends", "Long Pants", "Often", 19, 79, 24,    
  "School Year Weekends", "Long Pants", "Always",   8, 79, 10,  
  "Summer", "Long sleeve shirt", "Never",   9, 80, 11,  
  "Summer", "Long sleeve shirt", "Seldom", 18, 80, 22,  
  "Summer", "Long sleeve shirt", "Sometimes", 31,   80, 39, 
  "Summer", "Long sleeve shirt", "Often",   14, 80, 18, 
  "Summer", "Long sleeve shirt", "Always", 8,   80, 10, 
  "Summer", "Long Pants", "Never", 7,   76, 9,  
  "Summer", "Long Pants", "Seldom", 16, 76, 21, 
  "Summer", "Long Pants", "Sometimes", 27,  76, 36, 
  "Summer", "Long Pants", "Often", 18, 76,  24, 
  "Summer", "Long Pants", "Always", 8, 76,  11
) %>%  print()

#changes to wide
summary_stats %>% 
  # Combine n and percent into a single character string
  mutate(n_percent = paste0(n, " (", percent, ")")) %>% 
  # We no longer need n, n_total, percent
  select(-n:-percent) %>% 
  pivot_wider(
    names_from = "period",
    values_from = "n_percent"
  )

#Tidy data each variable has its own column each observation have its own row each value its own cell
#each variable must have its own column
births_ntd <- tibble(
  state   = rep(c("CA", "FL", "TX"), each = 2),
  outcome = rep(c("births", "neural tube defects"), 3),
  count   = c(454920, 318, 221542, 155, 378624, 265)
) %>% print()
#tidy this data using pivot wider
births_ntd %>% 
  pivot_wider(
    names_from  = "outcome",
    values_from = "count"
  )
#how to deal with column names that may include a value
births_sex <- tibble(
  state  = c("CA", "FL", "TX"),
  f_2018 = c(222911, 108556, 185526),
  m_2018 = c(232009, 112986, 193098)
) %>% print()
#tidy data by giving sex and year a new column
births_sex %>% 
  pivot_longer(
    cols      = -state,
    names_to  = c("sex", "year"),
    names_sep = "_",
    values_to = "births" )
#data looks more presentable
#each observation must have is own row. In the babies data the same observation eg weight violated this
births_decade <- tibble(
  state  = c("CA", "FL", "TX"),
  `2010` = c(409428, 199388, 340762),
  `2020` = c(454920, 221542, 378624)
) %>%  print()
#the data may be rearranged to look like this
births_decade %>% 
  pivot_longer(
    cols      = -state,
    names_to  = "year",
    values_to = "births"
  )
#each value must have its own cell
baby_sleep <- tibble(
  id          = c(1001, 1002, 1003),
  sleep_range = c(".5-2", ".75-2.4", "1.1-3.8")
) %>% print()
#used tidy separate to present the value in a different way
baby_sleep %>% 
  separate(
    col     = sleep_range,
    into    = c("min_hours", "max_hours"),
    sep     = "-",
    convert = TRUE )
#The complete function used to fill holes in the data
reports <- tibble(
  date      = as.Date(c(
    "2019-10-29", "2019-10-29", "2019-10-30", "2019-11-02", "2019-11-02"
  )),
  emp_id    = c(5123, 2224, 5153, 9876, 4030),
  report_id = c("a8934", "af2as", "jzia3", "3293n", "dsf98")
) %>% print()
#if one is interested in reports made per day
reports %>% count(date)
reports %>% count(date) %>% summarise(mean_reports_per_day = mean(n))
#the data was collected from 29th october to 2nd october so the days in between not on the tibble mean that there was no case reported. include the zeros to avoid error when calculating mean etc
reports %>% 
  count(date) %>% 
  complete(
    date = seq.Date(
      from = as.Date("2019-10-28"), 
      to = as.Date("2019-11-03"), 
      by = "days"
    )
  )
#looks better but the problem with this is that the value isnt missing (NA) its value should be 0
reports %>% 
  count(date) %>% 
  complete(
    date = seq.Date(
      from = as.Date("2019-10-28"), 
      to = as.Date("2019-11-03"), 
      by = "days"
    ),
    fill = list(n = 0)
  )
#we can now finally correct the mean value
reports %>% 
  count(date) %>% 
  complete(
    date = seq.Date(
      from = as.Date("2019-10-28"), 
      to = as.Date("2019-11-03"), 
      by = "days"
    ),
    fill = list(n = 0)
  ) %>% 
  summarise(mean_reports_per_day = mean(n))
#as observed the mean changes in the first set R interpreted the data as representative of 3 dates this second time it factored in the dates when the cases were 0
