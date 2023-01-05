# Advent of Code Day 1, 2022

# load libraries
library(tidyverse)

# read in data
data <- read.table("/Users/haleyfox/Documents/advent_of_code/data/day_1_data.txt", sep = '\t', blank.lines.skip = FALSE, header = FALSE)

# add column that sums row i with i+1
data_1 <- data %>% 
  mutate(sum = cumsum(coalesce(V1, 0)))

# create new column that includes the sum only where V1 is null (line break)
# create a new df with only the non-NA values of new  
data_2 <- data_1 %>% 
  mutate(new = case_when(!is.na(V1) ~ NA_real_,
                         is.na(V1) ~ sum,
                         TRUE ~ NA_real_)) %>% 
  filter(!is.na(new)) %>% 
  select(new)

# subtract previous value in column to get the sum per group
data_3 <- data_2 %>% 
  mutate(real_sum = (new - lag(new))) %>% 
  arrange(desc(real_sum))

# look at first row, column 2 to get answer
head(data_3[1,2])
