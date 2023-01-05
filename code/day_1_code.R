# Advent of Code Day 1, 2022

# load libraries
library(tidyverse)

# read in data
data <- read.table("https://raw.githubusercontent.com/haleyepperlyfox/advent_of_code_2022/main/data/day_1_data.txt", sep = '\t', blank.lines.skip = FALSE, header = FALSE)

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

# look at first row, column 2 to find elf with the highest calorie snacks
head(data_3[1,2])

# sum the top 3 rows to get the sum of calories carried by the top 3 elves
data_3 %>%
  slice_head(n = 3) %>%
  summarise(sum = sum(real_sum))
