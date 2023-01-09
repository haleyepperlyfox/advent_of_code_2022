# Advent of Code Day 3, 2022

# load libraries
library(tidyverse)

# read in data
data <- read.table("https://raw.githubusercontent.com/haleyepperlyfox/advent_of_code_2022/main/data/day_3_data.txt", header = FALSE)


### Part 1

# divide strings in half 
data_1 <- data %>% 
  mutate(pocket_1 = substr(V1, 1, nchar(V1)/2)) %>% 
  mutate(pocket_2 = substr(V1, (nchar(V1)/2)+1, nchar(V1))) 

# for loop to run through each row of data, extract letter that's present 
# in both halves of string, and save as a vector
chars <- vector()
for (row in 1:nrow(data_1)) {
  new_char <- data_1 %>% 
    qgrams(x = .[row,2], y = .[row,3], q = 1) %>% 
    as.data.frame() %>% 
    slice(-1) %>% 
    select(where(~ all(. > 0))) %>% 
    colnames()
  chars <- append(new_char, chars)
}

# reverse the order of the vector
chars_1 <- rev(chars)

# create key connecting letters to numbers
numbers <- c(1:52)
letters_low <- letters[1:26]
uppercase_l <- LETTERS[1:26]
letters <- append(letters_low, uppercase_l)
key <- data.frame(letters, numbers)

# join key to chars df
chars_df <- data.frame(chars_1)
chars_df_1 <- chars_df %>% 
  left_join(key, by = c("chars_1" = "letters"))

# sum of priorities of item types (letters)
sum(chars_df_1$numbers)


### Part 2



