# Advent of Code Day 4, 2022

# load libraries
library(tidyverse)

# read in data
data <- read.table("https://raw.githubusercontent.com/haleyepperlyfox/advent_of_code_2022/main/data/day_4_data.txt", header = FALSE)


### Part 1

# split into 4 columns using regex - if:
# col 1 >= column 3 & col 2 <= column 4 
# then the second elf completely encompasses the first elf. If 
# col 3 >= column 1 & col 4 <= column 2
# then the first elf completely encompasses the second elf.

data_1 <- data %>% 
  separate(V1, ",", into=c("elf1", "elf2")) %>% 
  separate(elf1, "-", into=c("elf1_low", "elf1_high")) %>% 
  separate(elf2, "-", into=c("elf2_low", "elf2_high")) %>% 
  mutate_all(as.numeric) %>% 
  mutate(encompass = case_when(elf1_low >= elf2_low & elf1_high <= elf2_high ~ 1,
                               elf2_low >= elf1_low & elf2_high <= elf1_high ~ 1,
                               TRUE ~ 0))
sum(data_1$encompass)

### Part 2 - partial overlap

data_2 <- data_1 %>% 
  mutate(partial = case_when(elf2_low <= elf1_low & elf2_high <= elf1_high & elf2_high >= elf1_low ~ 1,
                             elf1_low <= elf2_low & elf1_high <= elf2_high & elf1_high >= elf2_low ~ 1,
                             elf1_low >= elf2_low & elf1_high <= elf2_high ~ 1,
                             elf2_low >= elf1_low & elf2_high <= elf1_high ~ 1,
                             TRUE ~ 0))
sum(data_2$partial)
