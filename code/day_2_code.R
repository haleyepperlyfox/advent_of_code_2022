# Advent of Code Day 2, 2022

# load libraries
library(tidyverse)

# read in data
data <- read.table("https://raw.githubusercontent.com/haleyepperlyfox/advent_of_code_2022/main/data/day_2_data.txt", header = FALSE)


### Part 1

# A = Rock
# B = Paper
# C = Scissors

# X = Rock (1 pt)
# Y = Paper (2 pts)
# Z = Scissors (3 pts)

# lose = 0 pts
# tie = 3 pts
# win = 6 pts

# rename columns
data <- data %>% 
  rename(elf = V1, me = V2)

# add column with pts for rock, paper or scissors
data_1 <- data %>% 
  mutate(type_pt = case_when(me == "X" ~ 1,
                             me == "Y" ~ 2,
                             TRUE ~ 3))

# add column for pts with whether I win or lose
data_2 <- data_1 %>% 
  mutate(outcome = case_when(elf == "A" & me == "X" ~ 3,
                             elf == "A" & me == "Y" ~ 6,
                             elf == "A" & me == "Z" ~ 0,
                             elf == "B" & me == "X" ~ 0,
                             elf == "B" & me == "Y" ~ 3,
                             elf == "B" & me == "Z" ~ 6,
                             elf == "C" & me == "X" ~ 6,
                             elf == "C" & me == "Y" ~ 0,
                             elf == "C" & me == "Z" ~ 3,
                             TRUE ~ NA_real_))

# get scores per round by summing type_pt and outcome columns
data_3 <- data_2 %>% 
  mutate(round_score = type_pt + outcome)

# total score
sum(data_3$round_score)


### Part 2

# A = Rock
# B = Paper
# C = Scissors

# X = lose (0 pts)
# Y = tie (3 pts)
# Z = win (6 pts)

# Rock (1 pt)
# Paper (2 pts)
# Scissors (3 pts)

# add column for which item I need to play based on whether I need to win, lose or draw
data_4 <- data %>% 
  mutate(item = case_when(elf == "A" & me == "X" ~ "scissors",
                             elf == "A" & me == "Y" ~ "rock",
                             elf == "A" & me == "Z" ~ "paper",
                             elf == "B" & me == "X" ~ "rock",
                             elf == "B" & me == "Y" ~ "paper",
                             elf == "B" & me == "Z" ~ "scissors",
                             elf == "C" & me == "X" ~ "paper",
                             elf == "C" & me == "Y" ~ "scissors",
                             elf == "C" & me == "Z" ~ "rock",
                             TRUE ~ "NA"))

# add column for item pts
data_5 <- data_4 %>% 
  mutate(item_pt = case_when(item == "scissors" ~ 3,
                             item == "rock" ~ 1,
                             TRUE ~ 2))

# add column for pts with win, loss, draw
data_6 <- data_5 %>% 
  mutate(game_pt = case_when(me == "X" ~ 0,
                             me == "Y" ~ 3,
                             TRUE ~ 6))

# sum item and game pt columns to get scores per round
data_7 <- data_6 %>% 
  mutate(round_score = item_pt + game_pt)

# total score
sum(data_7$round_score)
