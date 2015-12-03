
library(readr)
library(dplyr)
library(plyr)
# read input from file
input <- read_file("input.txt")
# split input string into individual characters
input_split <- strsplit(input, split = "") %>% unlist
# function to determine move direction
get_coord <- function(move) {
  x <- 0; y <- 0
  
  if (move == "^") {
    y <- 1
  } else if (move == ">") {
    x <- 1
  } else if (move == "v") {
    y <- -1
  } else if (move == "<") {
    x <- -1
  }
  
  data.frame(x = x, y = y)
}
# find move direction for each character
moves <- ldply(input_split, get_coord) %>% rbind(data.frame(x = 0, y = 0))
# sum move directions to get house locations
coords <- data.frame(x = cumsum(moves$x), y = cumsum(moves$y))
coords <- coords %>% mutate(pair = paste0(x, ", ", y))
# determine number of unique houses
length(unique(coords$pair))

# 2565
