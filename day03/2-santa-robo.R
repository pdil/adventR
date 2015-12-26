
library(readr)
library(dplyr)
library(plyr)
# read input from file
input <- read_file("input.txt")
# separate input string into individual characters
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
moves <- ldply(input_split, get_coord) %>% rbind(data.frame(x = c(0, 0), y = c(0, 0)))
# separate moves into Santa and Robo-Santa sets
santa_moves <- moves[c(TRUE, FALSE), ]
robo_moves <- moves[c(FALSE, TRUE), ]
# sum move directions to get house locations
santa_coords <- data.frame(x = cumsum(santa_moves$x), y = cumsum(santa_moves$y))
robo_coords <- data.frame(x = cumsum(robo_moves$x), y = cumsum(robo_moves$y))
# create ordered pair to find unique locations
santa_coords <- santa_coords %>% mutate(pair = paste0(x, ", ", y))
robo_coords <- robo_coords %>% mutate(pair = paste0(x, ", ", y))
# combine sets of house locations
combined <- rbind(santa_coords, robo_coords)
# determine number of unique houses
length(unique(combined$pair))

#2639
