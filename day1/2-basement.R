
library(readr)
# read input file
input <- read_file("input.txt")
# split string into individual elements
input_vec <- substring(input, 1:nchar(input), 1:nchar(input))
# replace '(' with 1 and ')' with -1
input_vec[input_vec == "("] <- "1"
input_vec[input_vec == ")"] <- "-1"
# convert to numbers and find cumulative sum
input_vec <- as.numeric(input_vec)
cumulative_floor <- cumsum(input_vec)
# find first element where cumulative sum is negative
which(cumulative_floor < 0)[1]

# 1795