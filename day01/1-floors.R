
library(readr)
# read input file
input <- read_file("input.txt")
# remove all '('
input_edit <- gsub("[(]", "", input)
# compare before and after length (i.e. 'up' moves)
num_up <- nchar(input) - nchar(input_edit)
# the rest correspond to down moves
num_down <- nchar(input) - num_up
# result is number of up moves minus number of down moves
num_up - num_down

# 74