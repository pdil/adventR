
library(readr)

input <- read_file("input.txt")               # import input file
input_edit <- gsub("[(]", "", input)          # remove all '('

num_up <- nchar(input) - nchar(input_edit)    # compare before and after length ('up' moves)
num_down <- nchar(input) - num_up             # the rest correspond to down moves

num_up - num_down                             # the result is number of up moves minus number of down moves