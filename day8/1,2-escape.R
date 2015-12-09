### needs to be updated

library(readr)

input <- read_lines("input.txt")
escape_len <- sum(nchar(input))

input <- gsub("(\\\"|\\\\)", "00", x = input)

sum(nchar(input)) - escape_len + 2 * length(input)

# 1350

# 2085
