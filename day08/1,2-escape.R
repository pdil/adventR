### needs to be updated

library(readr)

input <- read_lines("input.txt")

# Part 1

sum(nchar(input)) - 
  sum(sapply(input, function(line) eval(parse(text = sprintf("nchar(iconv(%s, sub = '.'))", line)))))

# 1350

# Part 2

sum(nchar(sapply(input, deparse))) - sum(nchar(input))

# 2085
