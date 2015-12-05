
library(readr)
input <- read_lines("input.txt")

sum(grepl("(.*[aeiou]){3}", input) & !grepl("(ab|cd|pq|xy)", input) & grepl("(.)\\1", input))
#258

sum(grepl("(..).*\\1", input, perl = TRUE) & grepl("(.).\\1", input))
#53
