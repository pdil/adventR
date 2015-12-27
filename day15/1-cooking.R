
library(readr)
library(plyr)
library(dplyr)

parse_lines <- function(line) {
  data.frame(
    name = substr(line[1], 1, nchar(line[1]) - 1),
    capacity = as.numeric(substr(line[3], 1, nchar(line[3]) - 1)), 
    durability = as.numeric(substr(line[5], 1, nchar(line[5]) - 1)), 
    flavor = as.numeric(substr(line[7], 1, nchar(line[7]) - 1)),
    texture = as.numeric(substr(line[9], 1, nchar(line[9]) - 1)),
    calories = as.numeric(line[11])
  )
}

df <- read_lines("input.txt") %>% strsplit(" ") %>% ldply(parse_lines)

scores <- c()

for (i in 1:100) {
  for (j in 1:100) {
    for (k in 1:100) {
      for (l in 1:100) {
        if (i + j + k + l == 100) {
          amounts <- c(i, j, k, l)
          scores <- c(scores, max(sum(df$capacity * amounts), 0) * max(sum(df$durability * amounts), 0) *
                        max(sum(df$flavor * amounts), 0) * max(sum(df$texture * amounts), 0))
        }
      }
    }
  }
}

max(scores)

# 21367368