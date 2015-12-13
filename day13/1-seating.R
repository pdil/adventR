
library(readr)
library(combinat)
library(dplyr)
library(plyr)

input <- read_lines("input.txt")
lines <- strsplit(input, " ")

parse_input <- function(line) {
  name <- line[1]
  change <- ifelse(line[3] == "gain", 1, -1) * as.numeric(line[4])
  next_to <- substr(line[11], 1, nchar(line[11]) - 1) # remove period also
  
  data.frame(name = name, change = change, next_to = next_to)
}

df <- ldply(lines, parse_input)

df$name <- as.character(df$name)
df$next_to <- as.character(df$next_to)
comb <- permn(unique(df$name))

sums <- c()

for (j in 1:length(comb)) {
  total <- 0
  
  total <- total + (df %>% filter(name == comb[[j]][1], next_to == comb[[j]][8]))$change
  total <- total + (df %>% filter(name == comb[[j]][1], next_to == comb[[j]][2]))$change
  
  for (k in 2:(length(comb[[j]]) - 1)) {
    total <- total + (df %>% filter(name == comb[[j]][k], next_to == comb[[j]][k + 1]))$change
    total <- total + (df %>% filter(name == comb[[j]][k], next_to == comb[[j]][k - 1]))$change
  }
  
  total <- total + (df %>% filter(name == comb[[j]][8], next_to == comb[[j]][7]))$change
  total <- total + (df %>% filter(name == comb[[j]][8], next_to == comb[[j]][1]))$change
  
  sums <- c(sums, total)
}

max(sums)

# 733
