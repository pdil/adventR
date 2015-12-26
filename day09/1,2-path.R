
library(readr)
library(combinat)
library(dplyr)
library(plyr)

input <- read_lines("input.txt")
lines <- strsplit(input, split = " ")

parse_input <- function(l) {
  l1 <- l[1]
  l2 <- l[3]
  d <- as.numeric(l[5])
  
  data.frame(loc1 = l1, loc2 = l2, dist = d, stringsAsFactors = FALSE)
}

df <- ldply(lines, parse_input)

paths <- permn(unique(c(df$loc1, df$loc2)))

get_dist <- function(path) {
  sum <- 0
  for (k in 1:(length(path) - 1)) {
    sum <- sum + 
      (df %>% filter((loc1 == path[k] & loc2 == path[k + 1]) | (loc1 == path[k + 1] & loc2 == path[k])) %>% 
         select(dist))$dist
    
  }
  
  return(sum)
}

distances <- sapply(paths, get_dist)

min(distances) # part 1: 207
max(distances) # part 2: 804

