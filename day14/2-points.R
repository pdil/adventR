
library(readr)
library(plyr)
library(dplyr)

parse_lines <- function(line) {
  data.frame(
    name = line[1],
    speed = as.numeric(line[4]), 
    runtime = as.numeric(line[7]), 
    resttime = as.numeric(line[14])
  )
}

df <- read_lines("input.txt") %>% strsplit(" ") %>% ldply(parse_lines)

# distances after 2503 seconds
end <- 2503

iterate_second <- function(t) {
  df <- df %>% mutate(distance = speed * runtime * floor(t / (runtime + resttime)) + 
                        speed * pmin(runtime, t %% (runtime + resttime)))
  
  max(df$distance)
}

result <- 1:end %>% ldply(iterate_second)
