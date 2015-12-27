
library(readr)
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
df %>% 
  mutate(distance = speed * runtime * floor(end / (runtime + resttime)) + 
           speed * pmin(runtime, end %% (runtime + resttime))) %>%
  filter(distance == max(distance))

# 2640