
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
df$score <- rep(0, length(df$name))

# distances after 2503 seconds
end <- 2503

for (t in 1:end) {
  df <- df %>% transform(distance = speed * runtime * floor(t / (runtime + resttime)) + 
                               speed * pmin(runtime, t %% (runtime + resttime)))
  winners <- (df %>% filter(distance == max(distance)))$name
  df$score[df$name %in% winners] <- df$score[df$name %in% winners] + 1
}

max(df$score)

# 1102