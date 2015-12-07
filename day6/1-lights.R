
library(readr)
library(plyr)

input <- read_lines("input.txt")
lights <- matrix(rep(-1, 1e6), ncol = 1000)

parse <- function(line) {
  s <- unlist(strsplit(line, split = " "))
  
  if (s[1] == "turn") {
    instruction <- paste(s[1], s[2])
    st <- s[3]
    e <- s[5]
  } else {
    instruction <- "toggle"
    st <- s[2]
    e <- s[4]
  }
  
  data.frame(start = st, end = e, instr = instruction, stringsAsFactors = FALSE)
}

instructions <- ldply(input, parse)


for (i in 1:nrow(instructions)) {
  start <- instructions[i, "start"]
  end <- instructions[i, "end"]
  instr <- instructions[i, "instr"]
  
  start_x <- as.numeric(unlist(strsplit(start, split = ","))[1])
  start_y <- as.numeric(unlist(strsplit(start, split = ","))[2])
  end_x <- as.numeric(unlist(strsplit(end, split = ","))[1])
  end_y <- as.numeric(unlist(strsplit(end, split = ","))[2])
  
  if (instr == "toggle") {
    lights[start_y:end_y, start_x:end_x] <- -lights[start_y:end_y, start_x:end_x]
  } else if (instr == "turn on") {
    lights[start_y:end_y, start_x:end_x] <- 1
  } else if (instr == "turn off") {
    lights[start_y:end_y, start_x:end_x] <- -1
  }
}

length(which(lights == 1))

# 400410
