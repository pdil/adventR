
library(readr)
library(dplyr)

input <- read_lines("input")

input_split <- input %>% 
  lapply(strsplit, split = "x") %>% 
  unlist %>% 
  matrix(ncol = 3, byrow = TRUE) %>% as.data.frame(stringsAsFactors = FALSE)

input_split$V1 <- as.numeric(input_split$V1)
input_split$V2 <- as.numeric(input_split$V2)
input_split$V3 <- as.numeric(input_split$V3)

input_split <- input_split %>% mutate(side1 = V1*V2, side2 = V1*V3, side3 = V2*V3) %>%
  mutate(smallest_side = pmin(side1, side2, side3), surface = 2 * side1 + 2 * side2 + 2 * side3)

sum(input_split$surface) + sum(input_split$smallest_side)
