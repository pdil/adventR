
library(readr)
library(dplyr)
# read input from file
input <- read_lines("input")
# separate each line into three separate numbers, convert to data frame
input_split <- input %>% lapply(strsplit, split = "x") %>% unlist %>% 
  matrix(ncol = 3, byrow = TRUE) %>% as.data.frame(stringsAsFactors = FALSE)
# coerce characters to numbers
input_split$V1 <- as.numeric(input_split$V1)
input_split$V2 <- as.numeric(input_split$V2)
input_split$V3 <- as.numeric(input_split$V3)
# compute relevant quantities
input_split <- input_split %>%
  mutate(peri1 = 2*V1 + 2*V2, peri2 = 2*V1 + 2*V3, peri3 = 2*V2 + 2*V3, vol = V1*V2*V3) %>%
  mutate(smallest_peri = pmin(peri1, peri2, peri3))
# result is sum of smallest perimeters and sum of all volumes
sum(input_split$smallest_peri) + sum(input_split$vol) 

# 3812909