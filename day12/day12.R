
library(readr)
library(stringr)
library(magrittr)

# Part 1
read_file("input.txt") %>% str_extract_all("-?\\d+") %>% unlist %>% as.numeric %>% sum
# 191164

# Part 2
read_file("input.txt") %>% str_replace_all("\\{.*?\"red\".*?\\}", "0") %>% 
  str_extract_all("-?\\d+") %>% unlist %>% as.numeric %>% sum

