
library(readr)
library(stringr)
library(dplyr)

input <- read_lines("input.txt") %>% str_replace_all("(:|,)", "")

df <- data.frame(Sue = numeric(), children = numeric(), cats = numeric(), samoyeds = numeric(),
                 pomeranians = numeric(), akitas = numeric(), vizslas = numeric(), 
                 goldfish = numeric(), trees = numeric(), cars = numeric(), perfumes = numeric())

for (i in 1:length(input)) {
  line <- strsplit(input[i], " ")[[1]]
  
  sue_num <- line[2]
  name1 <- line[3]; num1 <- as.numeric(line[4])
  name2 <- line[5]; num2 <- as.numeric(line[6])
  name3 <- line[7]; num3 <- as.numeric(line[8])
  
  df[i, 1] <- sue_num
  df[i, name1] <- num1
  df[i, name2] <- num2
  df[i, name3] <- num3
}

# The correct Sue has the following
# ---------------------------------
# children: 3, cats: 7, samoyeds: 2, 
# pomeranians: 3 akitas: 0, 
# vizslas: 0, goldfish: 5, 
# trees: 3, cars: 2, perfumes: 1

# PART 1
df %>% 
  filter(children == 3 | is.na(children), cats == 7 | is.na(cats), samoyeds == 2 | is.na(samoyeds), 
         pomeranians == 3 | is.na(pomeranians), akitas == 0 | is.na(akitas), 
         vizslas == 0 | is.na(vizslas), goldfish == 5 | is.na(goldfish), 
         trees == 3 | is.na(trees), cars == 2 | is.na(cars), perfumes == 1 | is.na(perfumes)) %>%
  select(Sue)

# 40

# PART 2
df %>% 
  filter(children == 3 | is.na(children), cats > 7 | is.na(cats), samoyeds == 2 | is.na(samoyeds), 
         pomeranians < 3 | is.na(pomeranians), akitas == 0 | is.na(akitas), 
         vizslas == 0 | is.na(vizslas), goldfish < 5 | is.na(goldfish), 
         trees > 3 | is.na(trees), cars == 2 | is.na(cars), perfumes == 1 | is.na(perfumes)) %>%
  select(Sue)

# 241
