
library(readr)

input <- as.numeric(read_lines("input.txt"))

count <- function(n, m) {
  if (n < 0 | m <= 0) {
    return(0)
  }
  
  if (n == 0) {
    return(1)
  }
  
  return(count(n, m - 1) + count(n - input[m], m))
}

count(150, length(input))
