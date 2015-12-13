
library(magrittr)

x <- "1113122113"

for (i in 1:50) {
  r <- x %>% strsplit("") %>% unlist %>% rle
  x <- rbind(as.character(r$lengths), r$values) %>% paste(collapse = "")
}

nchar(x)

# 5103798