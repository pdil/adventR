
library(readr)

start <- Sys.time()

input <- read_lines("input.txt")
lines <- strsplit(input, split = " ")

a <- 0

while (TRUE) {
  # p1 op p2 -> result
  for (index in 1:length(lines)) {
    p1 <- "0"
    op <- "0"
    p2 <- "0"
    result <- "0"
    
    if (length(lines[[index]]) == 5) {
      p1 <- lines[[index]][1]
      op <- lines[[index]][2]
      p2 <- lines[[index]][3]
      result <- lines[[index]][5]
    } else if (length(lines[[index]]) == 4) {
      op <- "NOT"
      p1 <- lines[[index]][2]
      result <- lines[[index]][4]
    } else if (length(lines[[index]]) == 3) {
      if (lines[[index]][3] == "a") {
        tryCatch({assign(lines[[index]][3], get(lines[[index]][1]))}, error = function(e) e)
      } else {
        assign(lines[[index]][3], as.numeric(lines[[index]][1]))
      }
    }
    
    if (!is.na(as.numeric(p1)))
      p1 <- as.numeric(p1)
    if (!is.na(as.numeric(p2)))
      p2 <- as.numeric(p2)
    
    if (class(p1) != "numeric") {
      test1 <- tryCatch(get(p1, mode = "numeric"), error = function(e) e)
    } else {
      test1 <- "ok"
    }
    
    if (class(p2) != "numeric") {
      test2 <- tryCatch(get(p2, mode = "numeric"), error = function(e) e)
    } else {
      test2 <- "ok"
    }
  
    if (!inherits(test1, "error") & !inherits(test2, "error")) {
      if (op == "AND") {
        if (class(p1) == "numeric") {
          assign(result, bitwAnd(p1, get(p2, mode = "numeric")))
        } else if (class(p2) == "numeric") {
          assign(result, bitwAnd(get(p1, mode = "numeric"), p2))
        } else {
          assign(result, bitwAnd(get(p1, mode = "numeric"), get(p2, mode = "numeric")))
        }
      } else if (op == "OR") {
        if (class(p1) == "numeric") {
          assign(result, bitwOr(p1, get(p2, mode = "numeric")))
        } else if (class(p2) == "numeric") {
          assign(result, bitwOr(get(p1, mode = "numeric"), p2))
        } else {
          assign(result, bitwOr(get(p1, mode = "numeric"), get(p2, mode = "numeric")))
        }
      } else if (op == "NOT") {
        assign(result, bitwNot(get(p1, mode = "numeric")))
      } else if (op == "LSHIFT") {
        assign(result, bitwShiftL(get(p1, mode = "numeric"), p2))
      } else if (op == "RSHIFT") {
        assign(result, bitwShiftR(get(p1, mode = "numeric"), p2))
      } else {
        # print("nope")
      }
      
      tryCatch({
        if (get(result, mode = "numeric") < 0) {
          assign(result, 65536 + get(result, mode = "numeric"))
        }
      }, error = function(e) e)
      
      input <- input[-index]
    }
  }

  
  if (!is.na(a) && a != 0) {
    print(a)
    break
  }
}

Sys.time() - start

# 956