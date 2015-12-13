
library(magrittr)
library(stringr)

check_password <- function(pass) {
  if (grepl("[iol]", pass)) return(FALSE)
  if (! grepl("(\\w)\\1.*(\\w)\\2", pass)) return(FALSE)
  if (! grepl("(abc|bcd|cde|def|fgh|pqr|qrs|rst|stu|tuv|uvw|vwx|wxy|xyz)", pass)) return(FALSE)
  TRUE
}

password <- "vzbxkghz"

while(check_password(password) == FALSE) {
  chars <- strsplit(password, "") %>% unlist
  
  next_ascii <- sapply(chars[nchar(password)], charToRaw) %>% strtoi(base = 16L) + 1
  
  # z = 122
  if (next_ascii == 123) {
    chars[8] <- "a"
    temp <- chars[7] %>% charToRaw %>% strtoi(base = 16L) + 1
    
    if (temp == 123) {
      chars[7] <- "a"
      temp <- chars[6] %>% charToRaw %>% strtoi(base = 16L) + 1
      
      if (temp == 123) {
        chars[6] <- "a"
        temp <- chars[5] %>% charToRaw %>% strtoi(base = 16L) + 1
        
        if (temp == 123) {
          chars[5] <- "a"
          temp <- chars[4] %>% charToRaw %>% strtoi(base = 16L) + 1
          
          if (temp == 123) {
            chars[4] <- "a"
            temp <- chars[3] %>% charToRaw %>% strtoi(base = 16L) + 1
            
            if (temp == 123) {
              chars[3] <- "a"
              temp <- chars[2] %>% charToRaw %>% strtoi(base = 16L) + 1
              
              if (temp == 123) {
                chars[2] <- "a"
                temp <- chars[1] %>% charToRaw %>% strtoi(base = 16L) + 1
                chars[1] <- temp %>% as.raw %>% rawToChar
              } else {
                chars[2] <- temp %>% as.raw %>% rawToChar
                chars[3:8] <- "a"
              }
            } else {
              chars[3] <- temp %>% as.raw %>% rawToChar
              chars[4:8] <- "a"
            }
          } else {
            chars[4] <- temp %>% as.raw %>% rawToChar
            chars[5:8] <- "a"
          }
        } else {
          chars[5] <- temp %>% as.raw %>% rawToChar
          chars[6:8] <- "a"
        }
      } else {
        chars[6] <- temp %>% as.raw %>% rawToChar 
        chars[7:8] <- "a"
      }
    } else {
      chars[7] <- temp %>% as.raw %>% rawToChar
      chars[8] <- "a"
    }
  } else {
    chars[8] <- next_ascii %>% as.raw %>% rawToChar
  }
  
  password <- paste(chars, collapse = "")
}

password

# vzbxxyzz