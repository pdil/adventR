
library(digest)

input <- "bgvyzdsv"

keys <- paste0(input, 1:5e5)
hashes <- sapply(keys, digest, algo = "md5", serialize = FALSE)
which(substr(hashes, 1, 5) == "00000")

# 254575

keys2 <- paste0(input, 1e6:2e6)
hashes2 <- sapply(keys2, digest, algo = "md5", serialize = FALSE)
which(substr(hashes2, 1, 6) == "000000")

# 1038736

# One liners:
which(substr(sapply(paste0(input, 1:5e5), digest, serialize = FALSE), 1, 5) == "00000")
which(substr(sapply(paste0(input, 1e6:2e6), digest, serialize = FALSE), 1, 6) == "000000") + 1e6
