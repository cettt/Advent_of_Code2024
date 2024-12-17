data17 <- readLines("Input/day17.txt")

reg <- as.numeric(gsub("\\D+", "", data17[1:3]))
prgm <- as.integer(strsplit(data17[5], "\\D+")[[1]][-1])
out <- integer()


idx <- 1L
while (idx <= length(prgm)) {
 
  if (prgm[idx] == 0L) { #adv
    reg[1] <- floor(reg[1] / 2^(c(0:3, reg)[prgm[idx + 1L] + 1L]))
    idx <- idx + 2L
  }  else if (prgm[idx] == 1) {
    reg[2] <- bitwXor(reg[2], prgm[idx + 1L])
    idx <- idx + 2L
  }  else if (prgm[idx] == 2) {
    reg[2] <- c(0:3, reg)[prgm[idx + 1L] + 1L] %% 8
    idx <- idx + 2L
  }  else if (prgm[idx] == 3 && reg[1] != 0) { #jump
    idx <- prgm[idx + 1L] + 1L
  }else if (prgm[idx] == 3) {
    idx <- idx + 2
  }  else if (prgm[idx] == 4) {
    reg[2] <- bitwXor(reg[2], reg[3])
    idx <- idx + 2L
  }  else if (prgm[idx] == 5) { #out
    out <- c(out, c(0:3, reg)[prgm[idx + 1L] + 1L] %% 8)
    idx <- idx + 2L
  } else if (prgm[idx] == 6) { #bdv
    reg[2] <- floor(reg[1] / 2^(c(0:3, reg)[prgm[idx + 1L] + 1L]))
    idx <- idx + 2L
  } else if (prgm[idx] == 7) { #cdv
    reg[3] <- floor(reg[1] / 2^(c(0:3, reg)[prgm[idx + 1L] + 1L]))
    idx <- idx + 2L
  }
}

paste(out, collapse = ",")

# part 2----------

run <- function(A) { #mimics  the program
  res <- integer()
  while (A > 0) {
    B <- 7 - (A %% 8) # 1 and 2
    C <- floor(A / 2^B)
    B <- 8 * floor(C / 8) + 7 - bitwXor(B, C %% 8)
    res <- c(res, B %% 8)
    A <- floor(A / 2^3)
  }
  return(res)
}


cur <- 0
for (k in rev(seq_along(prgm))) {
  cur2 <- numeric()
  for(.cur in cur) {
    res <- sapply(.cur * 8 + 0:7, \(x) f(x)[1])
    cur2 <- c(cur2, which(res == prgm[k]) - 1 + .cur * 8)
  }
  cur <- cur2
}

sprintf("%.f", min(cur))

