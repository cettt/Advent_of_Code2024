data17 <- readLines("Input/day17.txt")

reg <- as.numeric(gsub("\\D+", "", data17[1:3]))
prgm <- as.integer(strsplit(data17[5], "\\D+")[[1]][-1])
A <- reg[1]
idx <- 1L
out <- integer()


while (idx <= length(prgm)) {
  
  if (prgm[idx] == 3L && reg[1] != 0L) { #jnz
    idx <- prgm[idx + 1L] + 1L
  } else {
    cmbn <- c(0:3, reg)[prgm[idx + 1L] + 1L]
    
    if (prgm[idx] %in% c(0L, 6L, 7L)) { #adv, bdv, cdv
      k <- if (prgm[idx] == 0) 1L else prgm[idx] - 4L
      reg[k] <- floor(reg[1] / 2^cmbn)
    } else if (prgm[idx] == 1L) { #bxl
      reg[2] <- bitwXor(reg[2], prgm[idx + 1L])
    } else if (prgm[idx] == 2L) { #bst
      reg[2] <- cmbn %% 8
    } else if (prgm[idx] == 4L) {#bxc
      reg[2] <- bitwXor(reg[2], reg[3])
    } else if (prgm[idx] == 5L) { #out
      out <- c(out, cmbn %% 8)
    }
    
    idx <- idx + 2L
  } 
}

# part 1.-------------
paste(out, collapse = ",")

# part 2---------

run <- function(A) { #mimics iteration of the program 
  B <- bitwXor(7L, (A %% 8L)) # steps 1 and 2
  C <- A %/% 2^B # step 3
  return(bitwXor(7L, bitwXor(B, C %% 8L))) # steps 5, 6, 7
}

cur <- 0
for (k in rev(seq_along(prgm))) {
  cur2 <- numeric()
  for(.cur in cur) {
    res <- sapply(.cur * 8 + 0:7, run)
    cur2 <- c(cur2, which(res == prgm[k]) - 1 + .cur * 8)
  }
  cur <- cur2
}

sprintf("%.f", min(cur))

# easy part 1------
paste(run(A %/% 8^(0:8)), collapse = ",")

# Explanation program----

# Step 1: 2, 4: set B to A %% 8
# Step 2: 1, 7: set B to bitwXor(B, 7) 
# Step 3: 7, 5: set C to floor(A / 2^B)
# Step 4: 0, 3: set A to floor(A / 8) (can be skipped if only one digit is required)
# Step 5: 4, 4: set B to bitwXor(B, C) 
#   note that before step 5, B is always less than 8. In particular,
#   step 5 is then equivalent to B <- 8 * floor(C / 8) + bitwXor(C %% 8, B)
# Step 6: 1, 7: set B to bitwXor(B, 7)
# Step 7: 5, 5: output B %% 8
# Step 8: 3, 0: go back to Step 1 as long as A > 0

# Since in step 7 we only need the last three bits of B, 
#    we also only need the last three bits in step 6.
#  These are given by bitwXor(7, bitwXor(C %% 8, B)).