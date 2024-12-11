n <- 53L
x <- as.integer(as.matrix(read.fwf("Input/day10.txt", widths = rep(1, n))))


idx_k <- function(k) {
  k + c(if (k > n) - n, if (k <= n^2 - n) n, if (k %% n != 1L) -1L, if (k %% n != 0L) 1L) 
}


lookup <- sapply(seq_along(x), idx_k)

find_path <- function(k) {
  for (i in seq_len(9)) {
    nxt <- unlist(lookup[k])
    k <- nxt[x[nxt] == i]
  }
  c(length(unique(k)), length(k))
  
}

rowSums(sapply(which(x == 0L), find_path))
