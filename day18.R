n <- 71L
data18 <- as.matrix(read.table("Input/day18.txt", sep = ","))

byte <- (data18[,1] * n) + data18[,2] + 1L


idx_k <- function(k) {
  k + c(if (k > n) - n, if (k <= n^2 - n) n, if (k %% n != 1L) -1L, if (k %% n != 0L) 1L) 
}

lookup <- lapply(seq_len(n^2), idx_k)


find_path <- function(i, part1 = TRUE) {
  
  pos <- 1L
  k <- 1L
  visited <- pos
  gr2 <- byte[1:i]
  
  while (length(pos) > 0L) {
    
    nxt <- setdiff(unlist(lookup[pos]), visited)
    pos <- nxt[!nxt %in% gr2]
    if (any (pos == n^2)) break
    visited <- c(visited, pos)
    k <- k + 1L
    
  }
  if (part1 && length(pos) > 0) k else if (length(pos) > 0) FALSE else TRUE
}  

# part 1----------
find_path(1024L)


# part 2-------------
low <- 1024L
high <- length(byte)


while (high - low > 1L) {
  i <- trunc((high + low) / 2)
  res <- find_path(i, part1 = FALSE)
  
  if (res)  high <- i else low <- i
}

paste(data18[high,], collapse = ",")
