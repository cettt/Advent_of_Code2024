n <- 141L
data20 <- as.character(as.matrix(read.fwf("Input/day20.txt", widths = rep(1, n), comment.char = "")))

wall <- which(data20 == "#")
wall_in <- wall[wall > n & wall <= n^2 - n & wall %% n > 1]
gr <- which(data20 != "#")
start <- which(data20 == "S")
end <- which(data20 == "E")


idx_k <- function(k) {
  k2 <- (k - 1L) %% n^2 + 1
  k + c(if (k2 > n) - n, if (k2 <= n^2 - n) n, if (k2 %% n != 1L) -1L, if (k2 %% n != 0L) 1L) 
}

lookup <- lapply(seq_len(3 * n^2), idx_k)


#normal path---------

pos <- start
visited <- integer()
pre <- integer(n^2)
k <- 0L

while(pos[1] != end[1]) {
  nxt <- setdiff(lookup[[pos[1]]], visited)
  nxt <- nxt[!nxt %in% wall]
  pre[nxt] <- pos[1]
  visited <- c(visited, pos[1])
  pos <- c(pos[-1], nxt)
  k <- k + 1L
}

path_no_wall <- end[1]
while (path_no_wall[1] != start) path_no_wall <- c(pre[path_no_wall[1]], path_no_wall)
#

tmp <- c(-1, 1, n, -n)


compute_cheat_time <- function(w) {
  
  if (all((w + c(-n, n)) %in% gr)) {
   p <- .Internal(which(path_no_wall %in% (w + c(n, -n))))
  } else if (all((w + c(-1, 1)) %in% gr)) {
   p <- .Internal(which(path_no_wall %in% (w + c(1, -1))))
  } else return(0)
  
  abs(p[1] - p[2]) - 2L
  
}

ct <- sapply(wall_in, compute_cheat_time)


sum(ct >= 100)
