n <- 141L
data20 <- as.character(as.matrix(read.fwf("Input/day20.txt", widths = rep(1, n), comment.char = "")))

wall <- which(data20 == "#")
wall_in <- wall[wall > n & wall <= n^2 - n & wall %% n > 1]
gr <- which(data20 != "#")
start <- which(data20 == "S")
end <- which(data20 == "E")

tmp <- c(-1, 1, n, -n)
idx_k <- function(k) k + tmp


lookup <- lapply(seq_len(3 * n^2), idx_k)


#normal path---------

pos <- start
visited <- integer()
pre <- integer(n^2)

while(pos != end[1]) {
  nxt <- lookup[[pos[1]]]
  nxt <- nxt[(!nxt %in% wall) & nxt != pre[pos]]
  pre[nxt] <- pos
  pos <- nxt
}

path_no_wall <- end[1]
while (path_no_wall[1] != start) path_no_wall <- c(pre[path_no_wall[1]], path_no_wall)


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

#co-------
path_co <- apply(arrayInd(path_no_wall, .dim = c(n, n)), 1, \(x) x[1] * 1i + x[2]) 


count_dist <- function(i) {
  p0 <- path_co[i]
  p1 <- path_co[-seq_len(i)]
  
  d <- p1[abs(Re(p1) - Re(p0)) + abs(Im(p1) - Im(p0)) == 2L]
  
  sum(which(path_co %in% d) > i + 2L + 99) 
   
}


sum(sapply(seq_along(path_co[-1]), count_dist))


count_dist2 <- function(i) {
  p0 <- path_co[i]
  p1 <- path_co[-seq_len(i)]
  l1_d <- abs(Re(p1) - Re(p0)) + abs(Im(p1) - Im(p0))
  idx <- l1_d > 1L & l1_d <= 20L
  p1 <- p1[idx]
  l1_d <- l1_d[idx]
  
  idx2 <- path_co %in% p1
  pos_p1 <- sapply(p1, \(x) which(path_co == x))
  
  sum(pos_p1 > i + l1_d + 99) 
  
}

sum(sapply(seq_along(path_co[-1]), count_dist2))
