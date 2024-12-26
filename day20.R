data20 <- as.character(do.call(rbind, strsplit(readLines("Input/day20.txt"), "")))
n <- as.integer(sqrt(length(data20)))

co <- rep(seq_len(n), each = n) + rep.int(seq_len(n), n) * 1i

wall <- co[data20 == "#"]
wall_rw <- split(wall, Im(wall))
wall_cl <- split(wall, Re(wall))

# construct path--------
path <- integer(n^2 - length(wall))
path[1] <- co[data20 == "S"]

dir_vec <- c(-1, 1, 1i, -1i)
dir <- dir_vec[which(!(path[1] + dir_vec) %in% wall)]
wall2 <- if (Re(dir) == 0) wall_cl[[Re(path[1])]] else wall_rw[[Im(path[1])]]

for(k in 2:length(path)) {
  if ((path[k - 1] + dir) %in% wall2) {
    wall2 <- if (Re(dir) == 0L) wall_rw[[Im(path[k - 1L])]] else wall_cl[[Re(path[k - 1L])]]
    dir_idx <- .Internal(which(!(path[k - 1] + dir * c(-1i, 1i)) %in% wall2))
    dir <- dir * c(-1i, 1i)[dir_idx]
  }
  path[k] <- path[k - 1] + dir
}


compute_cheat <- function(i) {
  p0 <- path[i]
  p1 <- path[-seq_len(i + 100L)]
  l1_d <- abs(Re(p1) - Re(p0)) + abs(Im(p1) - Im(p0))
  idx <- l1_d <= 20
  l1_d <- l1_d[idx]
  l1_d <- l1_d[seq_along(p1)[idx] >= l1_d]
  
  c(sum(l1_d <= 2), length(l1_d)) 
}

#part 1 and 2 ----------
rowSums(sapply(seq_along(path[-seq_len(100)]), compute_cheat))

