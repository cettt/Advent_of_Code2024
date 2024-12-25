x <- as.character(do.call(rbind, strsplit(readLines("Input/day06.txt"), "")))
n <- as.integer(sqrt(length(x)))
rw <- rep.int(seq_len(n), n)
cl <- rep(seq_len(n), each = n)

obs <- which(x == "#")
obs_rw <- split(obs, rw[obs])
obs_cl <- split(obs, cl[obs])

dir_vec <- c(-1L, n, 1L, -n)

idx_k <- function(k) {
  .rw <- rw[k]
  .cl <- cl[k]
  
  obs2 <- obs_cl[[.cl]]
  res_ud <-  c(max(obs2[obs2 <= k], -2L) + 1L, min(obs2[obs2 >= k], 1e5L + 1L) - 1L)
  
  obs3 <- obs_rw[[.rw]]
  res_lr <- c(max(obs3[obs3 <= k], -1L - n) + n, min(obs3[obs3 >= k], 1e5L + n) - n)
  
  c(res_ud, res_lr)[c(1L, 4L, 2L, 3L)] # up, right, down, left
}

lookup <- sapply(seq_len(n^2), idx_k)

pos <- which(x == "^")
path <- pos

k <- 1L
while (TRUE) {
  pos <- c(lookup[k, pos[1]], pos)
  if (pos[1] == -1 || pos[1] == 1e5)  break
  path <- c(path, seq(pos[2], pos[1], by = dir_vec[k])[-1])
  k <- k %% 4L + 1L
}

# finish path
pos2 <- arrayInd(pos[2], c(n, n))
lng <- if (k == 1) pos2[1] else if (k == 3) 131 - pos2[1] else if (k == 2) pos2[2] else 131 - pos2[2]
final_path <- seq(pos[2], length.out = lng, by = dir_vec[k])[-1]
path <- c(path, final_path)
length(unique(path))

# part 2-------------

## update lookup-----
update_lookup <- function(k) {
  
  left  <- which(rw == rw[k] & cl > cl[k])
  right <- which(rw == rw[k] & cl < cl[k])
  up    <- which(cl == cl[k] & rw > rw[k]) 
  down  <- which(cl == cl[k] & rw < rw[k]) 
  lookup_new <- lookup
  lookup_new[4, left]  <- pmax.int(lookup[4, left], k + n)
  lookup_new[2, right] <- pmin.int(lookup[2, right], k - n)
  lookup_new[1, up]    <- pmax.int(lookup[1, up], k + 1L)
  lookup_new[3, down]  <- pmin.int(lookup[3, down], k - 1L)
  
  return(lookup_new)  
}


check_loop <- function(pos0, dir0) {
  lookup2 <- update_lookup(pos0 + dir0)
  k <- which(dir_vec == dir0)
  
  while (TRUE) {
    pos0 <- c(lookup2[k, pos0[1]], pos0)
    if (pos0[1] == -1 || pos0[1] == 1e5)  return(0)
    if (sum(pos0 == pos0[1]) > 3L) return(1)
    k <- k %% 4L + 1L
  }
}

unique_pos <- cbind(path[-length(path)], diff(path))
unique_pos <- unique_pos[!duplicated(unique_pos[,1] + unique_pos[,2]), ]


sum(apply(unique_pos, 1, \(x) check_loop(x[1], x[2])))
