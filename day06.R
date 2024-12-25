data06 <- as.character(do.call(rbind, strsplit(readLines("Input/day06.txt"), "")))
n <- as.integer(sqrt(length(data06)))
rw <- rep.int(seq_len(n), n)
cl <- rep(seq_len(n), each = n)

obs <- which(data06 == "#")
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

pos <- which(data06 == "^")
path <- pos

dir_counter <- 1L
while (TRUE) {
  pos <- c(lookup[dir_counter, pos[1]], pos)
  if (pos[1] == -1 || pos[1] == 1e5)  break
  path <- c(path, seq(pos[2], pos[1], by = dir_vec[dir_counter])[-1])
  dir_counter <- dir_counter %% 4L + 1L
}

# finish path
rwcl <- if (abs(dir_vec[dir_counter]) == 1) rw[pos[2]] else cl[pos[2]]
lng <- if (dir_vec[dir_counter] > 0) 131L - rwcl else rwcl - 1L
final_path <- seq(pos[2], length.out = lng, by = dir_vec[dir_counter])[-1]
path <- c(path, final_path)
length(unique(path))

# part 2-------------

check_loop <- function(pos0, dir0) {
  
  new_obs <- pos0 + dir0
  rw_new <- rw[new_obs] 
  cl_new <- cl[new_obs] 
  
  left  <- .Internal(which(rw == rw_new & cl > cl_new))
  right <- .Internal(which(rw == rw_new & cl < cl_new))
  up    <- .Internal(which(cl == cl_new & rw > rw_new)) 
  down  <- .Internal(which(cl == cl_new & rw < rw_new)) 
  lookup[1, up]    <- pmax.int(lookup[1, up], new_obs + 1L)
  lookup[2, right] <- pmin.int(lookup[2, right], new_obs - n)
  lookup[3, down]  <- pmin.int(lookup[3, down], new_obs - 1L)
  lookup[4, left]  <- pmax.int(lookup[4, left], new_obs + n)
  
  dir_counter <- which(dir_vec == dir0) %% 4L + 1L
  
  while (TRUE) {
    pos0 <- c(lookup[dir_counter, pos0[1]], pos0)
    if (pos0[1] == -1 || pos0[1] == 1e5)  return(0L)
    if (sum(pos0 == pos0[1]) > 3L) return(1L)
    dir_counter <- dir_counter %% 4L + 1L
  }
}

unique_pos <- cbind(path[-length(path)], diff(path))
unique_pos <- unique_pos[!duplicated(unique_pos[,1] + unique_pos[,2]), ]


sum(apply(unique_pos, 1, \(x) check_loop(x[1], x[2])))
