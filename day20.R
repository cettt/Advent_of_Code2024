data20 <- as.character(do.call(rbind, strsplit(readLines("Input/day20.txt"), "")))
n <- as.integer(sqrt(length(data20)))

wall <- which(data20 == "#")
start <- which(data20 == "S")
end <- which(data20 == "E")

tmp <- c(-1, 1, n, -n)
idx_k <- function(k) k + tmp

lookup <- lapply(seq_len(n^2), idx_k)


#normal path---------

path <- integer(n^2 - length(wall))
path[1] <- start

for(k in 2:length(path)) {
  nxt <- lookup[[path[k - 1]]]
  path[k] <- nxt[(!nxt %in% wall) & nxt != c(path[k - 2], 0)[1]]
  
}

path_co <- apply(arrayInd(path, .dim = c(n, n)), 1, \(x) x[1] * 1i + x[2]) 


compute_cheat <- function(i, max_cheat) {
  p0 <- path_co[i]
  p1 <- path_co[-seq_len(i)]
  l1_d <- abs(Re(p1) - Re(p0)) + abs(Im(p1) - Im(p0))
  
  sum(l1_d <= max_cheat & seq_along(p1) - l1_d >= 100)
  
}

#part 1 ----------
sum(sapply(seq_along(path_co[-1]), compute_cheat, max_cheat = 2L))

# part 2------------
sum(sapply(seq_along(path_co[-1]), compute_cheat, max_cheat = 20L))
