x  <- as.integer(strsplit(readLines("Input/day09.txt"), "")[[1]])
idx <- cumsum(c(0L, x))
disk <- rep(-1L, sum(x))


for (k in seq_len((length(x) + 1L) / 2)) {
  disk[(idx[2L * k - 1L] + 1L):(idx[2L * k])] <- k - 1L
}

n <- min(sum(disk == -1L), sum(disk != -1L))

idx_file <- rev(which(disk != -1L))[1:n]
idx_free <- which(disk == -1L)[1:n]

disk2 <- disk

 
disk2[idx_free] <- ifelse(idx_free < idx_file, disk[idx_file], -1L)
disk2[idx_file] <- ifelse(idx_free < idx_file, -1L, disk[idx_file])
 

sprintf("%.f", sum(pmax(0L, disk2) * (seq_along(disk) - 1L)))


# part2-------
idx_list <- lapply(seq_along(idx[-1]), \(k) idx[k] + seq_len(x[k]))
file_pos <- idx_list[seq_along(x) %% 2L == 1L]
free_pos <- idx_list[seq_along(x) %% 2L == 0L]
lng_vec <- lengths(free_pos)

for (k in length(file_pos):2) {
  m <- length(file_pos[[k]])
  if (k <= length(file_pos) / 2 && all(lng_vec[seq_len(k - 1L)] == 0)) break
  i <- .Internal(which(lng_vec[seq_len(k - 1L)] >= m))[1]
  if (!is.na(i)) {
    frp <- free_pos[[i]]
    file_pos[[k]] <- frp[seq_len(m)]
    free_pos[[i]] <- frp[-seq_len(m)] 
    lng_vec[i] <- lng_vec[i] - m
  }
}

sprintf("%.f", sum(sapply(seq_along(file_pos), \(k) sum((file_pos[[k]] - 1) * (k - 1)))))
