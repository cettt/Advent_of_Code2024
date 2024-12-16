n <- 141L

x <- as.character(as.matrix(read.fwf("Input/day16.txt", widths = rep(1, n), comment.char = "")))

#1.n^2 E-W
# n^2 + 1: 2 * n^2 NS
co <- seq_len(2 * n^2)[c(x, x) != "#"]
pos <- seq_along(x)[x == "S"]
end <- seq_along(x)[x == "E"] + c(0, n^2)


k_idx <- function(k) {
  if (! k %in% co) return(NULL)
  if (k <= n^2) {
    res <- k + c(if (k > n) - n, if (k <= n^2 - n) n , if (k %% n != 1) 1L + n^2, if (k %% n != 0) -1L + n^2)
  } else {
    k2 <- k - n^2
    res <- k + c(if (k2 > n) - n - n^2, if (k2 <= n^2 - n) n - n^2 , if (k %% n != 1) 1L, if (k %% n != 0) -1L)
  }
  
    res <- res[res %in% co]
}

lookup <- lapply(seq_len(2 * n^2), k_idx)

q <- collections::priority_queue(pos, priorities = 0L)
dist <- rep.int(2L^30, 2*n^2)  #
dist[pos] <- 0L

while (q$size() > 0) {
    cur <- q$pop()
    cur_dist <- dist[cur]
    for (ne in lookup[[cur]]) {
      nd <- cur_dist + ifelse(abs(ne - cur) > 2L * n, 1001L, 1L)
      if (dist[ne] > nd) {
        dist[ne] <- nd
        q$push(ne, priority = -nd)
      }
    }
    if (any(lookup[[cur]] %in% end)) break
  }
}


min(dist[end])

