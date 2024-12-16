x <- as.character(do.call(rbind, strsplit(readLines("Input/day16.txt"), "")))
n <- as.integer(sqrt(length(x)))

co <- seq_len(2 * n^2)[c(x, x) != "#"]
pos <- seq_along(x)[x == "S"]
end <- seq_along(x)[x == "E"] + c(0L, n^2)

tmp1 <- c(-n, n, c(1L, -1L) + n*n)
tmp2 <- c(c(-n , n) - n*n, 1L,  -1L)

find_ne <- function(k) {
  
  res <- k + if (k <= n^2) tmp1 else tmp2 
  res[res %in% co]
}

q <- collections::priority_queue(pos, priorities = 0L)
score <- rep.int(1e9L, 2L*n^2) 
pre <- vector("list", 2L * n^2)
score[pos] <- 0L

while (q$size() > 0) {
    cur <- q$pop()
    cur_dist <- score[cur]
    if (cur_dist >= min(score[end])) break
    for (ne in find_ne(cur)) {
      ns <- cur_dist + ifelse(abs(ne - cur) > 2L * n, 1001L, 1L)
      if (score[ne] >= ns) {
        pre[[ne]] <- c(cur, pre[[ne]])
        if (score[ne] > ns) {
          score[ne] <- ns
          q$push(ne, priority = -ns)
        }
      }
    }
}

# part 1------------
min(score[end])

# part 2----------
path <- list(end[which.min(score[end])])

while (any(path[[1]] != pos)) path <- c(list(unlist(pre[path[[1]]])), path)

length(unique(unlist(path) %% n^2))
