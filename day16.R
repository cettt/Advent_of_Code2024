x <- as.character(do.call(rbind, strsplit(readLines("Input/day16.txt"), "")))
n <- as.integer(sqrt(length(x)))

gr <- seq_len(2 * n^2)[c(x, x) != "#"]
pos <- seq_along(x)[x == "S"]
end <- seq_along(x)[x == "E"] + c(0L, n^2)

tmp1 <- c(-n, n, c(1L, -1L) + n*n)
tmp2 <- c(c(-n , n) - n*n, 1L,  -1L)

find_ne <- function(k) k + if (k <= n^2) tmp1 else tmp2 

q <- collections::priority_queue(pos, priorities = 0L)
score <- rep.int(1e9L, 2L*n^2) 
pre <- vector("list", 2L * n^2)
score[pos] <- 0L

while (TRUE) {
    cur <- q$pop()
    if (cur %in% gr) {
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
}
# part 1------------
min(score[end])

# part 2----------
path <- list(end[which.min(score[end])])

while (any(path[[1]] != pos)) path <- c(list(unlist(pre[path[[1]]])), path)

length(unique(unlist(path) %% n^2))


# explanation------- 
# the graph gr is represented as an integer vector seq_len(2 * n^2) where n = 141
# where the first half on entries correspond to a east-west orientation 
#   and the second half to north south orientation

# the function find_ne finds adjacent edges to an edge k:
#   note this is just a fast search and some edges might actually be wall tiles:
#    wall tiles are skipped during Dijkstra

# priority queues are used to find the shortest paths from S to E:
#   however the algorithm continues a little while longer until all tiles with a maximum score
#    of the part 1 target score are reached
# Furthermore, for each tile, its predecessors (potentially more than 1!) are stored:
#   these are then used to reconstruct all optimal paths.