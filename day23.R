data23 <- as.matrix(read.table("Input/day23.txt", sep = "-"))

count_triple <- function(comp) {
  
  nxt <- as.character(data23[data23[,1] %in% comp | data23[,2] %in% comp, ])
  tab <- split(nxt, nxt)
  n_t <- pmin(3L, length(grep("^t", comp)) + grepl("^t", names(tab)) + 1L)
  sum(ifelse(lengths(tab) == 2L, 1L, 0L) / n_t)
  
}

# part 1-------
t_rows <- data23[grepl("^t", data23[,1]) | grepl("^t", data23[,2]), ]
sum(apply(t_rows, 1, count_triple))


# part 2---------
netw <- rbind(data23, data23[,2:1])
find_cluster <- function(comp) {
  nxt <- netw[netw[,1] == comp, 2L]
  
  nxtnxt <- sapply(nxt, \(x) netw[netw[,1] %in% x, 2L])
  
  res <- sapply(nxt, \(x) sum(colSums(nxtnxt == x)))
  c(comp, names(res[res == max(res)]))
}


to_check <- unique(netw[, 1])
cl_list <- list()

while (length(to_check > 1)) {
  cl_list <- c(list(find_cluster(to_check[1])), cl_list)
  to_check <- setdiff(to_check, cl_list[[1]])
}

paste0(sort(cl_list[[which.max(lengths(cl_list))]]), collapse = ",")

