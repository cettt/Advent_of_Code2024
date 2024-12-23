data23 <- as.matrix(read.table("Input/day23.txt", sep = "-"))


check3 <- function(k) {
  x <- data23[k,]
  
  y <- as.character(data23[data23[,1] %in% x | data23[,2] %in% x, ])
  tab <- table(y)
  if (any(tab == 2)) {
    network <- lapply(names(tab[tab == 2]), \(z) c(x, z))
    sum(sapply(network, \(z) any(grepl("^t", z))))
  } else return(0L)
}


sum(sapply(seq_along(data23[,1]), check3)) / 3


# part 2---------
check_new <- function(cmp) {
  nxt <- sort(unique(as.character(data23[data23[,1] == cmp | data23[,2] == cmp, ])))
  
  nxtnxt <- lapply(nxt, \(x) sort(unique(as.character(data23[data23[,1] == x | data23[,2] == x, ]))))
  
  tab <- table(unlist(nxtnxt))
  tab <- tab[tab >= max(tab[names(tab) != cmp])]
  
  nxtnxt2 <- lapply(nxtnxt, \(x) x[x %in% names(tab)])
  Reduce(intersect, nxtnxt2[lengths(nxtnxt2) > 1L])
}


uc <- unique(as.character(data23))
res <- lapply(uc, check_new)

paste0(sort(res[lengths(res) == max(lengths(res))][[1]]), collapse = ",")
