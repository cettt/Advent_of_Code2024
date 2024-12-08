data05 <- readLines("Input/day05.txt")

or <- sapply(strsplit(data05[grep("\\|", data05)], "\\|"), as.integer)
upd <- lapply(strsplit(data05[grep(",", data05)], ","), as.integer)


check_order <- function(x) {
  
  or2 <- or[, colSums(matrix(or %in% x, 2)) == 2L] #subset of relevant orders
  x2 <- as.integer(names(sort(-table(or2[1, ])))) # sort x 
  
  x2 <- c(x2, setdiff(x, or2[1,]))
  
  id <- identical(x2, x)
  c(if (!id) 0, x2[(length(x) + 1) / 2], if(id) 0)
  
}

# part 1 and 2--------
rowSums(sapply(upd, check_order))

