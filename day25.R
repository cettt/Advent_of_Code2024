data25 <- readLines("Input/day25.txt")

compute_height <- function(x) {
  y <- do.call(rbind, strsplit(x[x != ""], ""))
  (colSums(y == "#") - 1) * if (all(y[1, ] == "#")) -1 else 1
}

res <- sapply(split(data25, cumsum(data25 == "")), compute_height)

key <- -res[, colSums(res) < 0]
lock <- res[, colSums(res) > 0]

sum(apply(key, 2, \(x) sum(colSums(x + lock <= 5) == 5)))
