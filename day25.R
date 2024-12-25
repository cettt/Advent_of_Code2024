data25 <- readLines("Input/day25.txt")

compute_height <- function(x) {
  y <- do.call(rbind, strsplit(x[x != ""], ""))
  (colSums(y == "#") - 1) * if (all(y[1, ] == "#")) -1 else 1
  
}

res <- lapply(split(data25, cumsum(data25 == "")), make_pic)

key <- res[sapply(res, \(z) all(z <= 0))]
locks <- res[sapply(res, \(z) all(z >= 0))]

sum(apply(expand.grid(1:250, 1:250), 1, \(x)  all(-key[[x[1]]] + locks[[x[2]]] <= 5)))

