data02 <- lapply(strsplit(readLines("Input/day02.txt"), " "), as.integer)

f <- function(x) {
  z <- diff(x)
  (all(z > 0L) && all(z < 4L)) || (all(z > -4L) && all(z < 0L))
}

# part 1----------
sum(sapply(data02, f))

# part 2--------
sum(sapply(data02, \(y) any(sapply(seq_along(y), \(k) f(y[-k])))))
