data13 <- sapply(strsplit(readLines("Input/day13.txt"), "\\D+"), \(x) as.integer(x[-1]))
x <- tapply(data13, cumsum(lengths(data13) == 0), \(y) do.call(c, y))


slv <- function(a, addon = 0) {
  a[5:6] <- a[5:6] + addon
  
  res <- c(a[5] * a[4] - a[3] * a[6], a[1] * a[6] - a[5] * a[2]) / (a[1] * a[4] - a[2] * a[3])
  
  if (identical(res, floor(res))) sum(res * c(3, 1)) else 0
}

# part 1---------
sum(sapply(x, slv))

# part 2---------
sprintf("%.f", sum(sapply(x, slv, addon = 1e13)))

