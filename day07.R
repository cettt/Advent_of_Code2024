data07 <- lapply(strsplit(readLines("Input/day07.txt"), "\\D+"), as.numeric)

f <- function(z, part2 = FALSE) {
  
  res <- Reduce(\(x, y) c(x + y, x * y, if (part2) x * 10^(floor(log10(y) + 1)) + y), z[-1])
  if (any(res == z[1])) z[1] else 0
}

res1 <- sapply(data07, f)

#part 1-------------
sprintf("%.f", sum(res1))

# part 2-------------
sprintf("%.f", sum(sapply(data07[res1 == 0], f, part2 = TRUE)) + sum(res1))

