data13 <- readLines("Input/day13.txt")


a <- as.integer(gsub(".*X[\\+=](\\d+).*", "\\1", data13)[seq_along(data13) %% 4L != 0])
b <- as.integer(gsub(".*Y[\\+=](\\d+).*", "\\1", data13)[seq_along(data13) %% 4L != 0])

res <- 0
for (k in 1:(length(a) / 3)) {
  
  x1 <- (a[k * 3 - 2]* b[k * 3] - a[k*3]* b[k * 3 - 2])  / (a[k * 3 - 2]* b[k * 3 - 1] - b[k * 3 - 2]* a[k * 3 - 1])
  x2 <- (a[k*3]* b[k * 3 - 1] - b[k * 3]* a[k * 3 - 1])  / (a[k * 3 - 2]* b[k * 3 - 1] - b[k * 3 - 2]* a[k * 3 - 1])
  
  if ((trunc(x1) == x1) && (trunc(x2) == x2)) res <- res + x1 + x2 * 3
}
res

#part 2------
a[seq_along(a) %% 3 == 0] <- a[seq_along(a) %% 3 == 0] + 1e13
b[seq_along(a) %% 3 == 0] <- b[seq_along(a) %% 3 == 0] + 1e13

res <- 0
for (k in 1:(length(a) / 3)) {
  
  x1 <- (a[k * 3 - 2]* b[k * 3] - a[k*3]* b[k * 3 - 2])  / (a[k * 3 - 2]* b[k * 3 - 1] - b[k * 3 - 2]* a[k * 3 - 1])
  x2 <- (a[k*3]* b[k * 3 - 1] - b[k * 3]* a[k * 3 - 1])  / (a[k * 3 - 2]* b[k * 3 - 1] - b[k * 3 - 2]* a[k * 3 - 1])
  
  if ((floor(x1) == x1) && (floor(x2) == x2)) res <- res + x1 + x2 * 3
}


sprintf("%.f", res)
