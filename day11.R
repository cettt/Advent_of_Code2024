tab <- table(as.numeric(read.table("Input/day11.txt")[1,]))

blink <- function(x, tab) {
  
  d <- ceiling(log10(x + 0.1))
  idx <- d %% 2 == 0
  .tmp <- 10^(d[idx] / 2)
  
  x2 <- x
  x2[!idx] <- x2[!idx] * 2024
  x2[x2 == 0] <- 1
  x2[idx] <- x2[idx] %/% .tmp
  
  sapply(split(c(tab, tab[idx]), c(x2, x[idx] %% .tmp)), sum)
  
}

for (i in 1:75) {
  tab <- blink(as.numeric(names(tab)), tab)
  if (i == 25L) part1 <- sum(tab)
}

# part 1------
part1

# part 2-------
sprintf("%.f", sum(tab))

