data22 <- read.table("Input/day22.txt")[, 1]
pow2 <- as.integer(2^(0:23))

xbit <- sapply(data22, \(x) as.integer(intToBits(x))[1:24])

dig <- matrix(0L, nrow = 2001L, ncol = length(data22))
dig[1, ] <- data22 %% 10L

for (k in 2:2001) {
  xbit[7:24 , ] <- (xbit[7:24 , ] + xbit[1:18, ]) 
  xbit[1:19 , ] <- (xbit[1:19 , ] + xbit[6:24, ])  
  xbit[12:24, ] <- (xbit[12:24, ] + xbit[1:13, ])
  xbit <- xbit %% 2L
  
  dig[k, ] <- as.integer(colSums((xbit * pow2)) %% 10L)
}


# part 1------------
sum(colSums(xbit * pow2))


# part 2-------

del <- dig[-1, ] - dig[-2001, ] + 9L
change <- matrix(0L, length(data22), 1997L)
pow19 <- as.integer(19^(3:0))

for (k in 1:1997) change[, k] <- as.integer(colSums(del[0:3 + k, ] * pow19))

tab <- sort(table(as.integer(change)), decreasing = TRUE)[1:250]

uc <- as.integer(names(tab))

n_banana <- 0L
for (y in uc) {
  rw <- apply(change, 1, \(x) .Internal(which(x == y))[1])
  n <- sum(diag(dig[rw + 4L, ]), na.rm = TRUE)
  if (n > n_banana) print(y)
  
  n_banana <- max(n_banana, n)
}

n_banana
