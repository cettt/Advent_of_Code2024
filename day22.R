data22 <- read.table("Input/day22.txt")[, 1]
pow2 <- as.integer(2^(0:23))
pow19 <- as.integer(19^(3:0))

xbit <- sapply(data22, \(x) as.integer(intToBits(x))[1:24])

dig <- matrix(0L, nrow = 2001L, ncol = length(data22))
dig[1, ] <- data22 %% 10L
del <- matrix(0, nrow = 2000, ncol = length(data22))
d_seq <- matrix(0, nrow = 1997, ncol = length(data22))

for (k in 1:2000) {
  xbit[7:24 , ] <- xbit[7:24 , ] + xbit[1:18, ]
  xbit[1:19 , ] <- xbit[1:19 , ] + xbit[6:24, ]
  xbit[12:24, ] <- xbit[12:24, ] + xbit[1:13, ]
  xbit <- xbit %% 2L
  
  dig[k + 1L, ] <- as.integer(colSums((xbit * pow2)) %% 10L)
  del[k, ] <- dig[k + 1L, ] - dig[k, ] + 9L
  if (k >= 4) {
    d_seq[k - 3L, ] <- colSums(del[k - 0:3, ] * pow19)
  }
}

# part 1------------
sum(colSums(xbit * pow2))

# part 2-------
dig <- dig[-(1:4), ]
n_banana <- integer(19^5)
for (k in seq_along(data22)) {
  dup <- !duplicated(d_seq[, k])
  n_banana[d_seq[dup, k] + 1L] <- n_banana[d_seq[dup, k] + 1L] + dig[dup, k]
}

max(n_banana)
