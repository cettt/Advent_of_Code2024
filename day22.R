x <- read.table("Input/day22.txt")[, 1]

dig <- matrix(x %% 10L, nrow = 2001L, ncol = length(x))
del <- matrix(0L, nrow = 2000, ncol = length(x))
d_seq <- matrix(0L, nrow = 1997, ncol = length(x))

for (k in 1:2000) {
  x <- bitwXor(x, (x * 64L) %% 16777216L) 
  x <- bitwXor(x, x %/% 32L)
  x <- bitwXor(x, (x * 2048) %% 16777216L)
  
  dig[k + 1L, ] <- x %% 10L
  del[k, ] <- dig[k + 1L, ] - dig[k, ] + 9L
  if (k >= 4L) {
    d_seq[k - 3L, ] <- colSums(del[k - 0:3, ] * 19^(3:0))
  }
}

# part 1------------
sum(x)

# part 2-------
n_banana <- integer(19^5)
for (k in seq_along(x)) {
  dup <- !duplicated(d_seq[, k])
  n_banana[d_seq[dup, k] + 1L] <- n_banana[d_seq[dup, k] + 1L] + dig[-(1:4), k][dup]
}

max(n_banana)

