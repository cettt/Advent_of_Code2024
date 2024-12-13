x <- sapply(strsplit(readLines("Input/day14.txt"), "[, =]"), \(x) as.integer(x[c(-1, -4)]))

res <- cbind((x[1,] + x[3,] * 100L) %% 101L, (x[2,] + x[4,] * 100L) %% 103L)
q <- split(seq_along(x[1,]), (sign(res[,1] - 50)) + (sign(res[,2] - 51)) * 3)

prod(lengths(q[c("-4", "-2", "2", "4")]))


# part 2---------------
sd_vec <- sapply(1:103, \(k) c(sd((x[1,] + x[3,] * k) %% 101L), sd(((x[2,] + x[4,] * k) %% 103L))))
x0 <- apply(sd_vec, 1, which.min)

k <- which(((x0[1] + 1:100 * 101L) %% 103L) == x0[2])*101L + x0[1]


plot(cbind((x[1,] + x[3,] * k) %% 101L, -(x[2,] + x[4,] * k) %% 103L), pch = 15)

