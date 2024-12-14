x <- sapply(strsplit(readLines("Input/day14.txt"), "[, =]"), \(x) as.integer(x[c(-1, -4)]))

res <- cbind((x[1,] + x[3,] * 100L) %% 101L, (x[2,] + x[4,] * 100L) %% 103L)

q <- ifelse(res[,1] < 50 & res[,2] < 51, 1, ifelse(
  res[,1] < 50 & res[,2] > 51, 2, ifelse(
    res[,1] > 50 & res[,2] < 51, 3, ifelse(
      res[,1] > 50 & res[,2] > 51, 4, 0
    )
  )  
)
)

prod(table(q[q!=0]))


# part 2---------------
y <- x

# for (k in 1:10403) {
# 
#   y[1:2, ] <- rbind((y[1,] + y[3,] * 1L) %% 101L, (y[2,] + y[4,] * 1L) %% 103L)
#   if (sum(sapply(split(y[1, ], y[2, ]), \(a) sum(diff(sort(a)) == 1)) > 20) > 2) print(k)
#   
#   if(sum(diff(lengths(split(y[1, ], y[2,]))) > 0) >= 40) print(k)
# }

f <- function(k) {
  y <- cbind((x[1,] + x[3,] * k) %% 101L, -((x[2,] + x[4,] * k) %% 103L))
  plot(y, pch = 15, cex = 2, xlim = c(-3, 103), ylim = c(-103, 3),  main = k)
  }

#manual search :/
pdf("day14.pdf")
sapply(7000:8000, f)
dev.off()
