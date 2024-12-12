n <- 140L
x <- as.character(as.matrix(read.fwf("Input/day12.txt", widths = rep(1, n))))


idx_k <- function(k) {
  k + c(if (k > n) - n, if (k <= n^2 - n) n, if (k %% n != 1L) -1L, if (k %% n != 0L) 1L) 
}

lookup <- sapply(seq_along(x), idx_k)

f <- function(k) {
  plt <- x[k]
  reg <- k
  cur <- k
  per <- 0L
  
  while (TRUE) {
    ngb <- unlist(lookup[cur])
    per <- per +  4 * length(cur) - sum(x[ngb] == plt)
    cur <- setdiff(ngb[x[ngb] == plt], reg)
    if (length(cur) == 0) break
    reg <- c(reg, cur)
  }
  
  check_list <<- setdiff(check_list, reg)

  
  length(reg) * c(per, count_sides(reg))
    
}

count_sides <- function(reg) {
 co <- apply(arrayInd(reg, c(n, n)), 1, \(x) x[1]*1i + x[2])
 
 sides <- 0L
 for (k in 0:n) {
   x1 <- Re(co[Im(co) == k])
   x2 <- Re(co[Im(co) == k + 1L])
   
   x12 <- sort(setdiff(x1, x2))
   x21 <- sort(setdiff(x2, x1))
   
   hside <- (length(x12) > 0) + (length(x21) > 0) + sum(diff(x12) > 1) + sum(diff(x21) > 1)
   
   y1 <- Im(co[Re(co) == k])
   y2 <- Im(co[Re(co) == k + 1L])
   
   y12 <- sort(setdiff(y1, y2))
   y21 <- sort(setdiff(y2, y1))
   
   vside <- (length(y12) > 0) + (length(y21) > 0) + sum(diff(y12) > 1) + sum(diff(y21) > 1)
   
   sides <- sides + hside + vside
 }
 return(sides)
 
}

res <- c(0L, 0L)
check_list <- seq_along(x)
while (length(check_list) > 0) {
  res <- res + f(check_list[1])
}

res
