n <- 140L
x <- as.character(as.matrix(read.fwf("Input/day12.txt", widths = rep(1, n))))


idx_k <- function(k) {
  ngb <- k + c(if (k > n) - n, if (k <= n^2 - n) n, if (k %% n != 1L) -1L, if (k %% n != 0L) 1L) 
  ngb[x[ngb] == x[k]]
}

lookup <- sapply(seq_along(x), idx_k)

find_region <- function(cur) {
  reg <- cur
  
  while (length(cur)) {
    ngb <- unique(unlist(lookup[cur]))
    cur <- ngb[match(ngb, reg, 0) == 0]
    reg <- c(reg, cur)
  }
  
  return(sort(reg))
  
}

compute_price <- function(reg) {
 
  rw <- (reg - 1L) %%  n + 1L
  cl <- (reg - 1L) %/% n + 1L
  
  sides <- 0L
  for (k in seq.int(min(rw) - 1L, max(rw) + 1L)) {
    x1 <- cl[rw == k]
    x2 <- cl[rw == k + 1L]
    
    x12 <- setdiff(x1, x2)
    x21 <- setdiff(x2, x1)
    
    hside <- (length(x12) > 0) + (length(x21) > 0) + sum(diff(x12) > 1) + sum(diff(x21) > 1)
    
    sides <- sides + hside * 2L
  }
  
  length(reg) * c(4L * length(reg) - length(unlist(lookup[reg])), sides)
  
}

res <- c(0L, 0L)
check <- rep(TRUE, length(x))
while (any(check)) {
  reg <- find_region(which(check)[1])
  check[reg] <- FALSE
  res <- res + compute_price(reg)
}

res
