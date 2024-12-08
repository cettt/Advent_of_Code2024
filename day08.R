x <- as.character(as.matrix(read.fwf("Input/day08.txt", widths = rep(1, 50))))


int2co <- \(k) (k - 1L) %/% 50L + 1L + ((k - 1L) %% 50L + 1L) * 1i

ant <- function(freq, part2 = FALSE) {
  
  f_co <- int2co(which(x == freq)) 
  a <- combn(f_co, 2L, \(x) x[1] + (x[1] - x[2]) * if (part2) (-49):49 else c(1, -2))
  
  a[Im(a) > 0 & Re(a) > 0 & Im(a) < 51 & Re(a) < 51]
}

# part 1------
length(unique(unlist(lapply(unique(x[x != "."]), ant))))

# part 2----------
length(unique(unlist(lapply(unique(x[x != "."]), ant, part2 = TRUE))))

