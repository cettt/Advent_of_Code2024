m <- matrix("", 146, 146)
m[4:143, 4:143] <- as.matrix(read.fwf("Input/day04.txt", widths = rep(1, 140)))

vec <- list(
  ud = 0:3, du = -(0:3), lr = 0:3 * 146L,  rl = -(0:3) * 146L, ne = 0:3 * 146L - 0:3, 
  se = 0:3 * 146L + 0:3, nw = -(0:3) * 146L - 0:3, sw = -(0:3) * 146L + 0:3
)

find_xmas <- function(k) sum(sapply(vec, \(a) paste(m[a + k], collapse = "") == "XMAS"))
sum(sapply(which(m == "X"), find_xmas)) 


# part 2--------
find_x_mas <- function(k) {
  m2 <- m[k + c(-145L, -147L, 145L, 147L)]
  (sum(m2 == "S") == 2L) && (sum(m2 == "M") == 2L) && (m2[1] != m2[3])
}

sum(sapply(which(m == "A"), find_x_mas))

