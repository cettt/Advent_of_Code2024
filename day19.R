data19 <- readLines("Input/day19.txt")

twl <- strsplit(data19[1], ", ")[[1]]
pat_vec <- data19[-(1:2)]

twl_list <- split(twl, nchar(twl))
nc <- sort(unique(nchar(twl)))

count_pat <- function(pat) {
  
  res <- numeric(nchar(pat) + 1L)
  res[1] <- 1
  
  for (j in seq_len(nchar(pat))) {
    twl_list2 <- twl_list[nc <= j]
    for (i in seq_along(twl_list2)) {
      if (any(substr(pat, j + 1L - nc[i], j) == twl_list2[[i]])) {
        res[j + 1L] <- res[j + 1L] + res[j + 1 - nc[i]]
      }
    }
  }
  res[length(res)]
  
}

res <- sapply(pat_vec, count_pat)

# part 1-----------------
sum(res > 0)

# part 2-----------------
sprintf("%.f", sum(res))


# explanation----------
# in order to count the number of patterns we use recursion:
#  res[j] contains the number of valid patterns for the substring containing the first j - 1 letters
#    by default res[1] is the empty string and there is only one combination to obtain it
# To illustrate the recursion consider the string ABCDE
#  -  First we check if any pattern matches the first letter: if yes, we set res[2] <- res[2] + res[1] = 1
#    Otherwise res[2] stays at zero.
#  - Next we check the first two letters AB
#     if B is matched then res[3] <- res[3] + res[2]
#     if AB is matched as well, we set res[3] <- res[3] + res[1]