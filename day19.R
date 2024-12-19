data19 <- readLines("Input/day19.txt")

twl <- strsplit(data19[1], ", ")[[1]]
pat_vec <- data19[-(1:2)]
sing <- c("w", "b", "u", "r", "g")

# missing single stripe
x1 <- setdiff(sing, twl) #missing single color: here w

# missing combinations of length two with single stripe: here rw 
x2 <- setdiff(c(paste0(sing, x1), paste0(x1, sing)), twl)

# the only pattern of length two which is impossible is rw
#  so whenever a pattern does not end in rw there is a possible combination


if (grepl(paste0(x1, "$"), x2)) {
  twl2 <- paste0(twl, "$")
  x3 <- paste0(x2, "$")
} else {
  twl2 <- paste0("^", twl)
  x3 <- paste0("^", x2)
}

check_pat <- function(pat) {
  if (nchar(pat) == 0) return(TRUE)
  if (!grepl(x3, pat)) return(TRUE)
  
  idx <- str_detect(pat, twl2)
  if (!any(idx)) return(FALSE)
  
  any(sapply(str_replace(pat, twl2[idx], ""), check_pat))
}


pos_idx <- sapply(pat_vec, check_pat)
sum(pos_idx)


# part 2----------
mem <- integer()

count_pat <- function(pat) {
  if (nchar(pat) == 0) return(1)
  if (pat %in% names(mem)) return(mem[pat])
  
  idx <- str_detect(pat, twl2)
  if (!any(idx)) return(0)
  
  res <- sum(sapply(str_replace(pat, twl2[idx], ""), count_pat))
  mem <<- c(mem, setNames(res, pat))
  return(res)
}

invisible(sapply(twl, count_pat)) # create simple combinations

sprintf("%.f", sum(sapply(pat_vec[pos_idx], count_pat)))

