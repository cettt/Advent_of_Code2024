pad1 <- setNames(c(1:3 + 1i, 1:3 + 2i, 1:3 + 3i, 2:3 + 4i), c(7:9, 4:6, 1:3, 0, "A"))
pad2 <- setNames(c(2:3 + 1i, 1:3 + 2i), c("^", "A", "<", "v", ">"))

data21 <- readLines("Input/day21.txt")

push1 <- function(code, pos0 = "A") {
  
  
  if (any(grepl("\\d", code))) {
    pad <- pad1
    gap <- 1 + 4i
  } else {
    pad <- pad2
    gap <- 1 + 1i
  }
  
  mv <- c("v" = 1i, "^" = -1i, "<" = - 1, ">" = 1)
  
  if (length(code) == 1) code <- strsplit(code, "")[[1]]
  
  vec <- pad[code]
  pos <- pad[pos0]
  
  res <- vector("list", length(code))
  
  for (k in seq_along(code)) {
    y <- Im(pos) - Im(vec[k])
    x <- Re(pos) - Re(vec[k])
    
    tmp <- c(rep(if (y > 0) "^" else "v", abs(y)), rep(if (x > 0) "<" else ">", abs(x)))
    tmp2 <- unique(list(tmp, rev(tmp)))
    tmp2 <- tmp2[sapply(tmp2, \(a) all(pos + cumsum(mv[a]) != gap))]
    res[[k]] <- sapply(tmp2, paste, collapse = "")  
    
    pos <- vec[k]
  }
  
  paste0(Reduce(\(x, y) as.character(outer(x, y, \(x, y) paste(x, y, sep = "A"))), res), "A")
  
}


code_seq <- function(code) {
  code1 <- push1(code)
  
  code2 <- unlist(lapply(code1, push1))
  code3 <- unlist(lapply(code2, push1))
  
  min(nchar(code3)) * as.numeric(gsub("\\D", "", code))
}


sum(sapply(data21, code_seq))

#part 2-----------

f <- function(a1, a2, a3, a4) {
  
  optimize_push <- function(y, x) {
    res1 <- push1(y, x)
    if (length(res1) == 1) return(res1)
    if (y == "A" & x == "v") return(c("^>A", ">^A")[a1])
    if (y == "v" & x == "A") return(c("v<A", "<vA")[a2])
    
    if (y == "^" & x == ">") return(c("<^A", "^<A")[a3])
    if (y == ">" & x == "^") return(c("v>A", ">vA")[a4])
    
    nc <- sapply(sub("A", "", res1), \(a) nchar(push1(a))[1])
    res1[which.min(nc)]
    
  }
  
  map <- unlist(lapply(names(pad2), \(x) sapply(names(pad2), \(y) optimize_push(y, x))))
  names(map) <- paste0(rep(names(pad2), each = 5), names(map))
  
  
  map_mat <- t(sapply(names(map), \(x) as.integer(grepl(x, paste0("A", map), fixed = TRUE))))
  colnames(map_mat) <- rownames(map_mat)
  
  code_seq3 <- function(code, n) {
    code1 <- paste0("A", push1(code))
    code1 <- strsplit(code1, "")
    
    
    res <- sapply(code1, function(cd) {
      y <- paste0(cd[-length(cd)], cd[-1])
      x <- integer(25L)
      names(x) <- colnames(map_mat)
      x[names(table(y))] <- table(y)
      x <- x[rownames(map_mat)]
      sum(Reduce(`%*%`, rep(list(map_mat), n)) %*% x)
    })
    
    min(res) * as.numeric(gsub("\\D", "", code))
    
  }
  sum(sapply(data21, code_seq3, n = 25))
}

eg <- expand.grid(a1 = 1:2, a2 = 1:2, a3 = 1:2, a4 = 1:2)

sprintf("%.f", min(apply(eg, 1, \(x) f(x[1], x[2], x[3], x[4]))))

