data24 <- readLines("Input/day24.txt")

val <- grep("\\d$", data24[cumsum(data24 == "") == 0], value = TRUE)

eq0 <- grep("->", data24, value = TRUE)
eq <- gsub("(.*) AND (.*) (-> .*)", "min(\\1, \\2) \\3", eq0)
eq <- gsub("(.*) XOR (.*) (-> .*)", "(\\1 + \\2) %% 2L \\3", eq)
eq <- gsub("(.*) OR (.*) (-> .*)",  "max(\\1, \\2) \\3", eq)

for (v in val) eq <- gsub(substr(v, 1, 3), substr(v, 6, 6), eq)

check <- grep("\\(\\d.*\\d\\)", eq)

while (length(check) > 0) {
  for (k in check) {
    var <- gsub(".* -> ", "", eq[k])
    prs <- if (!grepl("^z\\d{2}", var)) sub(" ->.*", "", eq[k]) else eq[k]
    evl <- eval(parse(text = prs))
    eq <- gsub(var, evl, eq)
  }
  eq <- eq[-check]
  check <- grep("\\(\\d.*\\d\\)", eq)
}

res <- sapply(ls(pattern = "^z"), get)

sprintf("%.f", sum(res * 2^(seq_along(res) - 1)))


# part 2---------
get_val <- \(x) gsub(".* -> ", "", x)
mypad <- \(n) ifelse(n <= 9L, paste0("0", n), as.character(n))
extract_gate <- \(x) regmatches(x, gregexpr("[a-z]{3}", x))[[1]]

ends_with <- \(x) grep(paste0(x, "$"), eq0, value = TRUE)


## wrong z gates: z always has to come from XOR (except z45)
res_z <- get_val(eq0[!grepl("XOR", eq0) & grepl("z", eq0) & !grepl("z45", eq0)])

## x_ XOR y_ is always mapped to a gate that maps to a z-gate
##   this can be used to find the correct gates
res_z_cor <- character()
for (n in sub("z", "", res_z)) {
  tmp <- eq0[grepl(paste0("x", n), eq0) & grepl(paste0("y", n), eq0) & grepl("XOR", eq0)]
  tmp2 <- eq0[grepl(paste0(get_val(tmp), "."), eq0) & grepl("XOR", eq0)]
  res_z_cor <- c(res_z_cor, get_val(tmp2))
}

 
for (k in seq_along(res_z)) {
  eq0 <- gsub(paste0(res_z[k], "$"), paste0(res_z_cor[k], "999"), eq0)
  eq0 <- gsub(paste0(res_z_cor[k], "$"), res_z[k], eq0)
  eq0 <- gsub("999", "", eq0)
}

res <- vector("list", 43)


for (k in 2:44)  {

  eq_z <- grep(paste0("z", mypad(k)), eq0, value = TRUE)
  eq_z2 <- sapply(extract_gate(eq_z), ends_with)
  eq_z2 <- eq_z2[order(!grepl("x\\d{2}", eq_z2))]
  
  eq_z3 <- sapply(extract_gate(eq_z2[2])[1:2], ends_with)
  eq_z3 <- eq_z3[order(!grepl("x\\d{2}", eq_z3))]
  
  res[[k - 1]] <- c(eq_z, eq_z2, eq_z3)
}


res_xy <- get_val(grep("XOR", sapply(res, \(z) z[2]), value = TRUE, invert = TRUE))
res_xy2 <- get_val(grep("AND", sapply(res, \(z) z[4]), value = TRUE, invert = TRUE))


paste0(sort(c(res_z, res_z_cor, res_xy, res_xy2)), collapse = ",")


# explanation------
# starting with the second bit (z02) each bit is constructed using the following five instructions:
# aaa XOR bbb -> z02
#   y02 XOR x02 -> aaa
#   ccc OR  ddd -> bbb
#     y01 AND x01 -> ccc
#     eee AND fff -> ddd  
#
# and they continue in the same fashion
#
# ggg XOR hhh -> z03
#   y02 XOR x02 -> ggg
#   iii OR  jjj -> hhh
#     y02 AND x02 -> iii
#     aaa AND bbb -> jjj  
#
# note that aaa and bbb appear both  for z02 and z03!
#
# this code checks the instructions for this structure:
#   res_z checks whether instructions which assign something on z contain XOR
#   res_z_cor are the corrected gates
# 
#  the list res then collects all instructions for each individual bit.
#    each component should contain five instructions in the same sequence as above