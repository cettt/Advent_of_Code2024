x <- readLines("Input/day03.txt")

inst <- c("do()", unlist(regmatches(x, gregexpr("mul\\(\\d+,\\d+\\)|do\\(\\)|don't\\(\\)", x))))
dig <- sapply(regmatches(inst, gregexpr("\\d+", inst)), \(x) prod(as.integer(x)))

# part 1---------
sum(dig[grepl("mul", inst)]) 

# part 2------
do_vec <- inst[grep("d", inst)][cumsum(grepl("d", inst))]
sum(dig[grepl("m", inst) & do_vec == "do()"])
