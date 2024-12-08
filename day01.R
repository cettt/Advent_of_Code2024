x <- read.table("Input/day01.txt")

# part 1-----
sum(abs(sort(x[, 2]) - sort(x[, 1])))

# part 2--------
sum(sapply(x[, 1], \(z) z * sum(x[, 2] == z)))
