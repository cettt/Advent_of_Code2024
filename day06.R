n <- 130
x <- read.fwf("Input/day06.txt", widths = rep(1, n), comment.char = "")

co <- apply(which(x != "#", arr.ind = TRUE), 1, \(z) z[1] *1i + z[2]) 

pos_vec <- sum(which(x == "^", arr.ind = TRUE)[1, ] * c(1i, 1))
dir <- -1i

while (TRUE) {
  
  co2 <- if (Re(dir) == 0) co[Re(co) == Re(pos)] else co[Im(co) == Im(pos)]
  pos <- pos_vec[length(pos_vec)]
  n_steps <- Position(\(k)! (pos + k*dir) %in% co2, 1:130) - 1L
  pos_vec <- c(pos_vec, pos + dir * seq_len(n_steps))
  pos <- pos + n_steps * dir
  pos2 <- pos + dir
  
  
  if (Re(pos2) < 1 || Re(pos2) > n || Im(pos2) < 1 || Im(pos2) > n) {
    break
  } else {
    dir <- dir * 1i
  }
}

length(unique(pos_vec))

f <- function(z0) {
  co <- co[co != z0]
  pos <- pos_vec[1]
  pos_vec <- pos
  dir <- -1i
  co2 <- co[Re(co) == Re(pos_pec)]
  turn_vec <- complex()
  dir_vec <- complex()
  check_loop <- FALSE
  
  while (TRUE) {
    
    n_steps <- Position(\(k)! (pos + k*dir) %in% co2, 1:n) - 1L
    pos_vec <- c(pos_vec, pos + dir * seq_len(n_steps))
    pos <- pos + n_steps * dir
    pos2 <- pos + dir
    if (pos2 == z0) check_loop <- TRUE
    
    if (Re(pos2) < 1 || Re(pos2) > n || Im(pos2) < 1 || Im(pos2) > n) {
      break
    } else {
      if (check_loop)  {
        if(length(turn_vec) > 4 && pos %in% turn_vec) {
          if (any(dir_vec[pos == turn_vec] == dir)) return(TRUE)
        } 
        turn_vec <- c(turn_vec, pos)
        dir_vec <- c(dir_vec, dir)
      }
      dir <- dir * 1i
      co2 <- if (Re(dir) == 0) co[Re(co) == Re(pos)] else co[Im(co) == Im(pos)]
    }
  }
  
  return(FALSE)
}

sum(sapply(unique(pos_vec)[-1], f))




go <- function(pos, dir, co, part1 = TRUE) {
  pos_vec <- pos 
  dir_vec <- dir
  loop_count <<- 0L
  
  while (TRUE) {
    
    co2 <- if (Re(dir) == 0) co[Re(co) == Re(pos)] else co[Im(co) == Im(pos)]
    n_steps <- Position(\(k)! (pos + k*dir) %in% co2, 1:130) - 1L
   
    new_pos <- pos + dir * seq_len(n_steps)
    
    if (!part1) {
      s <- sapply(new_pos[-n_steps], \(p) go(p, dir * 1i, co[co != p + dir]))
    }
   
    pos_vec <- c(pos_vec, new_pos)
    dir_vec <- c(dir_vec, rep(dir, n_steps))
    pos <- pos + n_steps * dir
    pos2 <- pos + dir
    dir <- dir * 1i 
    
    if (!part1 && length(pos_vec[dir_vec == dir & pos_vec == pos + dir]) > 0L) {
        loop_count <<- loop_count + 1L
        return(NULL)
    }

    if (Re(pos2) < 1 || Re(pos2) > n || Im(pos2) < 1 || Im(pos2) > n) break
  }
  
  if (part1) return(cbind(pos_vec, dir_vec)) else return(NULL)
  
}

res1 <- go(sum(which(x == "^", arr.ind = TRUE)[1, ] * c(1i, 1)), -1i, co, F)


length(unique(res1[,1]))

head(cbind(res1, pos_vec), 20)
