data15 <- readLines("Input/day15.txt")
n <- 50L
dir_vec <- unname(c("^" = -1i, "v" = 1i, ">" = 1, "<" = -1)[unlist(strsplit(data15[-(1:(n + 1))],""))])

x <- as.character(do.call(rbind, strsplit(data15[1:n], "")))
co <- rep(seq_len(n) - 1, each = n) + rep(seq_len(n) - 1, n) * 1i

wall <- co[x == "#"]
box <- co[x == "O"]
pos <- co[x == "@"]


for (dir in dir_vec) {
    new_pos <- pos + dir
    if (!new_pos %in% wall) {
        idx <- if (Im(dir) == 0) Im(box) == Im(pos) else Re(box) == Re(pos)
        box2 <- box[idx]
        if (new_pos %in% box2) {
            n_box <- Position(\(p) !p %in% box2, pos + seq_len(n) * dir)
            if (!(pos + n_box * dir) %in% wall) {
                box[idx][box2 == new_pos] <- pos + n_box * dir # move box to the end 
                pos <- new_pos
            }
        }
        else pos <- new_pos
    }
}

sum(100 * Im(box) + Re(box))

#part 2---------
update_tile <- \(x) gsub("#", "##", gsub("O", "[]", gsub("@", "@.", gsub("\\.", "..", x))))

x <- as.character(do.call(rbind, strsplit(update_tile(data15[1:n]), "")))
co <- rep(seq_len(2 * n) - 1, each = n) + rep((seq_len(n) - 1), 2 * n) * 1i
wall <- co[x == "#"]
box <- rbind(co[x == "["], co[x == "["] + 1)
pos <- co[x == "@"]

for (dir in dir_vec) {
    new_pos <- pos + dir
    if (!new_pos %in% wall) {
        if (Im(dir) == 0) { #left right
            idx <- Im(box) == Im(pos)
            box2 <- matrix(box[idx], nrow = 2)
            if (new_pos %in% box2) {
                n_box <- Position(\(p) !p %in% box2, pos + (seq_len(n) * 2L - 1L) * dir)
                if (!(pos + (n_box * 2L - 1L) * dir) %in% wall) {
                    mbox <- pos + (if (dir == 1) seq_len((n_box - 1) * 2L) else seq.int((n_box - 1) * 2L, 1L))  * dir #boxes to be moved
                    box[idx][box2 %in% mbox] <- mbox + dir
                    pos <- new_pos
                }
            }
            else pos <- new_pos
        } else { # up down 
            if (new_pos %in% box) {
                mbox <- box[, colSums(new_pos == box) == 1]
                n_box <- mbox
                move <- TRUE
                while(TRUE) {
                    if (any((n_box + dir) %in% wall)) {
                        move <- FALSE
                        break
                    }
                    idx2 <- box %in% (n_box + dir) 
                    
                    if (!any(idx2)) break
                    n_box <- box[, colSums(matrix(idx2, 2)) > 0]
                    
                    mbox <- c(mbox, n_box)
                }
                
                if (move) {
                    box[,box[1,] %in% mbox] <- mbox + dir
                    pos <- new_pos
                }
                
            }
            else pos <- new_pos
        }
    }
}

sum(100 * Im(box[1, ]) + Re(box[1, ]))
