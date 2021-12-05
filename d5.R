
# Data prep ----
input = readLines(here::here("2021", "d5_input.txt"))

indices = sub("->", "", input, fixed = TRUE)
indices = strsplit(indices, "\\s+") 
indices = lapply(indices, strsplit, ",", fixed = TRUE)
indices = rapply(indices, as.integer, how = "replace")

# Function to create matrix w/ indices from input coordinates
make_idx_m = function(input_vecs) {
    cbind(do.call(`:`, lapply(input_vecs, `[`, 1L)), #recycling time 
          do.call(`:`, lapply(input_vecs, `[`, 2L)))
}

# Part 1 ----
big_m = matrix(0L, nrow = 1e3, ncol = 1e3) # (numbers range from ~1-999)

for(i in seq_along(indices)) {
    is_diagonal = all(do.call(`!=`, indices[[i]]))

    if(!is_diagonal) {
        idx_m = make_idx_m(indices[[i]])
        big_m[idx_m] = big_m[idx_m] + 1L   
    }
}
sum(big_m >= 2)

# Part 2 ----
big_m = matrix(0L, nrow = 1e3, ncol = 1e3)

for(i in seq_along(indices)) {
    idx_m = make_idx_m(indices[[i]])
    big_m[idx_m] = big_m[idx_m] + 1L   
}
sum(big_m >= 2)



