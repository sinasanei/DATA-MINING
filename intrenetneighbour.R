m <- matrix(1:16, 4, 4, byrow=TRUE)

result <- matrix(sapply(seq_along(m), function(i) {
  ind <- which(col(m) == col(m)[i] & abs(row(m)[i] - row(m)) == 1 | 
                 row(m) == row(m)[i] & abs(col(m)[i] - col(m)) == 1 |
                 abs(row(m)[i] - row(m)) == 1 & abs(col(m)[i] - col(m)) == 1 )
  mean(m[ind])
}), dim(m))


result
