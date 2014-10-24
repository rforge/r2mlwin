df2matrix <- function(data, idcols, weightcols) {
  if (!is.data.frame(data) && !is.matrix(data)) {
    stop("Invalid input data")
  }
  id <- data[, idcols]
  weight <- data[, weightcols]
  id[weight==0] <- NA
  
  a <- NULL
  for (i in 1:ncol(id)) {
    a <- na.omit(union(a, id[, i]))
  }
  a <- sort(a)
  
  dat <- rep(0, nnzero(weight))
  indi <- dat
  indj <- dat
  
  ind <- 1
  for (i in 1:nrow(id)) {
    for (j in 1:ncol(id)) {
      if (!is.na(id[i, j])) {
        indi[ind] <- i
        indj[ind] <- which(a==id[i, j])
        dat[ind] <- weight[i, j]
        ind <- ind + 1
      }
    }
  }
  
  c <- sparseMatrix(indi, indj, x=dat, dimnames=list(1:nrow(id), a))
  c
}