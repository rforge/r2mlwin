matrix2df <- function(mat, idstub="id", weightstub="weight") {
  if (!is.matrix(mat) && !is(mat, "sparseMatrix")) {
    stop("Invalid input data")
  }
  numvars <- max(Matrix::rowSums(mat!=0))
  idcols <- data.frame(matrix(0, nrow(mat), numvars))
  rownames(idcols) <- 1:nrow(mat)
  colnames(idcols) <- paste0(idstub, 1:numvars)
  weightcols <- data.frame(matrix(0, nrow(mat), numvars))
  colnames(weightcols) <- paste0(weightstub, 1:numvars)
  rownames(weightcols) <- 1:nrow(mat)
  
  for (i in 1:nrow(mat)) {
    row <- mat[i, mat[i,] != 0, drop=FALSE]
    idcols[i, 1:length(row)] <- colnames(row)
    weightcols[i, 1:length(row)] <- as.numeric(row)
  }
  
  cbind(idcols, weightcols)
}