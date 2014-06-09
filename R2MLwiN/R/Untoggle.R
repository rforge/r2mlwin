Untoggle <- function (categrv, name = NULL){
  ## this function will untoggle categorical variable into a few separate binary variables  
  vars <- unique(categrv)
  N <- length(vars)
  rvs <- sapply(1:N, function(x) as.integer(categrv == vars[x]))
  vals <- suppressWarnings(as.numeric(vars))
  cnames <- as.character(vars)
  if (!is.null(name)) {
    cnames[!is.na(vals)] <- paste(name, cnames[!is.na(vals)], sep = "_")
  } else {
    cnames[!is.na(vals)] <- paste("v_", cnames[!is.na(vals)], sep = "_")
  }
  colnames(rvs) <- cnames
  rvs
}