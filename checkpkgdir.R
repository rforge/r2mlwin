checkpkgdir <- function(dir) {
   x <- system(paste("R CMD build", dir), intern=TRUE)
   cat(x, sep="\n")
   tarball <- grep("^\\* building '", x, value = TRUE)
   if (length(tarball) == 1) {
     tarball <- sub("^\\* building '", "", tarball)
     tarball <- sub("'$", "", tarball)
     system(paste("R CMD check", tarball))
   }
}

checkpkgdir("R2MLwiN")
