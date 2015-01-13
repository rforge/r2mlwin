.onAttach <- function(...) {
  if (is.null(getOption("MLwiN_path"))) {
    options(MLwiN_path = "C:/Program Files (x86)/MLwiN v2.31/")
  }
  packageStartupMessage(paste0("The MLwiN_path option is currently set to ", getOption("MLwiN_path"), "\n", "To change this use: options(MLwiN_path=\"<path to MLwiN>\")\n"))
} 
