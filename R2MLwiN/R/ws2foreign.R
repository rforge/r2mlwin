ws2foreign=function(wsfile, foreignfile, MLwiNPath = "C:\\Program Files (x86)\\MLwiN v2.30\\", stdout="", stderr="", x64=FALSE){
    ## Convert MLwiN worksheet file to other data file which is used in Minitab, SAS, SPSS, or Stata
    temptfile =gsub("\\", "/", tempfile("coversion_",fileext=".txt"),fixed=TRUE)
    cat(file=temptfile)
    write("ECHO     0", temptfile, append=T)
    write(paste("LOAD   '",wsfile,"'",sep=""), temptfile, append=T)
    write(paste("STOR   '",foreignfile,"'",sep=""), temptfile, append=T)
    write("EXIT", temptfile, append=T)

    
    pathinfo <- file.info(MLwiNPath)
    if (is.na(pathinfo$isdir)) {
      stop(paste0(MLwiNPath, " does not exist"))
    }
    
    if (pathinfo$isdir == FALSE) {
      if (file.access(MLwiNPath, mode=1) == 0) {
        cmd <- MLwiNPath
      } else {
        stop(paste0(MLwiNPath, " is not executable"))
      }
    }
    
    if (pathinfo$isdir == TRUE) {
      if (x64) {
        cmd <- paste0(MLwiNPath, "/x64/mlnscript.exe")
        if (file.access(cmd, mode=1) != 0) {
          cmd <- paste0(MLwiNPath, "/i386/mlnscript.exe")
          if (file.access(cmd, mode=1) != 0) {
            cmd <- paste0(MLwiNPath, "/mlwin.exe")
            if (file.access(cmd, mode=1) != 0) {
              stop("Cannot find valid MLwiN executable")
            }
          }
        }
      } else {
        cmd <- paste0(MLwiNPath, "/i386/mlnscript.exe")
        if (file.access(cmd, mode=1) != 0) {
          cmd <- paste0(MLwiNPath, "/mlwin.exe")
          if (file.access(cmd, mode=1) != 0) {
            stop("Cannot find valid MLwiN executable")
          }                
        }              
      }
    }
    
    args <- paste0("/nogui /run ", "\"", temptfile, "\"")
    
    #WD <- getwd()
    system2(cmd, args=args, stdout=stdout, stderr=stderr)
    #setwd(WD)
    file.remove(temptfile)
    cat("\n")
}
