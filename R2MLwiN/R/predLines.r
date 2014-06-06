predLines <- function(object, indata, xname, lev=2, selected=NULL, probs=c(.025,.975), 
                      legend=TRUE, legend.space="top", legend.ncol=4, ...){ 
  
  ## This function is to draw predicted lines at higher levels (level>=2)
  if (lev <2)
    stop("lev has to be greater than 1")
  cls <- class(object)
  if(!cls%in%c("mlwinfitIGLS", "mlwinfitMCMC"))
    stop('need a "mlwinfitIGLS" or "mlwinfitMCMC" class object')
  
  if (cls=="mlwinfitIGLS"){
    FP <- object["FP"]
    myresi <- object["residual"]
    levID <- object["levID"]
    
    categrv <- as.factor(indata[[rev(levID)[lev]]])
    levels(categrv) <- 1:length(levels(categrv))
    categrv <- as.integer(categrv)
    if (is.null(selected)){
      selected <- unique(categrv)
    }
    
    est.names <- names(myresi)[grep(paste("lev_",lev,"_resi_est",sep=""),names(myresi))]
    if (length(est.names)==1){
      est0 <- na.omit(myresi[[est.names]])
      if (length(est0)==length(unique(categrv))){
        est <- as.matrix(est0[categrv],ncol=1)
        colnames(est)=sub("_resi_est","",est.names)
      }else{
        stop("The number of groups do not match the number of residual estimates.")
      }
    }else{
      est0 <- NULL
      for (i in 1:length(est.names)){
        est0 <- cbind(est0,na.omit(myresi[[est.names[i]]]))
      }
      if (nrow(est0)==length(unique(categrv))){
        est <- as.matrix(est0[categrv,])
        colnames(est) <- sub("_resi_est","",est.names)
      }else{
        stop("The number of groups do not match the number of residual estimates.")
      }   
    }
    
    rpx.names <- sub(paste("lev_",lev,"_",sep=""),"",colnames(est))
    fp.names <- sub("FP_","",names(FP))
    tval <- 0
    for (i in 1:length(fp.names)){
      if (is.factor(indata[[fp.names[i]]])){
        indata[[fp.names[i]]] <- as.integer(indata[[fp.names[i]]])-1
      }
      tval <- tval+as.numeric(indata[[fp.names[i]]])*FP[i]  
    }
    for (i in 1:length(rpx.names)){
      if (is.factor(indata[[rpx.names[i]]])){
        indata[[rpx.names[i]]] <- as.integer(indata[[rpx.names[i]]])-1
      }
      tval <- tval+indata[[rpx.names[i]]]*est[,i]  
    }
    
    pred.min <- min(tval)
    pred.max <- max(tval)
    pred.diff <- pred.max-pred.min
    x <- indata[[xname]]
    x.min <- min(x)
    x.max <- max(x)
    
    if (legend){
      key=list(lines = Rows(trellis.par.get("superpose.line"),1:length(selected)),
               text=list(lab= as.character(selected)), space=legend.space, columns=legend.ncol)
    }else{
      key <- NULL
    }   
    
    trellis.obj <- xyplot(tval~x, 
                          prepanel = function(x,y,...){list(xlim=c(x.min, x.max), ylim=c(pred.min,pred.max))},
                          groups=categrv,
                          panel= function(x,y, groups,...){  
                            col <- Rows(trellis.par.get("superpose.line"),1:length(selected))$col
                            j <- 1
                            for (i in selected){
                              ypred <- y[which(groups==i)]
                              panel.xyplot(x=sort(x[which(groups==i)]),y=ypred[order(x[which(groups==i)])], col=col[j], type="l", ...)
                              j <- j +1
                            }
                          },key=key, ylab="ypred", xlab=xname, ...)
    print(trellis.obj)
  }  
  if (cls=="mlwinfitMCMC"){
    
    ## This function is to draw predicted lines (medians, lower quantiles and upper quantiles) at higher levels (level>=2) 
    resi.chains <- object["resi.chains"][[paste0("resi_lev", lev)]]
    chains <- object["chains"]
    levID <- object["levID"]
    
    categrv=indata[[rev(levID)[lev]]]
    if (is.null(selected)){
      selected =unique(categrv)
    }
    
    rpx.names=sub(paste("RP",lev,"_var_",sep=""),"",colnames(chains)[grep(paste("RP",lev,"_var_",sep=""),colnames(chains))])
    lenrpx=length(rpx.names)
    lencateg=length(unique(categrv))
    
    FP.pos=grep("FP_",colnames(chains))
    fp.names=sub("FP_","",colnames(chains)[FP.pos])
    tval=matrix(0, nrow(indata), nrow(chains))
    for (i in 1:length(fp.names)){
      fpxdata = indata[[fp.names[i]]]
      if (is.factor(fpxdata)) {
        fpxdata = as.integer(fpxdata) - 1
      }
      tval=tval+fpxdata%o%chains[,FP.pos[i]]
    }
    
    for (i in 1:length(rpx.names)){  
      for (j in 1:lencateg) {
        rpxdata = indata[[rpx.names[i]]][categrv==j]
        if (is.factor(rpxdata)) {
          rpxdata = as.integer(rpxdata) - 1
        }
        tval[categrv==j,] = tval[categrv==j,]+(rpxdata%o%resi.chains[,((i-1)*lencateg)+j])
      }
    }
    
    tval.med=apply(tval,1, median)
    tval.low=apply(tval,1,function(x) quantile(x,probs[1]))
    tval.up=apply(tval,1,function(x) quantile(x,probs[2]))
    pred.min=min(tval.low); pred.max=max(tval.up);pred.diff=pred.max-pred.min
    x=indata[[xname]]
    x.min=min(x) 
    x.max=max(x)
    
    if (legend){
      key=list(lines = Rows(trellis.par.get("superpose.line"),1:length(selected)),
               text=list(lab= as.character(selected)), space=legend.space, columns=legend.ncol)
    }else{
      key <- NULL
    }   
    
    trellis.obj <- xyplot(tval~x, 
                          prepanel = function(x,y,...){list(xlim=c(x.min, x.max), ylim=c(pred.min,pred.max))},
                          groups=categrv,
                          panel= function(x,y, groups,...){  
                            col <- Rows(trellis.par.get("superpose.line"),1:length(selected))$col
                            j <- 1
                            for (i in selected){
                              ypred <- y[which(groups==i)]
                              ypred.low=tval.low[which(categrv==i)]
                              ypred.up=tval.up[which(categrv==i)]
                              panel.xyplot(x=sort(x[which(groups==i)]),y=ypred[order(x[which(groups==i)])], col=col[j], type="l", ...)
                              panel.xyplot(x=sort(x[which(groups==i)]),y=ypred.low[order(x[which(groups==i)])], col=col[j], type="l",lty=3, ...)
                              panel.xyplot(x=sort(x[which(groups==i)]),y=ypred.up[order(x[which(groups==i)])], col=col[j], type="l",lty=3, ...)
                              j <- j +1
                            }
                          },key=key, ylab="ypred", xlab=xname, ...)
    print(trellis.obj)
  }
  invisible(trellis.obj)
}
