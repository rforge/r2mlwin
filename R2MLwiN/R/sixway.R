sixway <- function(chain,name=NULL,acf.maxlag=100,pacf.maxlag=10, ...){
  args <- list(...)
  
  isMCMC = TRUE
  if (!is.mcmc(chain) && !is.mcmc.list(chain)) {
    isMCMC = FALSE
    chain <- mcmc(chain)
  }
  
  if (nvar(chain) > 1) {
    stop("Cannot to plot more than one parameter at a time")
  }
  
  if (is.mcmc.list(chain)) {
    warning("Sixway does not fully support multiple chains - diagnostics will be on concatinated chains")
  }
  
  if(length(args) > 0 && "mar" %in% names(args)) {
    mar <- args[["mar"]]
  }else{
    mar <- c(4, 4, 2, 1)/2
  }
  
  if(length(args) > 0 && "mgp" %in% names(args)) {
    mgp <- args[["mgp"]]
  }else{
    mgp <- c(1,.25,0)
  }
  
  if (is.null(name)) {
    if (!is.null(varnames(chain))) {
      name=varnames(chain)[1]
    } else {
      name="x"
    }
  }
  
  dev.new()
  mypar <- par(mar = mar, mgp=mgp, ...)
  on.exit(par(mypar))
  split.screen( figs = c( 4, 1 ) )
  split.screen( figs = c( 1, 2 ), screen = 1 )
  split.screen( figs = c( 1, 2 ), screen = 2 )
  split.screen( figs = c( 1, 2 ), screen = 3)
  split.screen( figs = c( 1, 1 ), screen = 4)
  
  screen(5)
  traceplot(chain,xlab="stored updates",ylab="parameter",type="l",tcl=-.1,cex.axis=.8,main="")
  
  flatchain <- as.matrix(chain)
  
  screen(6)
  dens=density(flatchain)
  plot(dens,xlab="parameter value",ylab="kernel density",main="",tcl=-.1,cex.axis=.8)
  
  screen(7)
  aa=acf(flatchain,acf.maxlag,main="",mgp=c(1,.25,0),tcl=-.1,cex.axis=.8, ylim=c(0,1))
  rho=aa$acf[2]
  
  screen(8)
  pacf(flatchain,pacf.maxlag,main="",mgp=c(1,.25,0),tcl=-.1,cex.axis=.8, ylim=c(0,1))
  
  screen(9)
  mcse=MCSE(flatchain, rho, ll=.5, ul=20)
  plot(mcse[,1],mcse[,2],type='l',xlab="updates",ylab="MCSE",tcl=-.1,cex.axis=.8)
  
  RL1=raftery.diag(flatchain, q=0.025, r=0.005, s=0.95, converge.eps=0.001)
  RL2=raftery.diag(flatchain, q=0.975, r=0.005, s=0.95, converge.eps=0.001)
  Ndb=BD(mean(flatchain),var(flatchain),rho, k=2,alpha=0.05)
  
  screen(10)
  plot(1, xlim=c(1,10),ylim=c(1,5),type="n", axes=F, xlab="", ylab="",frame.plot=T)
  text(5,4.8, "Accuracy Diagnostics",cex=1.2)
  if(RL1$resmatrix[1]=="Error"){
    text(5,4,paste("RL diagnostic only available after ",RL1$resmatrix[2]," updates.",sep=""),cex=.8)
  }
  else{
    text(5,4,paste("Raftery-Lewis (quantile) : Nhat =(",RL1$resmatrix[1,"N"],",",RL2$resmatrix[1,"N"],")",sep=""),cex=.8)
  }
  text(5,3, "when q=(0.025,0.975), r=0.005 and s=0.95",cex=.8)
  text(5,2.1,paste("Brooks-Draper (mean) : Nhat =",Ndb),cex=.8)
  text(5,1.2, "when k=2 sigfigs and alpha=0.05",cex=.8)
  screen(11)
  plot(1, xlim=c(1,22),ylim=c(1,4),type="n", axes=F, xlab="", ylab="",frame.plot=T)
  text(10,3.8, "Summary Statistics",cex=1.2)
  quants=round(quantile(flatchain,c(.025,.05,.5,.95,.975)),3)
  text(10,2.9, paste("param name :",name, "posterior mean =",round(mean(flatchain),3),"SD = ",round(sd(flatchain),3),"mode =",round(dens$x[which.max(dens$y)],3)),cex=.8)
  text(10,2, paste("quantiles : 2.5% =",quants[1],"5% =",quants[2],"50% =",quants[3],"95% =",quants[4],"97.5% =",quants[5]),cex=.8)
  if (isMCMC){
    text(10,1.2, paste(niter(chain)*thin(chain),"actual iterations storing every ",paste(thin(chain),"th",sep="")," iteration. Effective Sample Size (ESS) =",round(effectiveSize(chain))),cex=.8)
  }
  else{
    text(10,1.2, paste(length(chain),"actual iterations. Diagnostics assume storing every 1th iteration. Effective Sample Size (ESS) =",round(effectiveSize(chain))),cex=.8)
  }
  close.screen( all.screens = TRUE )
}
