#' This S4 class object is used to save the outputs from the fitted multilevel model using MCMC.
#'
#' ADD DESCRIPTION
#'
#' @section An instance of the Class:
#'  An instance is created by calling function \code{\link{runMLwiN}}.
#'
#' @slot Nobs Computes the number of complete observations.
#' @slot DataLength Total number of cases.
#' @slot Hierarchy For each higher level of a multilevel model, returns the number of units at that level, together with the minimum, mean and maximum number of lower-level units nested within units of the current level.
#' @slot burnin An integer specifying length of the burn-in.
#' @slot iterations An integer specifying the number of iterations after burn-in.
#' @slot D A vector specifying the type of distribution to be modelled, which can include \code{"Normal"}, \code{"Binomial"} \code{"Poisson"}, \code{"Multinomial"}, \code{"Multivariate Normal"}, or \code{"Mixed"}.
#' @slot Formula A formula object (or a character string) specifying a multilevel model.
#' @slot levID A character string (vector) of the specified level ID(s).
#' @slot merr A vector which sets-up measurement errors on predictor variables.
#' @slot fact A list of objects specified for factor analysis, including \code{nfact}, \code{lev.fact}, \code{nfactor}, \code{factor}, \code{loading} and \code{constr}.
#' @slot xc MIGHT NEED REVISING: A list of objects specified for cross-classified and/or multiple membership models, including \code{class}, \code{N1}, \code{weight}, \code{id} and \code{car}. 
#' @slot FP Displays the fixed part estimates.
#' @slot RP Displays the random part estimates.
#' @slot FP.cov Displays a covariance matrix of the fixed part estimates.
#' @slot RP.cov Displays a covariance matrix of the random part estimates.
#' @slot chains Captures the MCMC chains from MLwiN for all parameters.
#' @slot elapsed.time Calculates the CPU time used for fitting the model.
#' @slot BDIC Bayesian Deviance Information Criterion (DIC)
#' @slot call The matched call.
#' @slot LIKE The deviance statistic (-2*log(like)).
#' @slot fact.loadings If \code{fact} is not empty, then the factor loadings are returned.
#' @slot fact.loadings.sd TO ADD
#' @slot fact.cov If \code{fact} is not empty, then factor covariances are returned.
#' @slot fact.cov.sd TO ADD
#' @slot fact.chains If \code{fact} is not empty, then the factor chains are returned.
#' @slot MIdata If \code{dami} is not empty, then the complete response variable \code{y} are returned.
#' @slot residual If \code{resi.store} is \code{TRUE}, then the residual estimates at all levels are returned.
#' @slot resi.chains If \code{resi.store.levs} is not empty, then the residual chains at these levels are returned.
#' @slot version TO ADD
#' @slot data TO ADD
#'
#' @author Zhang, Z., Charlton, C.M.J., Parker, R.M.A., Leckie, G., and Browne,
#' W.J. (2014) Centre for Multilevel Modelling, University of Bristol.
#'
#' @seealso
#' \code{\link{runMLwiN}}
#'
#' @examples
#' \dontrun{
#' library(R2MLwiN)
#' # NOTE: Assumes MLwiN path is C:/Program Files (x86)/MLwiN v2.30/
#' # ...so please change relevant line if different
#' # if using R2MLwiN via WINE, the path may look like 
#' # options(MLwiN_path = "/home/USERNAME/.wine/drive_c/Program Files (x86)/MLwiN v2.30/") 
#'   
#' ## Example: tutorial
#' data(tutorial)
#' formula = normexam ~ (0|cons + standlrt) + (2|cons + standlrt) + (1|cons)
#' levID = c('school', 'student')
#' ## Choose option(s) for inference
#' estoptions = list(EstM = 1)
#' ## Fit the model
#' mymodel = runMLwiN(formula, levID, indata = tutorial, estoptions = estoptions)
#'   
#' ##summary method
#' summary(mymodel)
#' 
#' ##get method
#' mymodel["BDIC"]
#' }
#'
setClass(Class = "mlwinfitMCMC", representation = representation(version="character",Nobs="numeric",DataLength="numeric",Hierarchy="ANY",burnin="numeric",iterations="numeric",
                                                                 D="ANY", Formula="ANY", levID="character", merr="ANY", fact="ANY", xc="ANY",
                                                                 FP="numeric", RP="numeric", RP.cov="matrix", FP.cov="matrix", chains="ANY",
                                                                 elapsed.time="numeric", call="ANY",BDIC="numeric", LIKE="ANY", fact.loadings="numeric",fact.loadings.sd="numeric",
                                                                 fact.cov="numeric", fact.cov.sd="numeric", fact.chains="ANY", MIdata="data.frame",
                                                                 residual="list", resi.chains="ANY", data="data.frame"))
#' This method gets a slot of an instance of the S4 class.
#' @name [
#' @rdname extract-methods
#' @aliases [[,mlwinfitMCMC-method
#' @docType methods
setMethod(
  f= "[",
  signature="mlwinfitMCMC",
  definition=function(x,i,j,drop){
    if(i=="version"){return(x@version)}else {}
    if(i=="Nobs"){return(x@Nobs)}else {}
    if(i=="DataLength"){return(x@DataLength)}else {}                
    if(i=="Hierarchy"){return(x@Hierarchy)}else {}        
    if(i=="burnin"){return(x@burnin)}else {}
    if(i=="iterations"){return(x@iterations)}else {}
    if(i=="D"){return(x@D)}else {}
    if(i=="Formula"){return(x@Formula)}else {}
    if(i=="levID"){return(x@levID)}else {}
    if(i=="merr"){return(x@merr)}else {}
    if(i=="fact"){return(x@fact)}else {}
    if(i=="xc"){return(x@xc)}else {}
    if(i=="FP"){return(x@FP)}else {}
    if(i=="RP"){return(x@RP)}else {}
    if(i=="FP.cov"){return(x@FP.cov)}else {}
    if(i=="RP.cov"){return(x@RP.cov)}else {}
    if(i=="chains"){return(x@chains)}else {}
    if(i=="elapsed.time"){return(x@elapsed.time)}else {}
    if(i=="BDIC"){return(x@BDIC)}else {}
    if(i=="call"){return(x@call)}else {}
    if(i=="LIKE"){return(x@LIKE)}else {}
    if(i=="fact.loadings"){return(x@fact.loadings)}else {}
    if(i=="fact.loadings.sd"){return(x@fact.loadings.sd)}else {}
    if(i=="fact.cov"){return(x@fact.cov)}else {}
    if(i=="fact.cov.sd"){return(x@fact.cov.sd)}else {}
    if(i=="fact.chains"){return(x@fact.chains)}else {}
    if(i=="MIdata"){return(x@MIdata)}else {}
    if(i=="residual"){return(x@residual)}else {}
    if(i=="resi.chains"){return(x@resi.chains)}else {}
    if(i=="data"){return(x@data)}else {}
  }
)

#' Replace names of mlwinfitMCMC
#' @name [
#' @rdname extract-methods
#' @aliases [<-,mlwinfitMCMC-method
#' @docType methods
setReplaceMethod(
  f= "[",
  signature="mlwinfitMCMC",
  definition=function(x,i,j,value){
    if(i=="version"){x@version<-value}else {}
    if(i=="Nobs"){x@Nobs<-value}else {}
    if(i=="DataLength"){x@DataLength<-value}else {}                                           
    if(i=="Hierarchy"){x@Hierarchy<-value}else {} 
    if(i=="burnin"){x@burnin<-value}else {}
    if(i=="iterations"){x@iterations<-value}else {}
    if(i=="D"){x@D<-value}else {}
    if(i=="Formula"){x@Formula<-value}else {}
    if(i=="levID"){x@levID<-value}else {}
    if(i=="merr"){x@merr<-value}else {}
    if(i=="fact"){x@fact<-value}else {}
    if(i=="xc"){x@xc<-value}else {}
    if(i=="FP"){x@FP<-value}else {}
    if(i=="RP"){x@RP<-value}else {}
    if(i=="FP.cov"){x@FP.cov<-value}else {}
    if(i=="RP.cov"){x@RP.cov<-value}else {}
    if(i=="chains"){x@chains<-value}else {}
    if(i=="elapsed.time"){x@elapsed.time<-value}else {}
    if(i=="BDIC"){x@BDIC<-value}else {}
    if(i=="call"){x@call<-value}else {}
    if(i=="LIKE"){x@LIKE<-value}else {}
    if(i=="fact.loadings"){x@fact.loadings<-value}else {}
    if(i=="fact.loadings.sd"){x@fact.loadings.sd<-value}else {}
    if(i=="fact.cov"){x@fact.cov<-value}else {}
    if(i=="fact.cov.sd"){x@fact.cov.sd<-value}else {}
    if(i=="fact.chains"){x@fact.chains<-value}else {}
    if(i=="MIdata"){x@MIdata<-value}else {}
    if(i=="residual"){x@residual<-value}else {}
    if(i=="resi.chains"){x@resi.chains<-value}else {}
    if(i=="data"){x@data<-value}else {}
    validObject(x)
    return (x)
  }
)

#' Extract parts of mlwinfitMCMC
#' @name [[
#' @rdname extract-methods
#' @aliases [,mlwinfitMCMC-method
#' @docType methods
setMethod(
  f= "[[",
  signature="mlwinfitMCMC",
  definition=function(x,i,j,drop){
    if(i=="version"){return(x@version)}else {}
    if(i=="Nobs"){return(x@Nobs)}else {}
    if(i=="DataLength"){return(x@DataLength)}else {}
    if(i=="Hierarchy"){return(x@Hierarchy)}else {}
    if(i=="burnin"){return(x@burnin)}else {}
    if(i=="iterations"){return(x@iterations)}else {}
    if(i=="D"){return(x@D)}else {}
    if(i=="Formula"){return(x@Formula)}else {}
    if(i=="levID"){return(x@levID)}else {}
    if(i=="merr"){return(x@merr)}else {}
    if(i=="fact"){return(x@fact)}else {}
    if(i=="xc"){return(x@xc)}else {}
    if(i=="FP"){return(x@FP)}else {}
    if(i=="RP"){return(x@RP)}else {}
    if(i=="FP.cov"){return(x@FP.cov)}else {}
    if(i=="RP.cov"){return(x@RP.cov)}else {}
    if(i=="chains"){return(x@chains)}else {}
    if(i=="elapsed.time"){return(x@elapsed.time)}else {}
    if(i=="BDIC"){return(x@BDIC)}else {}
    if(i=="call"){return(x@call)}else {}
    if(i=="LIKE"){return(x@LIKE)}else {}
    if(i=="fact.loadings"){return(x@fact.loadings)}else {}
    if(i=="fact.loadings.sd"){return(x@fact.loadings.sd)}else {}
    if(i=="fact.cov"){return(x@fact.cov)}else {}
    if(i=="fact.cov.sd"){return(x@fact.cov.sd)}else {}
    if(i=="fact.chains"){return(x@fact.chains)}else {}
    if(i=="MIdata"){return(x@MIdata)}else {}
    if(i=="residual"){return(x@residual)}else {}
    if(i=="resi.chains"){return(x@resi.chains)}else {}
    if(i=="data"){return(x@data)}else {}
  }
)

#' Replace names of mlwinfitMCMC
#' @name [[
#' @rdname extract-methods
#' @aliases [<-,mlwinfitMCMC-method
#' @docType methods
setReplaceMethod(
  f= "[[",
  signature="mlwinfitMCMC",
  definition=function(x,i,j,value){
    if(i=="version"){x@version<-value}else {}
    if(i=="Nobs"){x@Nobs<-value}else {}
    if(i=="DataLength"){x@DataLength<-value}else {}
    if(i=="Hierarchy"){x@Hierarchy<-value}else {}
    if(i=="burnin"){x@burnin<-value}else {}
    if(i=="iterations"){x@iterations<-value}else {}
    if(i=="D"){x@D<-value}else {}
    if(i=="Formula"){x@Formula<-value}else {}
    if(i=="levID"){x@levID<-value}else {}
    if(i=="merr"){x@merr<-value}else {}
    if(i=="fact"){x@fact<-value}else {}
    if(i=="xc"){x@xc<-value}else {}
    if(i=="FP"){x@FP<-value}else {}
    if(i=="RP"){x@RP<-value}else {}
    if(i=="FP.cov"){x@FP.cov<-value}else {}
    if(i=="RP.cov"){x@RP.cov<-value}else {}
    if(i=="chains"){x@chains<-value}else {}
    if(i=="elapsed.time"){x@elapsed.time<-value}else {}
    if(i=="BDIC"){x@BDIC<-value}else {}
    if(i=="call"){x@call<-value}else {}
    if(i=="LIKE"){x@LIKE<-value}else {}
    if(i=="fact.loadings"){x@fact.loadings<-value}else {}
    if(i=="fact.loadings.sd"){x@fact.loadings.sd<-value}else {}
    if(i=="fact.cov"){x@fact.cov<-value}else {}
    if(i=="fact.cov.sd"){x@fact.cov.sd<-value}else {}
    if(i=="fact.chains"){x@fact.chains<-value}else {}
    if(i=="MIdata"){x@MIdata<-value}else {}
    if(i=="residual"){x@residual<-value}else {}
    if(i=="resi.chains"){x@resi.chains<-value}else {}
    if(i=="data"){x@data<-value}else {}
    validObject(x)
    return (x)
  }
)
setMethod("summary",
          signature(object = "mlwinfitMCMC"),                  
          function (object,  ...)
          {
            object
          }
)
printMCMC <- function(x, digits = max(3, getOption("digits") - 2), signif.stars = getOption("show.signif.stars"), z.ratio = TRUE,...)
{
  
  object <- summary(x)
  align2right=function(titlename,ele){
    #for printing the table on the screen
    all.ele=c(titlename,ele)
    len.all.ele=nchar(all.ele)
    max.len.ele=max(len.all.ele)
    for (j in 1:length(all.ele)){
      if (len.all.ele[j]<max.len.ele){
        len.diff=max.len.ele-len.all.ele[j]
        all.ele[j]=paste(paste(rep(" ",len.diff),collapse=""),all.ele[j],sep="")
      }
    }
    all.ele
  }
  
  align2left=function(titlename,ele){
    #for printing the table on the screen
    all.ele=c(titlename,ele)
    len.all.ele=nchar(all.ele)
    max.len.ele=max(len.all.ele)
    for (j in 1:length(all.ele)){
      if (len.all.ele[j]<max.len.ele){
        len.diff=max.len.ele-len.all.ele[j]
        all.ele[j]=paste(all.ele[j],paste(rep(" ",len.diff),collapse=""),sep="")
      }
    }
    all.ele
  }
  
  signifstar = function(pval){
    starstr="N/A"
    if (!is.na(pval) && pval>=0&&pval<=1){
      if(pval<0.001){
        starstr='***'
      }
      if(pval>=0.001&&pval<0.01){
        starstr='** '
      }
      if(pval>=0.01&&pval<0.05){
        starstr='*  '
      }
      if(pval>=0.05&&pval<0.1){
        starstr='.  '
      }
      if(pval>=0.1){
        starstr='   '
      }
    }
    starstr
  }

  chainnames=varnames(object@chains)
  FP.names=grep("^FP_", chainnames, value=TRUE)
  RP.names=grep("^RP_", chainnames, value=TRUE)
  ESS=effectiveSize(object@chains)
  levID0=object@levID
  cat("\n")
  cat(paste(rep("-",50),collapse="*"),"\n")
  cat(object@version, " multilevel model",paste("(",object@D[1],")",sep=""),"\n")
  if (!is.null(object@Hierarchy)) print(object@Hierarchy)
  if (!is.null(object@xc) && isTRUE(object@xc)){
    cat("Estimation algorithm:  MCMC      Cross-classified              Elapsed time :",paste(round(object@elapsed.time,2),"s",sep=""), "\n")
  }else{
    cat("Estimation algorithm:  MCMC      Elapsed time :",paste(round(object@elapsed.time,2),"s",sep=""), "\n")
  }
  cat("Number of obs: ",object@Nobs,"(from total",object@DataLength,")        Number of iter.:",object@iterations,"         Burn-in:", object@burnin , "\n")
    
  if (!(object@D[1]=="Mixed")&&is.null(object@merr)&&is.null(object@fact)){
    cat("Bayesian Deviance Information Criterion (DIC)\n")
    cat("Dbar      D(thetabar)    pD      DIC\n")
    cat(formatC(object@BDIC,format="f",digits=3,width=-10),"\n")
  }else{
    cat(paste("Deviance statistic: ", round(object@LIKE,1)),"\n")
  }
  
  
  cat(paste(rep("-",50),collapse="-"),"\n")
  cat("The model formula:\n")
  print(object@Formula)
  levID.display=""
  if (is.na(levID0[length(levID0)])){
    levID0=levID0[-length(levID0)]
  }
  for (i in 1:length(levID0)){
    levID.display=paste(levID.display,"Level ",length(levID0)+1-i,": ",levID0[i],"     ",sep="")
  }
  cat(levID.display,"\n")
  cat(paste(rep("-",50),collapse="-"),"\n")
  
  if (!is.null(object@fact)&&object@D[1]=='Multivariate Normal'){
    qt025=object@fact.loadings-qnorm(.975)*object@fact.loadings.sd
    qt975=object@fact.loadings+qnorm(.975)*object@fact.loadings.sd
    loads=rbind(object@fact.loadings,object@fact.loadings.sd,qt025,qt975)
    
    for (j in 1:object@fact$nfact){
      cat("The estimates of factor",j,"loadings:\n")
      loadx.names=colnames(loads)[grep(paste0("load+",j,"+\\_"),colnames(loads))]
      loadx=loads[,loadx.names]
      printcol0=align2left("        ",loadx.names)
      printcol1=align2right("Coef.",format(round(loadx[1,],digits),nsmall = digits))
      printcol2=align2right("Std. Err.",format(round(loadx[2,],digits),nsmall = digits))
      printcol3=align2right("[95% Conf.",format(round(loadx[3,],digits),nsmall = digits))
      printcol4=align2right("Interval]",format(round(loadx[4,],digits),nsmall = digits))
      for (i in 1:(ncol(loadx)+1)){
        cat(printcol0[i]," ",printcol1[i]," ",printcol2[i]," ",printcol3[i]," ",printcol4[i],"\n")
      }
      cat(paste(rep("-",50),collapse="-"),"\n")
    }
    
    qt025=object@fact.cov-qnorm(.975)*object@fact.cov.sd
    qt975=object@fact.cov+qnorm(.975)*object@fact.cov.sd
    fcov=rbind(object@fact.cov,object@fact.cov.sd,qt025,qt975)
    
    cat("The estimates of factor covariances:\n")
    printcol0=align2left("        ",colnames(fcov))
    printcol1=align2right("Coef.",format(round(fcov[1,],digits),nsmall = digits))
    printcol2=align2right("Std. Err.",format(round(fcov[2,],digits),nsmall = digits))
    printcol3=align2right("[95% Conf.",format(round(fcov[3,],digits),nsmall = digits))
    printcol4=align2right("Interval]",format(round(fcov[4,],digits),nsmall = digits))
    for (i in 1:(ncol(fcov)+1)){
      cat(printcol0[i]," ",printcol1[i]," ",printcol2[i]," ",printcol3[i]," ",printcol4[i],"\n")
    }
    cat(paste(rep("-",50),collapse="-"),"\n")
  }
  
  cat("The fixed part estimates: ","\n")
  FP.print=rbind(object@FP,sqrt(diag(object@FP.cov)))
  if (sum(grepl("bcons",colnames(object@chains)))>0){
    bcons.pos=grep("bcons",colnames(object@chains))
    object@chains[1,bcons.pos]=object@chains[1,bcons.pos]-0.001
  }
  
  t.stats=apply(object@chains,2,function(x) mean(x)/sd(x))
  
  p.values=2*pnorm(abs(t.stats),lower.tail =FALSE)
  t.stat=NULL
  for (i in FP.names)  t.stat=c(t.stat, t.stats[[i]])
  p.value=NULL                
  for (i in FP.names)  p.value=c(p.value, p.values[[i]])
  onesided.p.value=NULL
  for (i in FP.names){
    x=object@chains[,i]
    if(sign(mean(x))>0){
      onesided.p.values=sum(x<0)/length(x)
    }else{
      onesided.p.values=sum(x>0)/length(x)
    }
    onesided.p.value=c(onesided.p.value, onesided.p.values)
  }
  strstar=as.vector(sapply(p.value,signifstar))
  qt025=NULL
  for (i in FP.names)  qt025=c(qt025, quantile(object@chains[,i],.025))
  qt975=NULL
  for (i in FP.names)  qt975=c(qt975, quantile(object@chains[,i],.975))
  FP.print=rbind(FP.print,t.stat,p.value,onesided.p.value,qt025,qt975,ESS[FP.names])
  FP.names2=gsub("FP+\\_","",FP.names)
  
  printcol0=align2left("        ",FP.names2)
  printcol1=align2right("Coef.",format(round(FP.print[1,],digits),nsmall = digits))
  printcol2=align2right("Std. Err.",format(round(FP.print[2,],digits),nsmall = digits))
  printcol3=align2right("z",format(round(FP.print[3,],2),nsmall = 2))
  printcol4=align2right("Pr(>|z|)",formatC(FP.print[4,]))
  printcol4b=align2right("   ",strstar)
  printcol5=align2right("pMCMC(1-sided)",formatC(FP.print[5,]))
  printcol6=align2right("[95% Cred.",format(round(FP.print[6,],digits),nsmall = digits))
  printcol7=align2right("Interval]",format(round(FP.print[7,],digits),nsmall = digits))
  printcol8=align2right("ESS",format(round(FP.print[8,]),nsmall=0))
  for (i in 1:(ncol(FP.print)+1)){
    cat(printcol0[i]," ",printcol1[i]," ",printcol2[i]," ")
    if (z.ratio){
      cat(printcol3[i]," ",printcol4[i]," ")
      if (signif.stars){
        cat(printcol4b[i]," ")
      }
    }
    else{
      cat(printcol5[i]," ")
    }
    cat(printcol6[i]," ",printcol7[i]," ",printcol8[i],"\n")
  }
  if(signif.stars & z.ratio){
    cat("Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 ","\n")
  }
  
  nlev=length(object@levID)
  if (is.na(object@levID[length(object@levID)])){
    mlwinlev=(nlev-1):1
    levID2=levID0
  }else{
    mlwinlev=nlev:1
    levID2=object@levID
  }
  
  RP.print=rbind(object@RP,sqrt(diag(object@RP.cov)))
  qt025=NULL
  for (i in RP.names)  qt025=c(qt025, quantile(object@chains[,i],.025))
  qt975=NULL
  for (i in RP.names)  qt975=c(qt975, quantile(object@chains[,i],.975))
  RP.print=rbind(RP.print,qt025,qt975,ESS[RP.names])
  for (i in 1:length(mlwinlev)){
    RPx.pos=grep(paste("RP",mlwinlev[i],sep=""),RP.names)
    if (length(RPx.pos)!=0){
      cat(paste(rep("-",50),collapse="-"),"\n")
      RPx.names=gsub(paste("RP+",mlwinlev[i],"+\\_",sep=""),"",RP.names[RPx.pos])
      RPx = as.matrix(RP.print[,RPx.pos],nrow=4)
      printcol0=align2left("        ",RPx.names)
      printcol1=align2right("Coef.",format(round(RPx[1,],digits),nsmall = digits))
      printcol2=align2right("Std. Err.",format(round(RPx[2,],digits),nsmall = digits))
      printcol5=align2right("[95% Cred.",format(round(RPx[3,],digits),nsmall = digits))
      printcol6=align2right("Interval]",format(round(RPx[4,],digits),nsmall = digits))
      printcol7=align2right("ESS",format(round(RPx[5,]),nsmall = 0))
      cat("The random part estimates at the",levID2[i],"level:","\n")
      for (i in 1:(ncol(RPx)+1)){
        cat(printcol0[i]," ",printcol1[i]," ",printcol2[i]," ",printcol5[i]," ",printcol6[i]," ",printcol7[i],"\n")
      }
    }
  }
  cat(paste(rep("-",50),collapse="*"),"\n")
}

setMethod("print", "mlwinfitMCMC", printMCMC)
setMethod("show",  "mlwinfitMCMC", function(object) printMCMC(object))
setMethod("update", "mlwinfitMCMC", updateMLwiN)

setMethod("formula", "mlwinfitMCMC", function(x, env = parent.frame(), ...) {
  as.formula(x@Formula)
})

setMethod("coef", "mlwinfitMCMC", function (object,  ...) {
  c(object@FP, object@RP)           
})

setMethod("coefficients", "mlwinfitMCMC", function (object,  ...) {
  coef(object)
})

setMethod("vcov", "mlwinfitMCMC", function (object,  ...) {
  m <- matrix(0, nrow(object@FP.cov)+nrow(object@RP.cov), ncol(object@FP.cov)+ncol(object@RP.cov))
  colnames(m) <- c(colnames(object@FP.cov), colnames(object@RP.cov))
  rownames(m) <- c(rownames(object@FP.cov), rownames(object@RP.cov))
  m[colnames(object@FP.cov), rownames(object@FP.cov)] <- object@FP.cov  
  m[colnames(object@RP.cov), rownames(object@RP.cov)] <- object@RP.cov 
  m
})

setMethod("df.residual", "mlwinfitMCMC", function (object,  ...) {
  nobs(object) - length(coef(object))
})

setMethod("fitted", "mlwinfitMCMC", function (object,  ...) {
  predict(object, type="response")
})

setMethod("fitted.values", "mlwinfitMCMC", function (object,  ...) {
  fitted(object)
})

setMethod("residuals", "mlwinfitMCMC", function (object,  ...) {
  form <- Formula.translate(object@Formula, object@D, object@data)
  if (!is.list(form$resp)) {
    object@data[[form$resp]] - fitted(object)
  } else {
    warning("residuals only implemented for univariate models")
    NULL
  }
})

setMethod("resid", "mlwinfitMCMC", function (object,  ...) {
  residuals(object)
})

setMethod("predict", "mlwinfitMCMC", function (object, newdata=NULL, params=NULL, type="link", se.fit=FALSE, terms=NULL,  ...) {
  if (is.null(newdata)) {
    indata <- object@data
  } else {
    indata <- Formula.translate(object@Formula, object@D, object@data)$indata
  }
  if (is.null(params)) {
    fp.names <- names(FP <- object@FP)
  } else {
    fp.names <- params
  }
  x.names <- sub("FP_","",fp.names)
  if (type=="link") {
    tval <- as.vector(as.matrix(indata[x.names]) %*% as.matrix(object@FP[fp.names]))
    if (se.fit) {
      #seval <- as.vector(sqrt(diag(as.matrix(indata[x.names]) %*% as.matrix(object@FP.cov[fp.names, fp.names]) %*% t(as.matrix(indata[x.names])))))
      seval <- as.vector(sqrt(rowSums(as.matrix(indata[x.names]) %*% as.matrix(object@FP.cov[fp.names, fp.names]) * indata[x.names])))
      return(list(fit=tval, se.fit=seval))
    } else {
      return(tval)
    }
  }
  if (type == "terms") {
    if (is.null(terms)) {
      terms <- fp.names
    } else {
      x.names <- sub("FP_","",terms)
    }
    tval <- as.matrix(t(t(indata[x.names]) * object@FP[fp.names]))
    if (se.fit) {
      seval <- as.matrix(sqrt(t(t(indata[x.names]^2) * diag(object@FP.cov[fp.names, fp.names]))))
      return(list(fit=tval, se.fit=seval))
    } else {
      return(tval)
    }
  }
  if (type == "response") {
    tval <- as.vector(as.matrix(indata[x.names]) %*% as.matrix(object@FP[fp.names]))
    D = object@D
    if (D == "Normal" || D[1] == "Multivariate Normal") {
      return(tval)
    }
    if (D[1] == "Binomial") {
      if (D[2] == "logit") {
        antilogit <- function(x) { exp(x) / (1 + exp(x) ) }
        return(antilogit(tval))
      }
      if (D[2] == "probit") {
        return(pnorm(tval))
      }
      if (D[2] == "cloglog") {
        anticloglog <- function(x) { 1 - exp(-exp(x)) }
        return(anticloglog(tval))
      }
    }
    if (D[1] == "Poisson") {
      return(exp(tval))
    }
    if (D[1] == "Negbinom") {
      return(exp(tval))
    }
    if (D[1] == "Mixed") {
    }
    if (D[1] == "Multinomial") {
    }
    warning("link function transformation not yet implemented")
    return(NULL)
  }
})

setMethod("nobs", "mlwinfitMCMC", function (object,  ...) {
  object@Nobs
})
