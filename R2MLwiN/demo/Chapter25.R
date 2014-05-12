############################################################################
#     MLwiN MCMC Manual
#
# 25  Hierarchical Centring . . . . . . . . . . . . . . . . . . . . . . .401
#
#     Browne, W.J. (2009) MCMC Estimation in MLwiN, v2.13. Centre for
#     Multilevel Modelling, University of Bristol.
############################################################################
#     R script to replicate all analyses using R2MLwiN
#
#     Zhang, Z., Charlton, C., Parker, R, Leckie, G., and Browne, W.J.
#     Centre for Multilevel Modelling, 2012
#     http://www.bristol.ac.uk/cmm/software/R2MLwiN/
############################################################################

# 25.1 What is hierarchical centering? . . . . . . . . . . . . . . . . . 401

# 25.2 Centring Normal models using WinBUGS . . . . . . . . . . . . . . .403

library(R2MLwiN)
## Input the MLwiN tutorial data set
# MLwiN folder
mlwin <- getOption("MLwiN_path")
while (!file.access(mlwin, mode=1)==0) {
  cat("Please specify the root MLwiN folder or the full path to the MLwiN executable:\n")
  mlwin=scan(what=character(0),sep ="\n")
  mlwin=gsub("\\", "/",mlwin, fixed=TRUE)  
}
options(MLwiN_path=mlwin)

# User's input if necessary

## Read tutorial data from runmlwin (Leckie&Charlton, 2011) data folder
library(foreign); indata =read.dta("http://www.bristol.ac.uk/cmm/media/runmlwin/tutorial.dta")

## Alternatively converts tutorial.ws under mlwin sample folder to tutorial.dta
## MLwiN sample worksheet folder
#wsfile=paste(mlwin,"/samples/tutorial.ws",sep="")
## the tutorial.dta will be save under the temporary folder
#inputfile=paste(tempdir(),"/tutorial.dta",sep="")
#ws2foreign(wsfile, foreignfile=inputfile)
#library(foreign); indata =read.dta(inputfile)

## openbugs executable
if(!exists("openbugs")) openbugs="C:/Program Files (x86)/OpenBUGS321/OpenBUGS.exe"
while (!file.access(openbugs,mode=0)==0||!file.access(openbugs,mode=1)==0||!file.access(openbugs,mode=4)==0){
    cat("Please specify the path for the OpenBUGS executable:\n")
    openbugs=scan(what=character(0),sep ="\n")
    openbugs=gsub("\\", "/",openbugs, fixed=TRUE)
}

# User's input if necessary

## winbugs executable
#winbugs="C:/Program Files (x86)/WinBUGS14/WinBUGS14.exe"

## Define the model
formula=normexam~(0|cons+standlrt)+(2|cons)+(1|cons)
levID=c('school','student')

## Hierarchical centring at level 2 (DO NOT USE VERSION 2.25; the bug has been fixed for VERSION 2.26)
estoptions= list(EstM=1, mcmcOptions=list(hcen=2),show.file=T)
mymodel=runMLwiN(formula, levID, indata=indata, estoptions=estoptions,BUGO=c(version=4,n.chains=1,debug=F,seed=1,bugs=openbugs, OpenBugs = T))
sixway(mymodel[[1]][,"beta[1]"],"beta[1]")

# 25.3 Binomial hierarchical centering algorithm . . . . . . . . . . . . 408

# 25.4 Binomial example in practice . . . . . . . . . . . . . . . . . . .410

## Read bang1 data from runmlwin (Leckie&Charlton, 2011) data folder
library(foreign); indata =read.dta("http://www.bristol.ac.uk/cmm/media/runmlwin/bang1.dta")

## Alternatively converts bang1.ws under mlwin sample folder to bang1.dta
## MLwiN sample worksheet folder
#wsfile=paste(mlwin,"/samples/bang1.ws",sep="")
## the tutorial.dta will be save under the temporary folder
#inputfile=paste(tempdir(),"/bang1.dta",sep="")
#ws2foreign(wsfile, foreignfile=inputfile)
#library(foreign); indata =read.dta(inputfile)
levels(indata[["lc"]])=c("nokids",     "onekid",     "twokids",    "threepluskids")

## openbugs executable
if(!exists("openbugs")) openbugs="C:/Program Files (x86)/OpenBUGS321/OpenBUGS.exe"
while (!file.access(openbugs,mode=0)==0||!file.access(openbugs,mode=1)==0||!file.access(openbugs,mode=4)==0){
  cat("Please specify the path for the OpenBUGS executable:\n")
  openbugs=scan(what=character(0),sep ="\n")
  openbugs=gsub("\\", "/",openbugs, fixed=TRUE)
}

# User's input if necessary

## winbugs executable
#winbugs="C:/Program Files (x86)/WinBUGS14/WinBUGS14.exe"

## Define the model
formula=logit(use,denomb)~(0|cons+age+lc[nokids]+urban)+(2|cons+urban)
levID=c('district','woman')

## Hierarchical centring at level 2
estoptions= list(EstM=1, mcmcOptions=list(hcen=2))
(mymodel=runMLwiN(formula, levID, D="Binomial", indata=indata, estoptions=estoptions))
trajectories(mymodel["chains"])

## Hierarchical centring at level 2 + Orthogonal updates
estoptions= list(EstM=1, mcmcOptions=list(hcen=2,orth=1))
(mymodel=runMLwiN(formula, levID, D="Binomial", indata=indata, estoptions=estoptions))
trajectories(mymodel["chains"])

# 25.5 The Melanoma example . . . . . . . . . . . . . . . . . . . . . . .414

wsfile=paste(mlwin,"/samples/mmmec1.ws",sep="")
# the tutorial.dta will be save under the temporary folder
inputfile=paste(tempdir(),"/mmmec1.dta",sep="")
ws2foreign(wsfile, foreignfile=inputfile)
library(foreign); indata =read.dta(inputfile)

indata[["logexp"]]=double2singlePrecision(log(indata[["exp"]]))
levels(indata[["nation"]])=c("Belgium", "W_Germany", "Denmark", "France", "UK", "Italy", "Ireland", "Luxembourg", "Netherlands")

## Define the model
formula=log(obs,logexp)~(0|nation[]+Belgium:uvbi+W_Germany:uvbi+Denmark:uvbi+France:uvbi+UK:uvbi+Italy:uvbi+Ireland:uvbi+Luxembourg:uvbi+Netherlands:uvbi)+(2|cons)
levID=c('region','county')

## Hierarchical centring at level 2
estoptions= list(EstM=1,mcmcMeth=list(iterations=50000), mcmcOptions=list(hcen=2))
(mymodel=runMLwiN(formula, levID, D="Poisson", indata=indata, estoptions=estoptions))
sixway(mymodel["chains"][,"FP_Belgium"],acf.maxlag=100,"beta_1")

## Hierarchical centring at level 2 + Orthogonal updates
estoptions= list(EstM=1, mcmcMeth=list(iterations=50000), mcmcOptions=list(orth=1,hcen=2))
(mymodel=runMLwiN(formula, levID, D="Poisson", indata=indata, estoptions=estoptions))
sixway(mymodel["chains"][,"FP_Belgium"],acf.maxlag=100,"beta_1")

# 25.6 Normal response models in MLwiN . . . . . . . . . . . . . . . . . 419

## Read tutorial data from runmlwin (Leckie&Charlton, 2011) data folder
library(foreign); indata =read.dta("http://www.bristol.ac.uk/cmm/media/runmlwin/tutorial.dta")

## Alternatively converts tutorial.ws under mlwin sample folder to tutorial.dta
## MLwiN sample worksheet folder
#wsfile=paste(mlwin,"/samples/tutorial.ws",sep="")
## the tutorial.dta will be save under the temporary folder
#inputfile=paste(tempdir(),"/tutorial.dta",sep="")
#ws2foreign(wsfile, foreignfile=inputfile)
#library(foreign); indata =read.dta(inputfile)

## Define the model
formula=normexam~(0|cons+standlrt)+(2|cons)+(1|cons)
levID=c('school','student')

## Univariate MH
## Hierarchical centring at level 2
estoptions= list(EstM=1, mcmcMeth=list(fixM=2,residM=2),mcmcOptions=list(hcen=2))
(mymodel=runMLwiN(formula, levID, indata=indata, estoptions=estoptions))
trajectories(mymodel["chains"],Range=c(4501,5000))
## Gibbs
## Hierarchical centring at level 2
estoptions= list(EstM=1, mcmcOptions=list(hcen=2))
(mymodel=runMLwiN(formula, levID, indata=indata, estoptions=estoptions))
trajectories(mymodel["chains"],Range=c(4501,5000))

# Chapter learning outcomes . . . . . . . . . . . . . . . . . . . . . . .422





############################################################################
