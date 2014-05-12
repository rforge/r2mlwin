############################################################################
#     MLwiN MCMC Manual
#
# 2   Single Level Normal Response Modelling . . . . . . . . . . . . . . .21
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
formula=normexam~(0|cons+standlrt)+(1|cons)
levID='student'
## Choose IGLS algoritm for estimation
## Fit the model
(mymodel=runMLwiN(formula, levID, indata=indata))

# 2.1 Running the Gibbs Sampler . . . . . . . . . . . . . . . . . . . . . 26

## Choose MCMC algoritm for estimation
estoptions= list(EstM=1)
## Fit the model
(mymodel=runMLwiN(formula, levID, indata=indata, estoptions=estoptions))

estimates=mymodel["chains"]
par(mfrow=c(2,2))
plot(1:nrow(estimates),estimates[,"deviance"],xlab="iteration",
ylab=expression(paste("Est. of deviance")),type="l")
plot(1:nrow(estimates),estimates[,"FP_cons"],xlab="iteration",
ylab=expression(paste("Est. of ",beta[0])),type="l")
plot(1:nrow(estimates),estimates[,"FP_standlrt"],xlab="iteration",
ylab=expression(paste("Est. of ",beta[1])),type="l")
plot(1:nrow(estimates),estimates[,"RP1_var_cons"],xlab="iteration",
ylab=expression(paste("Est. of ",sigma[e0]^2)),type="l")

# 2.2 Deviance statistic and the DIC diagnostic . . . . . . . . . . . . . 28

# 2.3 Adding more predictors . . . . . . . . . . . . . . . . . . . . . . .29

indata[["boysch"]]=as.integer(indata[["schgend"]]=="boysch")
indata[["girlsch"]]=as.integer(indata[["schgend"]]=="girlsch")

## Define the model
formula=normexam~(0|cons+standlrt+girl+boysch+girlsch)+(1|cons)
levID='student'
## Choose IGLS algoritm for estimation
## Fit the model
(mymodel=runMLwiN(formula, levID, indata=indata))

## Choose MCMC algoritm for estimation
estoptions= list(EstM=1)
## Fit the model
(mymodel=runMLwiN(formula, levID, indata=indata, estoptions=estoptions))

# 2.4 Fitting school effects as fixed parameters . . . . . . . . . . . . .32

indata=cbind(indata,Untoggle(indata[["school"]],"school"))

## Define the model
tempstr=paste("+",names(indata)[14:77],collapse="")
formula=paste("normexam~(0|cons+standlrt+girl",tempstr,")+(1|cons)",sep="")
levID='student'
## Choose MCMC algoritm for estimation (IGLS will be used to obtain starting values for MCMC)
estoptions= list(EstM=1)
## Fit the model
(mymodel=runMLwiN(formula, levID, indata=indata, estoptions=estoptions))

# Chapter learning outcomes . . . . . . . . . . . . . . . . . . . . . . . 33





############################################################################
