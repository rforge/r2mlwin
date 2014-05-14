############################################################################
#     MLwiN MCMC Manual
#
# 14  Adjusting for Measurement Errors in Predictor Variables . . . . . .199
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

# User's input if necessary

## Read tutorial data
data(tutorial)

# 14.1 Effects of measurement error on predictors . . . . . . . . . . . .200
set.seed(1)
error=double2singlePrecision(rnorm(length(tutorial[,"standlrt"]),0,sqrt(.2)))
obslrt=double2singlePrecision(tutorial[,"standlrt"]+error)
tutorial=cbind(tutorial,error,obslrt)

formula=normexam~(0|cons+standlrt)+(1|cons)
levID='student'
(mymodel=runMLwiN(formula, levID, indata=tutorial))


formula=normexam~(0|cons+error)+(1|cons)
levID='student'
(mymodel=runMLwiN(formula, levID, indata=tutorial))

formula=normexam~(0|cons+obslrt)+(1|cons)
levID='student'
(mymodel=runMLwiN(formula, levID, indata=tutorial))
estoptions= list(EstM=1,merr=c(N=1,"obslrt",.2))
(mymodel=runMLwiN(formula, levID, indata=tutorial, estoptions=estoptions))

# 14.2 Measurement error modelling in multilevel models . . . . . . . . .205

formula=normexam~(0|cons+standlrt)+(2|cons+standlrt)+(1|cons)
levID=c('school','student')
estoptions= list(EstM=1)
(mymodel=runMLwiN(formula, levID, indata=tutorial, estoptions=estoptions))

formula=normexam~(0|cons+obslrt)+(2|cons+obslrt)+(1|cons)
levID=c('school','student')
estoptions= list(EstM=1)
(mymodel=runMLwiN(formula, levID, indata=tutorial, estoptions=estoptions))

formula=normexam~(0|cons+obslrt)+(2|cons+obslrt)+(1|cons)
levID=c('school','student')
estoptions= list(EstM=1,merr=c(N=1,"obslrt",.2))
(mymodel=runMLwiN(formula, levID, indata=tutorial, estoptions=estoptions))

# 14.3 Measurement errors in binomial models . . . . . . . . . . . . . . 208


## Read bang1 data
data(bang1)

bang1[["denomb"]] <- bang1[["cons"]]
bang1[["use"]] <- as.integer(bang1[["use"]]) - 1

set.seed(1)
obsage=double2singlePrecision(bang1[["age"]]+rnorm(length(bang1[["age"]]),0,5))
bang1=cbind(bang1,obsage)

formula=logit(use,denomb)~(0|cons+age)
levID=c('district','woman')
estoptions= list(EstM=1)
(mymodel=runMLwiN(formula, levID, D="Binomial", indata=bang1, estoptions=estoptions))

formula=logit(use,denomb)~(0|cons+obsage)
levID=c('district','woman')
estoptions= list(EstM=1)
(mymodel=runMLwiN(formula, levID, D="Binomial", indata=bang1, estoptions=estoptions))
## Adjust for the measurement errors
estoptions= list(EstM=1,merr=c(N=1,"obsage",25))
(mymodel=runMLwiN(formula, levID, D="Binomial", indata=bang1, estoptions=estoptions))

# 14.4 Measurement errors in more than one variable and
#      misclassifications . . . . . . . . . . . . . . . . . . . . . . . .211

# Chapter learning outcomes . . . . . . . . . . . . . . . . . . . . . . .128





############################################################################
