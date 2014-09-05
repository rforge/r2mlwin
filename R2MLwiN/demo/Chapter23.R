############################################################################
#     MLwiN MCMC Manual
#
# 23  Using Orthogonal fixed effect vectors . . . . . . . . . . . . . . .357
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

# 23.1 A simple example . . . . . . . . . . . . . . . . . . . . . . . . .358

# 23.2 Constructing orthogonal vectors . . . . . . . . . . . . . . . . . 359

# 23.3 A Binomial response example . . . . . . . . . . . . . . . . . . . 360

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

## Read bang1 data
data(bang1)

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

bang1$denomb <- bang1$cons
bang1$urban <- as.integer(bang1$urban) - 1
bang1$use <- as.integer(bang1$use) - 1

## Define the model

(mymodel <- runMLwiN(logit(use,denomb)~(0|cons+age+lc[None]+urban)+(2|cons+urban), levID=c('district','woman'), D="Binomial", estoptions=list(EstM=1), data=bang1))

trajectories(mymodel["chains"])

##Orthogonal update

(mymodel <- runMLwiN(logit(use,denomb)~(0|cons+age+lc[None]+urban)+(2|cons+urban), levID=c('district','woman'), D="Binomial", estoptions=list(EstM=1, mcmcOptions=list(orth=1)), data=bang1))

trajectories(mymodel["chains"])

# 23.4 A Poisson example . . . . . . . . . . . . . . . . . . . . . . . . 364

## Read mmmec1 data
data(mmmec1)

mmmec1$logexp <- double2singlePrecision(log(mmmec1$exp))
levels(mmmec1[["nation"]]) <- c("Belgium", "W_Germany", "Denmark", "France", "UK", "Italy", "Ireland", "Luxembourg", "Netherlands")

## Define the model
## Choose option(s) for inference
## Fit the model
(mymodel <- runMLwiN(log(obs,logexp)~(0|nation[]+Belgium:uvbi+W_Germany:uvbi+Denmark:uvbi+France:uvbi+UK:uvbi+Italy:uvbi+Ireland:uvbi+Luxembourg:uvbi+Netherlands:uvbi)+(2|cons),
 levID=c('region','county'), D="Poisson", estoptions=list(EstM=1,mcmcMeth=list(iterations=50000)), data=mmmec1))

sixway(mymodel["chains"][,"FP_Belgium"],acf.maxlag=5000,"beta_1")

##Orthogonal update

(mymodel <- runMLwiN(log(obs,logexp)~(0|nation[]+Belgium:uvbi+W_Germany:uvbi+Denmark:uvbi+France:uvbi+UK:uvbi+Italy:uvbi+Ireland:uvbi+Luxembourg:uvbi+Netherlands:uvbi)+(2|cons),
 levID=c('region','county'), D="Poisson", estoptions=list(EstM=1, mcmcMeth=list(iterations=50000), mcmcOptions=list(orth=1)), data=mmmec1))

sixway(mymodel["chains"][,"FP_Belgium"],acf.maxlag=100,"beta_1")

# 23.5 An Ordered multinomial example . . . . . . . . . . . . . . . . . .368

## Read alevchem data
data(alevchem)

# Note: Establishment codes on their own do not uniquely identify schools.
# Schools are instead uniquely identified by LEA code, establishment ID 
# combination. Thus, here we generated a unique school ID.
alevchem$school <- as.numeric(factor(paste0(alevchem$lea, alevchem$estab)))

alevchem$gcseav <- double2singlePrecision(alevchem$gcse_tot/alevchem$gcse_no-6)
alevchem$gcse2 <- double2singlePrecision(alevchem$gcseav^2)
alevchem$gcse3 <- double2singlePrecision(alevchem$gcseav^3)

##MCMC
## Fit the model
(mymodel <- runMLwiN(logit(a_point,cons,A)~(`0s`|cons)+(`0c`|gcseav+gcse2+gender)+(`2c`|cons), levID=c('school','pupil'), D='Ordered Multinomial', estoptions=list(EstM=1), data=alevchem))

trajectories(mymodel["chains"])

##Orthogonal update
## Fit the model
(mymodel <- runMLwiN(logit(a_point,cons,A)~(`0s`|cons)+(`0c`|gcseav+gcse2+gender)+(`2c`|cons), levID=c('school','pupil'), D='Ordered Multinomial', estoptions=list(EstM=1, mcmcOptions=list(orth=1)), data=alevchem))

trajectories(mymodel["chains"])

# 23.6 The WinBUGS interface . . . . . . . . . . . . . . . . . . . . . . 372

## Read bang1 data
data(bang1)

bang1$denomb <- bang1$cons
bang1$urban <- as.integer(bang1$urban) - 1
bang1$use <- as.integer(bang1$use) - 1

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
##Orthogonal update (WinBUGS)

mymodel <- runMLwiN(logit(use,denomb)~(0|cons+age+lc[nokids]+urban)+(2|cons+urban), levID=c('district','woman'), D="Binomial",
 estoptions=list(EstM=1, mcmcOptions=list(orth=1), show.file=T), BUGO=c(version=4, n.chains=1, debug=F, seed=1, bugs=openbugs, OpenBugs = T), data=bang1)

apply(mymodel[[1]],2,effectiveSize)
sixway(mymodel[[1]][,"beta[1]"],"beta[1]")

# Chapter learning outcomes . . . . . . . . . . . . . . . . . . . . . . .379





############################################################################
