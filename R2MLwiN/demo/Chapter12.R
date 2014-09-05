############################################################################
#     MLwiN MCMC Manual
#
# 12  Unordered Categorical Responses . . . . . . . . . . . . . . . . . .167
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


## Read bang data
data(bang)

# 12.1 Fitting a first single-level multinomial model . . . . . . . . . .169

## Define the model
## Fit the model
(mymodel <- runMLwiN(log(use4,cons,4)~(0|cons), levID="woman", D='Unordered Multinomial', estoptions=list(EstM=1), data=bang))

cat(paste("Pr(y = 1) =", round(exp(mymodel["FP"]["FP_cons_1"])/(1+exp(mymodel["FP"]["FP_cons_1"])+exp(mymodel["FP"]["FP_cons_2"])+exp(mymodel["FP"]["FP_cons_3"])),4),"\n"))
cat(paste("Pr(y = 2) =", round(exp(mymodel["FP"]["FP_cons_2"])/(1+exp(mymodel["FP"]["FP_cons_1"])+exp(mymodel["FP"]["FP_cons_2"])+exp(mymodel["FP"]["FP_cons_3"])),4),"\n"))
cat(paste("Pr(y = 3) =", round(exp(mymodel["FP"]["FP_cons_3"])/(1+exp(mymodel["FP"]["FP_cons_1"])+exp(mymodel["FP"]["FP_cons_2"])+exp(mymodel["FP"]["FP_cons_3"])),4),"\n"))

# 12.2 Adding predictor variables . . . . . . . . . . . . . . . . . . . .173

## Define the model
## Fit the model
(mymodel <- runMLwiN(log(use4,cons,4)~(0|cons+lc[None]), levID="woman", D='Unordered Multinomial', estoptions=list(EstM=1), data=bang))

cat(paste("Pr(y = 3) =", round(exp(mymodel["FP"]["FP_cons_3"])/(1+exp(mymodel["FP"]["FP_cons_1"])+exp(mymodel["FP"]["FP_cons_2"])+exp(mymodel["FP"]["FP_cons_3"])),4),"\n"))
cat(paste("Pr(y = 3) =", round(exp(mymodel["FP"]["FP_cons_3"]+mymodel["FP"]["FP_Two children_3"])/(1+exp(mymodel["FP"]["FP_cons_1"]+mymodel["FP"]["FP_Two children_1"])+
exp(mymodel["FP"]["FP_cons_2"]+mymodel["FP"]["FP_Two children_2"])+exp(mymodel["FP"]["FP_cons_3"]+mymodel["FP"]["FP_Two children_3"])),4),"\n"))

# 12.3 Interval estimates for conditional probabilities . . . . . . . . .175

chains <- mymodel["chains"]
pred1 <- exp(chains[,"FP_cons_3"])/(1+exp(chains[,"FP_cons_1"])+exp(chains[,"FP_cons_2"])+exp(chains[,"FP_cons_3"]))
summary(pred1)
sixway(pred1,"prob1")

pred2 <- exp(chains[,"FP_cons_3"]+chains[,"FP_Two children_3"])/(1+exp(chains[,"FP_cons_1"]+chains[,"FP_Two children_1"])+
exp(chains[,"FP_cons_2"]+chains[,"FP_Two children_2"])+exp(chains[,"FP_cons_3"]+chains[,"FP_Two children_3"]))
summary(pred2)
sixway(pred2,"prob1")

# 12.4 Adding district level random effects . . . . . . . . . . . . . . .177

## Define the model
#Uses IGLS
## Fit the model
(mymodel <- runMLwiN(log(use4,cons,4)~(0|cons+lc[None])+(2|cons), levID=c('district','woman'), D='Unordered Multinomial', estoptions=list(EstM=0, nonlinear=c(1,2)), data=bang))

## Uses MCMC
## Fit the model
(mymodel <- runMLwiN(log(use4,cons,4)~(0|cons+lc[None])+(2|cons), levID=c('district','woman'), D='Unordered Multinomial', estoptions=list(EstM=1, nonlinear=c(1,2)), data=bang))
sixway(mymodel["chains"][,"RP2_var_cons_1"],"sigma2v0")

RP3.cons <- matrix(,3,3)
RP3.cons[upper.tri(RP3.cons,diag=T)] <- mymodel["RP"][1:6]
RP3.cons[lower.tri(RP3.cons)] <- RP3.cons[upper.tri(RP3.cons)]
round(cov2cor(RP3.cons),3)

# Chapter learning outcomes . . . . . . . . . . . . . . . . . . . . . . .128





############################################################################
