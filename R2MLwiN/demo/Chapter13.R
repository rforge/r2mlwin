############################################################################
#     MLwiN MCMC Manual
#
# 13  Ordered Categorical Responses . . . . . . . . . . . . . . . . . . .181
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

# 13.1 A level chemistry dataset . . . . . . . . . . . . . . . . . . . . 181

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

## Read alevchem data
data(alevchem)

alevchem["gcseav"]=double2singlePrecision(alevchem["gcse_tot"]/alevchem["gcse_no"]-6)
alevchem["gcse2"]=double2singlePrecision(alevchem["gcseav"]^2)
alevchem["gcse3"]=double2singlePrecision(alevchem["gcseav"]^3)

hist(alevchem[["gcseav"]],breaks=20)

# 13.2 Normal response models . . . . . . . . . . . . . . . . . . . . . .184

## Define the model
formula=a_point ~ (0|cons)+(1|cons )
levID='pupil'
estoptions= list(EstM=1)
## Fit the model
(mymodel=runMLwiN(formula, levID, indata=alevchem, estoptions=estoptions))

## Define the model
formula=a_point ~ (0|cons+gcseav+gcse2+gcse3+gender)+(1|cons )
levID='pupil'
estoptions= list(EstM=1, resi.store=T)
## Fit the model
(mymodel=runMLwiN(formula, levID, indata=alevchem, estoptions=estoptions))

resi=mymodel["residual"]
FP=mymodel["FP"]
predCurves(mymodel, indata=alevchem, xname="gcseav", group="gender")

# 13.3 Ordered multinomial modelling . . . . . . . . . . . . . . . . . . 186

##Define the model
formula=logit(a_point,cons,A) ~ (`0s`|cons)
levID=c('pupil')
##IGLS
## Fit the model
(mymodel=runMLwiN(formula, levID, D='Ordered Multinomial', indata=alevchem))

##MCMC
estoptions= list(EstM=1)
## Fit the model
(mymodel=runMLwiN(formula, levID, D='Ordered Multinomial', indata=alevchem, estoptions=estoptions))

# 13.4 Adding predictor variables . . . . . . . . . . . . . . . . . . . .191
formula=logit(a_point,cons,A) ~ (`0s`|cons)+(`0c`|gcseav+gcse2+gcse3+gender)
levID=c('pupil')
##MCMC
estoptions= list(EstM=1)
## Fit the model
(mymodel=runMLwiN(formula, levID, D='Ordered Multinomial', indata=alevchem, estoptions=estoptions))

# 13.5 Multilevel ordered response modelling . . . . . . . . . . . . . . 192

# Note: Establishment codes on their own do not uniquely identify schools.
# Schools are instead uniquely identified by LEA code, establishment ID 
# combination. Thus, here we generated a unique school ID.
alevchem$school <- as.numeric(factor(paste0(alevchem$lea, alevchem$estab)))

formula=logit(a_point,cons,A) ~ (`0s`|cons)+(`0c`|gcseav+gcse2+gender) +( `2c` | cons)
levID=c('school','pupil')
##MCMC
estoptions= list(EstM=1)
## Fit the model
(mymodel=runMLwiN(formula, levID, D='Ordered Multinomial', indata=alevchem, estoptions=estoptions))


formula=logit(a_point,cons,A) ~ (`0s`|cons)+(`0c`|gcseav+gcse2+gender) +( `2c` | cons+gcseav )
levID=c('school','pupil')
##MCMC
estoptions= list(EstM=1)
## Fit the model
(mymodel=runMLwiN(formula, levID, D='Ordered Multinomial', indata=alevchem, estoptions=estoptions))
sixway(mymodel["chains"][,"RP2_var_cons_12345"],acf.maxlag = 300,"sigma2v6")

##Increases iterations to 50,000
estoptions= list(EstM=1,mcmcMeth=list(iterations=50000))
## Fit the model
(mymodel=runMLwiN(formula, levID, D='Ordered Multinomial', indata=alevchem, estoptions=estoptions))
sixway(mymodel["chains"][,"RP2_var_cons_12345"],acf.maxlag = 300,"sigma2v6")

# Chapter learning outcomes . . . . . . . . . . . . . . . . . . . . . . .128





############################################################################
