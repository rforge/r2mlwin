############################################################################
#     MLwiN User Manual
#
# 7   Modelling the Variance as a Function of Explanatory Variables . . . 89
#
#     Rasbash, J., Steele, F., Browne, W. J. and Goldstein, H. (2012).
#     A User’s Guide to MLwiN, v2.26. Centre for Multilevel Modelling,
#     University of Bristol.
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

# Double return HERE
# User's input if necessary

# 7.1 A level 1 variance function for two groups . . . . . . . . . . . . .89

data(tutorial)

tutorial["girl"] <- as.integer(tutorial$sex) - 1
tutorial["boy"] <- 1 - tutorial$girl

covmatrix <- matrix(, nrow = 3, ncol = 1)
covmatrix[1,1] = 1
covmatrix[2,1] = 'boy'
covmatrix[3,1] = 'girl'

(mymodel1 <- runMLwiN(normexam ~ (0|boy+girl)+(1|boy+girl), levID="student", estoptions=list(clre=covmatrix), data=tutorial))

# 7.2 Variance functions at level 2 . . . . . . . . . . . . . . . . . . . 95

(mymodel2 <- runMLwiN(normexam ~ (0|cons+standlrt)+(1|cons)+(2|cons+standlrt), levID=c("school", "student"), data=tutorial))

l2varfn <- mymodel2@RP["RP2_var_cons"] + (2*mymodel2@RP["RP2_cov_cons_standlrt"]*tutorial$standlrt) + (mymodel2@RP["RP2_var_standlrt"]*tutorial$standlrt^2)

varfndata <- as.data.frame(cbind(tutorial$standlrt, l2varfn)[order(tutorial$standlrt),])
colnames(varfndata) <- c("standlrt", "l2varfn")

plot(varfndata$standlrt, varfndata$l2varfn, type="l")

# 7.3 Further elaborating the model for the student-level variance . . . .99

(mymodel3 <- runMLwiN(normexam ~ (0|cons+standlrt)+(1|cons+standlrt)+(2|cons+standlrt), levID=c("school", "student"), data=tutorial))

l2varfn <- mymodel3@RP["RP2_var_cons"] + (2*mymodel3@RP["RP2_cov_cons_standlrt"]*tutorial$standlrt) + (mymodel3@RP["RP2_var_standlrt"]*tutorial$standlrt^2)

l1varfn <- mymodel3@RP["RP1_var_cons"] + (2*mymodel3@RP["RP1_cov_cons_standlrt"]*tutorial$standlrt) + (mymodel3@RP["RP1_var_standlrt"]*tutorial$standlrt^2)

varfndata <- as.data.frame(cbind(tutorial$standlrt, l2varfn, l1varfn)[order(tutorial$standlrt),])
colnames(varfndata) <- c("standlrt", "l2varfn", "l1varfn")

xyplot(l2varfn+l1varfn~standlrt, data=varfndata, type="l")


covmatrix <- matrix(, nrow = 3, ncol = 3)
covmatrix[1,1] = 1
covmatrix[2,1] = 'standlrt'
covmatrix[3,1] = 'standlrt'
covmatrix[1,2] = 1
covmatrix[2,2] = 'girl'
covmatrix[3,2] = 'cons'
covmatrix[1,3] = 1
covmatrix[2,3] = 'standlrt'
covmatrix[3,3] = 'girl'

(mymodel4 <- runMLwiN(normexam ~ (0|cons+standlrt+girl)+(1|cons+standlrt+girl)+(2|cons+standlrt), levID=c("school", "student"), estoptions=list(clre=covmatrix), data=tutorial))

covmatrix <- matrix(, nrow = 3, ncol = 2)
covmatrix[1,1] = 1
covmatrix[2,1] = 'standlrt'
covmatrix[3,1] = 'standlrt'
covmatrix[1,2] = 1
covmatrix[2,2] = 'girl'
covmatrix[3,2] = 'cons'

(mymodel5 <- runMLwiN(normexam ~ (0|cons+standlrt+girl)+(1|cons+standlrt+girl)+(2|cons+standlrt), levID=c("school", "student"), estoptions=list(clre=covmatrix), data=tutorial))

l2varfn <- mymodel5@RP["RP2_var_cons"] + (2*mymodel5@RP["RP2_cov_cons_standlrt"]*tutorial$standlrt) + (mymodel5@RP["RP2_var_standlrt"]*tutorial$standlrt^2)

l1varfnboys <- mymodel5@RP["RP1_var_cons"] + (2*mymodel5@RP["RP1_cov_cons_standlrt"]*tutorial$standlrt)

l1varfngirls <- mymodel5@RP["RP1_var_cons"] + (2*mymodel5@RP["RP1_cov_cons_standlrt"]*tutorial$standlrt) + (2*mymodel5@RP["RP1_cov_standlrt_girl"]*tutorial$standlrt) + mymodel5@RP["RP1_var_girl"]

varfndata <- as.data.frame(cbind(tutorial$standlrt, l2varfn, l1varfnboys, l1varfngirls)[order(tutorial$standlrt),])
colnames(varfndata) <- c("standlrt", "l2varfn", "l1varfnboys", "l1varfngirls")

xyplot(l2varfn+l1varfnboys+l1varfngirls~standlrt, data=varfndata, type="l")

#     Chapter learning outcomes . . . . . . . . . . . . . . . . . . . . .106

############################################################################
