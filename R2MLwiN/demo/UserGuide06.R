############################################################################
#     MLwiN User Manual
#
# 6   Contextual Effects . . . . . . . . . . . . . . . . . . . . . . . . .79
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

data(tutorial)

(mymodel1 <- runMLwiN(normexam~(0|cons+standlrt)+(1|cons)+(2|cons+standlrt), levID=c("school", "student"), data=tutorial))

# 6.1 The impact of school gender on girls' achievement . . . . . . . . . 80

tutorial <- cbind(tutorial,Untoggle(tutorial[["schgend"]],"schgend"))

(mymodel2 <- runMLwiN(normexam~(0|cons+standlrt+sex+schgend_boysch+schgend_girlsch)+(1|cons)+(2|cons+standlrt), levID=c("school", "student"), data=tutorial))

tutorial["boyschXstandlrt"] <- tutorial$schgend_boysch * tutorial$standlrt
tutorial["girlschXstandlrt"] <- tutorial$schgend_girlsch * tutorial$standlrt

(mymodel3 <- runMLwiN(normexam~(0|cons+standlrt+sex+schgend_boysch+schgend_girlsch+boyschXstandlrt+girlschXstandlrt)+(1|cons)+(2|cons+standlrt), levID=c("school", "student"), estoptions=list(startval=list(FP.b=mymodel2@FP, FP.v=mymodel2@FP.cov, RP.b=mymodel2@RP, RP.v=mymodel2@RP.cov)), data=tutorial))


# 6.2 Contextual effects of school intake ability averages . . . . . . . .83

tutorial["mid"] <- as.integer(as.integer(tutorial$schav) == 2)
tutorial["high"] <- as.integer(as.integer(tutorial$schav) == 3)

(mymodel4 <- runMLwiN(normexam~(0|cons+standlrt+sex+schgend_boysch+schgend_girlsch+mid+high)+(1|cons)+(2|cons+standlrt), levID=c("school", "student"), data=tutorial))

tutorial["standlrtXmid"] <- tutorial$standlrt*tutorial$mid
tutorial["standlrtXhigh"] <- tutorial$standlrt*tutorial$high

(mymodel5 <- runMLwiN(normexam~(0|cons+standlrt+sex+schgend_boysch+schgend_girlsch+mid+high+standlrtXmid+standlrtXhigh)+(1|cons)+(2|cons+standlrt), levID=c("school", "student"), data=tutorial))

hilovars = c("high", "standlrtXhigh")
hilopars = c("FP_high", "FP_standlrtXhigh")

hilodiff <- as.matrix(tutorial[,hilovars]) %*% as.matrix(mymodel5@FP[hilopars])
hilodiff_se <- sqrt(diag(as.matrix(tutorial[,hilovars]) %*% mymodel5@FP.cov[hilopars, hilopars] %*% t(as.matrix(tutorial[,hilovars]))))

hilodiff_lo <- hilodiff - 1.96*hilodiff_se
hilodiff_hi <- hilodiff + 1.96*hilodiff_se

highdata <- as.data.frame(cbind(tutorial$high, tutorial$standlrtXhigh, hilodiff, hilodiff_lo, hilodiff_hi)[order(tutorial$standlrtXhigh), ])
colnames(highdata) <- c("high", "standlrtXhigh", "hilodiff", "hilodiff_lo", "hilodiff_hi")
highdata <- highdata[highdata$high==1, ]

plot(highdata$standlrtXhigh, highdata$hilodiff, type="l")

xyplot(hilodiff~standlrtXhigh,
  panel=function(x, y, subscripts){
	panel.xyplot(x, y, type="l")
	panel.xyplot(x, highdata$hilodiff_hi, type="l", lty=2)
	panel.xyplot(x, highdata$hilodiff_lo, type="l", lty=2)
  },
  data=highdata
)

#     Chapter learning outcomes . . . . . . . . . . . . . . . . . . . . . 87



############################################################################
