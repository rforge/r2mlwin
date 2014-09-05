############################################################################
#     MLwiN User Manual
#
# 12  Modelling Count Data . . . . . . . . . . . . . . . . . . . . . . . 181
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

# 12.1 Introduction . . . . . . . . . . . . . . . . . . . . . . . . . . .181

data(mmmec)
summary(mmmec)

# 12.2 Fitting a simple Poisson model . . . . . . . . . . . . . . . . . .182

mmmec$logexp = log(mmmec$exp)

(mymodel1 <- runMLwiN(log(obs, logexp)~(0|cons+uvbi), levID="county", D="Poisson", data=mmmec))

# 12.3 A three-level analysis . . . . . . . . . . . . . . . . . . . . . .184

(mymodel2 <- runMLwiN(log(obs, logexp)~(0|cons)+(2|cons)+(3|cons), levID=c("nation", "region", "county"), D="Poisson", estoptions=list(Meth=0), data=mmmec))

(mymodel3 <- runMLwiN(log(obs, logexp)~(0|cons)+(2|cons)+(3|cons), levID=c("nation", "region", "county"), D="Poisson", estoptions=list(Meth=0, nonlinear=c(N=1,M=2)), data=mmmec))

(mymodel4 <- runMLwiN(log(obs, logexp)~(0|cons+uvbi)+(2|cons)+(3|cons), levID=c("nation", "region", "county"), D="Poisson", estoptions=list(Meth=0, nonlinear=c(N=1,M=2)), data=mmmec))

# 12.4 A two-level model using separate country terms . . . . . . . . . .186

addmargins(with(mmmec, table(nation)))

mmmec$belgium <- as.integer(mmmec$nation == "Belgium")
mmmec$wgermany <- as.integer(mmmec$nation == "W Germany")
mmmec$denmark <- as.integer(mmmec$nation == "Denmark")
mmmec$france <- as.integer(mmmec$nation == "France")
mmmec$uk <- as.integer(mmmec$nation == "UK")
mmmec$italy <- as.integer(mmmec$nation == "Italy")
mmmec$ireland <- as.integer(mmmec$nation == "Ireland")
mmmec$luxembourg <- as.integer(mmmec$nation == "Luxembourg")
mmmec$netherlands <- as.integer(mmmec$nation == "Netherlands")

mmmec$belgiumXuvbi <- mmmec$belgium * mmmec$uvbi
mmmec$wgermanyXuvbi <- mmmec$wgermany * mmmec$uvbi
mmmec$denmarkXuvbi <- mmmec$denmark * mmmec$uvbi
mmmec$franceXuvbi <- mmmec$france * mmmec$uvbi
mmmec$ukXuvbi <- mmmec$uk * mmmec$uvbi
mmmec$italyXuvbi <- mmmec$italy * mmmec$uvbi
mmmec$irelandXuvbi <- mmmec$ireland * mmmec$uvbi
mmmec$luxembourgXuvbi <- mmmec$luxembourg * mmmec$uvbi
mmmec$netherlandsXuvbi <- mmmec$netherlands * mmmec$uvbi

(mymodel5 <- runMLwiN(log(obs, logexp)~(0|belgium+wgermany+denmark+france+uk+italy+ireland+luxembourg+netherlands+belgiumXuvbi+wgermanyXuvbi+denmarkXuvbi+franceXuvbi+ukXuvbi+italyXuvbi+irelandXuvbi+luxembourgXuvbi+netherlandsXuvbi)+(2|cons),
 levID=c("region", "county"), D="Poisson", estoptions=list(Meth=0, nonlinear=c(N=1,M=2)), data=mmmec))

xb <- as.matrix(mmmec[,c("belgium", "wgermany", "denmark", "france", "uk", "italy", "ireland", "luxembourg", "netherlands", "belgiumXuvbi", "wgermanyXuvbi", "denmarkXuvbi", "franceXuvbi", "ukXuvbi", "italyXuvbi", "irelandXuvbi", "luxembourgXuvbi", "netherlandsXuvbi")]) %*% as.matrix(mymodel5@FP)

plot(mmmec$uvbi, xb, xlab="UV B radiation",ylab="prediction", type="n")
lines(mmmec$uvbi[mmmec$belgium==1], xb[mmmec$belgium==1], col=1)
lines(mmmec$uvbi[mmmec$wgermany==1], xb[mmmec$wgermany==1], col=2)
lines(mmmec$uvbi[mmmec$denmark==1],xb[mmmec$denmark==1], col=3)
lines(mmmec$uvbi[mmmec$france==1], xb[mmmec$france==1], col=4)
lines(mmmec$uvbi[mmmec$uk==1], xb[mmmec$uk==1], col=5)
lines(mmmec$uvbi[mmmec$italy==1],xb[mmmec$italy==1],  col=6)
lines(mmmec$uvbi[mmmec$ireland==1], xb[mmmec$ireland==1], col=7)
lines(mmmec$uvbi[mmmec$luxembourg==1], xb[mmmec$luxembourg==1], col=8)
lines(mmmec$uvbi[mmmec$netherlands==1], xb[mmmec$netherlands==1], col=9)
legend(7, 0.7, c("belgium", "wgermany", "denmark", "france", "uk", "italy", "ireland", "luxembourg", "netherlands"), lty=1, col=1:9)

# 12.5 Some issues and problems for discrete response models . . . . . . 190

# Chapter learning outcomes . . . . . . . . . . . . . . . . . . . . . . .190

############################################################################
