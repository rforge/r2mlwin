############################################################################
#     MLwiN User Manual
#
# 2   Introduction to Multilevel Modelling . . . . . . . . . . . . . . . . 9
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

# 2.1 The tutorial data set . . . . . . . . . . . . . . . . . . . . . . . .9

# 2.2 Opening the worksheet and looking at the data . . . . . . . . . . . 10

data(tutorial)

summary(tutorial)

head(tutorial)



# 2.3 Comparing two groups . . . . . . . . . . . . . . . . . . . . . . . .13

tab <- cbind(tapply(tutorial$normexam, tutorial$sex, length), tapply(tutorial$normexam, tutorial$sex, mean), tapply(tutorial$normexam, tutorial$sex, sd))
tab <- rbind(tab, c(length(tutorial$normexam), mean(tutorial$normexam), sd(tutorial$normexam)))
colnames(tab) <- c("N", "Mean", "SD")
rownames(tab)[3] <- "Total"

tab

t.test(normexam~sex, data=tutorial, var.equal=TRUE)

(mymodel1 <- runMLwiN(normexam~(0|cons+sex)+(1|cons), levID=c("student"), data=tutorial))

# 2.4 Comparing more than two groups: Fixed effects models . . . . . . . .20

mean_normexam <- aggregate(normexam ~ school, mean, data=tutorial)$normexam

hist(mean_normexam)

mymodel2 <- runMLwiN(normexam~(0|cons)+(1|cons), levID=c("student"), data=tutorial)

tutorial <- cbind(tutorial,Untoggle(tutorial[["school"]],"school"))

formula <- as.formula(paste0("normexam~(0|cons+", paste0("school_", 1:64, collapse="+"), ")+(1|cons)"))

(mymodel3 <- runMLwiN(formula, levID=c("student"), data=tutorial))

aov(normexam ~ school, data=tutorial)


LR <- logLik(mymodel2) - logLik(mymodel3)
LR


tutorial <- cbind(tutorial,Untoggle(tutorial[["schgend"]],"schgend"))

formula <- as.formula(paste0("normexam~(0|cons+", paste0("school_", 1:64, collapse="+"), "+schgend_boysch+schgend_girlsch)+(1|cons)"))


(mymodel4 <- runMLwiN(formula, levID=c("student"), data=tutorial))



# 2.5 Comparing means: Random effects or multilevel model . . .  . . . . .28

(mymodel5 <- runMLwiN(normexam~(0|cons)+(1|cons)+(2|cons), levID=c("school", "student"), data=tutorial))

(mymodel6 <- runMLwiN(normexam~(0|cons+schgend_boysch+schgend_girlsch)+(1|cons)+(2|cons), levID=c("school", "student"), data=tutorial))


#     Chapter learning outcomes . . . . . . . . . . . . . . . . . . . . . 35


############################################################################
