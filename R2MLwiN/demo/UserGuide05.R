############################################################################
#     MLwiN User Manual
#
# 5   Graphical Procedures for Exploring the Model . . . . . . . . . . . .65
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

# 5.1 Displaying multiple graphs . . . . . . . . . . . . . . . . . . . . .65

data(tutorial)

(mymodel1 <- runMLwiN(normexam~(0|cons+standlrt)+(1|cons)+(2|cons), levID=c("school", "student"), estoptions=list(resi.store=TRUE), data=tutorial))

u0 = na.omit(mymodel1@residual$lev_2_resi_est_cons)
u0se <- sqrt(na.omit(mymodel1@residual$lev_2_resi_var_cons))

u0rank <- rank(u0)
u0rankhi <- u0+u0se
u0ranklo <- u0-u0se
u0rankno <- order(u0rank)

plot(1:65, u0[u0rankno], ylim=c(-1,1), pch=15, xlab="Rank",ylab="u0 residual estimate")
points(1:65, u0rankhi[u0rankno], pch=24,bg="grey")
points(1:65, u0ranklo[u0rankno], pch=25,bg="grey")
for(i in 1:65) {lines(rep(i,2), c(u0ranklo[u0rankno[i]],u0rankhi[u0rankno[i]]))}

plot(tutorial$standlrt, tutorial$normexam, asp=1)

# 5.2 Highlighting in graphs . . . . . . . . . . . . . . . . . . . . . . .68

xb <- as.matrix(tutorial[,c("cons", "standlrt")]) %*% as.matrix(mymodel1@FP)

xbu <- xb + u0[tutorial$school]

pred <- as.data.frame(cbind(tutorial$school, tutorial$standlrt, xb, xbu)[order(tutorial$school, tutorial$standlrt), ])

colnames(pred) <- c("school", "standlrt", "xb", "xbu")

xyplot(xbu~standlrt, type="l", group=school, data=pred)

xyplot(xbu~standlrt,
  panel=function(x, y, subscripts){
	panel.xyplot(x, y, type="l", groups=pred$school, subscripts=subscripts)
	panel.xyplot(pred$standlrt, pred$xb, type="l", lwd=3, color="black")
  },
  data=pred
)

c(unique(tutorial$school)[u0rank==65], u0rank[u0rank==65])

sch53 <- which(levels(as.factor(tutorial$school))==53)

plot(1:65, u0[u0rankno], ylim=c(-1,1), pch=15, xlab="Rank",ylab="u0 residual estimate")
points(1:65, u0rankhi[u0rankno], pch=24,bg="grey")
points(1:65, u0ranklo[u0rankno], pch=25,bg="grey")
for(i in 1:65) {lines(rep(i,2), c(u0ranklo[u0rankno[i]],u0rankhi[u0rankno[i]]))}
points(x=which(u0rankno==sch53),y=u0[u0rankno[which(u0rankno==sch53)]],pch=22,bg=2)
legend(5, 1, "School 53", pch=22, pt.bg=2, col=2)

plot(tutorial$standlrt, xbu, type="n")
for (i in 1:65) {
  lines(tutorial$standlrt[tutorial$school==i], xbu[tutorial$school==i], col="blue")
}
lines(tutorial$standlrt, xb, col=1, lwd=3)
lines(tutorial$standlrt[tutorial$school==53], xbu[tutorial$school==53], col="red")
legend(-3, 2, "School 53", lty=1, col="red")

plot(tutorial$standlrt, tutorial$normexam, type="p")
points(tutorial$standlrt[tutorial$school==53], tutorial$normexam[tutorial$school==53], col="red")
legend(-3, 3, "School 53", lty=1, col="red")

schid <- as.vector(by(tutorial$school, tutorial$school, function(x) x[1]))
schsize <- as.vector(by(tutorial$school, tutorial$school, length))

cbind(schid, schsize, u0rank)[u0rank >= 28 & u0rank <= 32,]

sch48 <- which(levels(as.factor(tutorial$school))==48)

plot(1:65, u0[u0rankno], ylim=c(-1,1), pch=15, xlab="Rank",ylab="u0 residual estimate")
points(1:65, u0rankhi[u0rankno], pch=24,bg="grey")
points(1:65, u0ranklo[u0rankno], pch=25,bg="grey")
for(i in 1:65) {lines(rep(i,2), c(u0ranklo[u0rankno[i]],u0rankhi[u0rankno[i]]))}
points(x=which(u0rankno==sch53),y=u0[u0rankno[which(u0rankno==sch53)]],pch=22,bg=2)
points(x=which(u0rankno==sch48),y=u0[u0rankno[which(u0rankno==sch48)]],pch=22,bg=3)
legend(5, 1, c("School 53", "School 48"), pch=22, pt.bg=c(2,3), col=c(2,3))

plot(tutorial$standlrt, xbu, type="n")
for (i in 1:65) {
  lines(tutorial$standlrt[tutorial$school==i], xbu[tutorial$school==i], col="blue")
}
lines(tutorial$standlrt, xb, col=1, lwd=3)
lines(tutorial$standlrt[tutorial$school==53], xbu[tutorial$school==53], col="red")
lines(tutorial$standlrt[tutorial$school==48], xbu[tutorial$school==48], col="green")
legend(-3, 2, c("The average school", "School 53", "School 48"), lty=1, col=c("black", "red", "green"))

plot(tutorial$standlrt, tutorial$normexam, type="p")
points(tutorial$standlrt[tutorial$school==53], tutorial$normexam[tutorial$school==53], col="red")
points(tutorial$standlrt[tutorial$school==48], tutorial$normexam[tutorial$school==48], col="green")
legend(-3, 3, c("School 53", "School 48"), lty=1, col=c("red", "green"))

cbind(schid, u0rank)[u0rank == 1,]

sch59 <- which(levels(as.factor(tutorial$school))==59)

plot(1:65, u0[u0rankno], ylim=c(-1,1), pch=15, xlab="Rank",ylab="u0 residual estimate")
points(1:65, u0rankhi[u0rankno], pch=24,bg="grey")
points(1:65, u0ranklo[u0rankno], pch=25,bg="grey")
for(i in 1:65) {lines(rep(i,2), c(u0ranklo[u0rankno[i]],u0rankhi[u0rankno[i]]))}
points(x=which(u0rankno==sch53),y=u0[u0rankno[which(u0rankno==sch53)]],pch=22,bg=2)
points(x=which(u0rankno==sch59),y=u0[u0rankno[which(u0rankno==sch59)]],pch=22,bg=3)
legend(5, 1, c("School 53", "School 59"), pch=22, pt.bg=c(2,3), col=c(2,3))

plot(tutorial$standlrt, xbu, type="n")
for (i in 1:65) {
  lines(tutorial$standlrt[tutorial$school==i], xbu[tutorial$school==i], col="blue")
}
lines(tutorial$standlrt, xb, col=1, lwd=3)
lines(tutorial$standlrt[tutorial$school==53], xbu[tutorial$school==53], col="red")
lines(tutorial$standlrt[tutorial$school==59], xbu[tutorial$school==59], col="green")
legend(-3, 2, c("The average school", "School 53", "School 59"), lty=1, col=c("black", "red", "green"))

plot(tutorial$standlrt, tutorial$normexam, type="p")
points(tutorial$standlrt[tutorial$school==53], tutorial$normexam[tutorial$school==53], col="red")
points(tutorial$standlrt[tutorial$school==59], tutorial$normexam[tutorial$school==59], col="green")
legend(-3, 3, c("School 53", "School 59"), lty=1, col=c("red", "green"))

(mymodel2 <- runMLwiN(normexam~(0|cons+standlrt)+(1|cons)+(2|cons+standlrt), levID=c("school", "student"), estoptions=list(resi.store=TRUE, resioptions=c("estimates", "sampling")), data=tutorial))

xb <- as.matrix(tutorial[,c("cons", "standlrt")]) %*% as.matrix(mymodel2@FP)

u0 <- na.omit(mymodel2@residual$lev_2_resi_est_cons)
u1 <- na.omit(mymodel2@residual$lev_2_resi_est_standlrt)

xbu <- xb + u0[tutorial$school] + u1[tutorial$school] * tutorial$standlrt

plot(u1, u0, xlab="Slope", ylab="Intercept")
points(u1[schid==53], u0[schid==53], col="red")
points(u1[schid==59], u0[schid==59], col="green")
legend(0.2, -0.5, c("School 53", "School 59"), pch=22, pt.bg=c("red","green"), col=c("red", "green"))

plot(tutorial$standlrt, xbu, type="n")
for (i in 1:65) {
  lines(tutorial$standlrt[tutorial$school==i], xbu[tutorial$school==i], col="blue")
}
lines(tutorial$standlrt, xb, col=1, lwd=3)
lines(tutorial$standlrt[tutorial$school==53], xbu[tutorial$school==53], col="red")
lines(tutorial$standlrt[tutorial$school==59], xbu[tutorial$school==59], col="green")
legend(-3, 2, c("School 53", "School 59"), lty=1, col=c("red", "green"))

plot(tutorial$standlrt, tutorial$normexam, type="p")
points(tutorial$standlrt[tutorial$school==53], tutorial$normexam[tutorial$school==53], col="red")
points(tutorial$standlrt[tutorial$school==59], tutorial$normexam[tutorial$school==59], col="green")
legend(-3, 3, c("School 53", "School 59"), lty=1, col=c("red", "green"))

u0var <- na.omit(mymodel2@residual$lev_2_resi_var_cons)
u0u1cov <- na.omit(mymodel2@residual$lev_2_resi_cov_standlrt_cons)
u1var <- na.omit(mymodel2@residual$lev_2_resi_var_standlrt)

xbu_lo <- xbu - 1.96*sqrt(u0var[tutorial$school] + 2*u0u1cov[tutorial$school]*tutorial$standlrt + u1var[tutorial$school]*tutorial$standlrt^2)
xbu_hi <- xbu + 1.96*sqrt(u0var[tutorial$school] + 2*u0u1cov[tutorial$school]*tutorial$standlrt + u1var[tutorial$school]*tutorial$standlrt^2)

plotdata <- as.data.frame(cbind(tutorial$standlrt, tutorial$school, xb, xbu, xbu_lo, xbu_hi)[order(tutorial$standlrt),])
colnames(plotdata) <- c("standlrt", "school", "xb", "xbu", "xbu_lo", "xbu_hi")

plot(plotdata$standlrt, plotdata$xb, xlim=c(-4, 3), ylim=c(-2.5, 3), type="l")
lines(plotdata$standlrt[plotdata$school==53], plotdata$xbu[plotdata$school==53], col="red")
lines(plotdata$standlrt[plotdata$school==53], plotdata$xbu_lo[plotdata$school==53], lty=3, col="red")
lines(plotdata$standlrt[plotdata$school==53], plotdata$xbu_hi[plotdata$school==53], lty=3, col="red")
lines(plotdata$standlrt[plotdata$school==59], plotdata$xbu[plotdata$school==59], col="green")
lines(plotdata$standlrt[plotdata$school==59], plotdata$xbu_lo[plotdata$school==59], lty=3, col="green")
lines(plotdata$standlrt[plotdata$school==59], plotdata$xbu_hi[plotdata$school==59], lty=3, col="green")
legend(-4, 3, c("School 53", "School 59"), lty=1, col=c("red", "green"))

#     Chapter learning outcomes . . . . . . . . . . . . . . . . . . . . . 77

############################################################################
