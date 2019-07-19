## 
library(CCA)
library(Morpho)
require(latex2exp)
## 
asf <- as.matrix(t(scale(PK1)))%*%as.matrix(scale(PK1))/1720
svd(asf)$d
length1 = 500
hsu1 <- matrix(NA,nrow=length1)
hsu2 <- matrix(NA,nrow=length1)
hsu3 <- matrix(NA,nrow=length1)
hsu4 <- matrix(NA,nrow=length1)
hsu5 <- matrix(NA,nrow=length1)
i <- 1
for ( i in 1:length1){
  idx <- sample(1:dim(PK1)[1],round(dim(PK1)[1]/2))
  X1 <- PK1[idx,]
  X2 <- PK1[-idx,]
  
  Y1 <- BG1[idx,]
  Y2 <- BG1[-idx,]
  ################### FIRST ######################################################
  Cxx <- var(X1, na.rm = TRUE, use = "pairwise") + diag(0.00155,ncol(PK1))
  Cyy <- var(Y1, na.rm = TRUE, use = "pairwise") + diag(0.0154,ncol(BG1))
  Cxy <- cov(X1, Y1, use = "pairwise")
  ## Do SVD
  Bfac <- chol(Cxx)
  Cfac <- chol(Cyy)
  Bfacinv <- solve(Bfac)
  Cfacinv <- solve(Cfac)
  Dmat <- t(Bfacinv) %*% Cxy %*% Cfacinv
  
  result1 <- svd2(Dmat)
  xcoef1 <- Bfacinv %*% result1$u
  ################ SECOND ###############################################################
  Cxx <- var(X2, na.rm = TRUE, use = "pairwise") + diag(0.00155,ncol(PK1))
  Cyy <- var(Y2, na.rm = TRUE, use = "pairwise") + diag(0.0154,ncol(BG1))
  Cxy <- cov(X2, Y2, use = "pairwise")
  ## Do SVD
  Bfac <- chol(Cxx)
  Cfac <- chol(Cyy)
  Bfacinv <- solve(Bfac)
  Cfacinv <- solve(Cfac)
  Dmat <- t(Bfacinv) %*% Cxy %*% Cfacinv
  
  result2 <- svd2(Dmat)
  xcoef2 <- Bfacinv %*% result2$u
  
  #rcc_obj1 <- rcc(X1,Y1,lambda1 = 0.00155,lambda2 = 0.0154)
  #rcc_obj2 <- rcc(X2,Y2,lambda1 = 0.00155,lambda2 = 0.0154)
  #hsu1[i] <- abs(cos(angle.calc(rcc_obj1$xcoef[,1],rcc_obj2$xcoef[,1])))
  #hsu2[i] <- abs(cos(angle.calc(rcc_obj1$xcoef[,2],rcc_obj2$xcoef[,2])))
  #hsu3[i] <- abs(cos(angle.calc(rcc_obj1$xcoef[,3],rcc_obj2$xcoef[,3])))
  #hsu4[i] <- abs(cos(angle.calc(rcc_obj1$xcoef[,4],rcc_obj2$xcoef[,4])))
  
  #hsu1[i] <- abs(cos(angle.calc(xcoef1[,1],xcoef2[,1])))
  #hsu2[i] <- abs(cos(angle.calc(xcoef1[,2],xcoef2[,2])))
  #hsu3[i] <- abs(cos(angle.calc(xcoef1[,3],xcoef2[,3])))
  #hsu4[i] <- abs(cos(angle.calc(xcoef1[,4],xcoef2[,4])))
  
  
  hsu1[i] <- abs(cos(angle.calc(result1$v[,1],result2$v[,1])))
  hsu2[i] <- abs(cos(angle.calc(result1$v[,2],result2$v[,2])))
  hsu3[i] <- abs(cos(angle.calc(result1$v[,3],result2$v[,3])))
  hsu4[i] <- abs(cos(angle.calc(result1$v[,4],result2$v[,4])))
  hsu5[i] <- abs(cos(angle.calc(result1$v[,5],result2$v[,5])))
  
  
  
  
  
  print(i)
  
}

jh <- data.frame(CC1 = hsu1,CC2 = hsu2,CC3 = hsu3,CC4 = hsu4,CC5 = hsu5)

jhh <- jh %>% gather(CC,Value,CC1:CC5)

ggplot(data = jhh,aes(x = CC,y = Value)) + geom_boxplot() + theme_bw() + ggtitle("Similarity between Canonical components in split half test") + xlab("Canonical Component 1-5") + ylab("Cosince similarity")



plot(NA,xlim=c(0,500),xaxs="i",pch=20,main="Absolute cosine value of angle respective \n canonical variates" ,ylab="Cosine to angle between CVs",ylim=c(0,1),xlab="Iteration")
abline(h = mean(abs(hsu1)),lty=1)
abline(h = mean(hsu1) + sd(hsu1),lty=2)
abline(h = mean(hsu1) - sd(hsu1),lty=2)

polygon(c(0:499,rev(0:499)),c(rep(mean(abs(hsu1)) + sd(hsu1),500),rep(rev(mean(abs(hsu1)) - sd(hsu1)),500)),col=alpha("black",alpha=0.3),border=NA)

abline(h = mean(abs(hsu2)),lty=1,col="red")
abline(h = mean(abs(hsu2)) + sd(hsu2),lty=2,col="red")
abline(h = mean(abs(hsu2)) - sd(hsu2),lty=2,col="red")

polygon(c(0:499,rev(0:499)),c(rep(mean(abs(hsu2)) + sd(hsu2),500),rep(rev(mean(abs(hsu2)) - sd(hsu2)),500)),col=alpha("red",alpha=0.3),border=NA)


abline(h = mean(abs(hsu3)),lty=1,col="blue")
abline(h = mean(abs(hsu3)) + sd(hsu3)*0.5,lty=2,col="blue")
abline(h = mean(abs(hsu3)) - sd(hsu3)*0.5,lty=2,col="blue")

polygon(c(0:499,rev(0:499)),c(rep(mean(abs(hsu3)) + sd(hsu3)*0.5,500),rep(rev(mean(abs(hsu3)) - sd(hsu3)*0.5),500)),col=alpha("blue",alpha=0.3),border=NA)


abline(h = mean(abs(hsu4)),lty=1,col="orange")
abline(h = mean(abs(hsu4)) + sd(hsu4),lty=2,col="orange")
abline(h = mean(abs(hsu4)) - sd(hsu4),lty=2,col="orange")

polygon(c(0:499,rev(0:499)),c(rep(mean(abs(hsu4)) + sd(hsu4),500),rep(rev(mean(abs(hsu4)) - sd(hsu4)),500)),col=alpha("orange",alpha=0.3),border=NA)

legend(380,0.4,legend=c(TeX("$\\mu_1 \\pm \\sigma_1"),TeX("$\\mu_2 \\pm \\sigma_2"),TeX("$\\mu_3 \\pm \\sigma_3"),TeX("$\\mu_4 \\pm \\sigma_4")),lty=1,col=c("black","red","blue","orange"),cex=0.7,bty = "o",)



































####################### ============================================================================================================

## 
library(CCA)
library(Morpho)
require(latex2exp)
## 
asf <- as.matrix(t(scale(PK1)))%*%as.matrix(scale(PK1))/1720
svd(asf)$d
length1 = 500
hsu1 <- matrix(NA,nrow=length1)
hsu2 <- matrix(NA,nrow=length1)
hsu3 <- matrix(NA,nrow=length1)
hsu4 <- matrix(NA,nrow=length1)
hsu5 <- matrix(NA,nrow=length1)

for ( i in 1:length1){
  idx <- sample(1:dim(PK1)[1],round(dim(PK1)[1]/2))
  X1 <- PK1[idx,]
  X2 <- PK1[-idx,]
  
  Y1 <- BG1[idx,]
  Y2 <- BG1[-idx,]
  ################### FIRST ######################################################
  Cxx <- var(X1, na.rm = TRUE, use = "pairwise") + diag(0.00155,ncol(PK1))
  Cyy <- var(Y1, na.rm = TRUE, use = "pairwise") + diag(0.0154,ncol(BG1))
  Cxy <- cov(X1, Y1, use = "pairwise")
  ## Do SVD
  Bfac <- chol(Cxx)
  Cfac <- chol(Cyy)
  Bfacinv <- solve(Bfac)
  Cfacinv <- solve(Cfac)
  Dmat <- t(Bfacinv) %*% Cxy %*% Cfacinv
  
  result1 <- svd2(Dmat)
  xcoef1 <- Bfacinv %*% result1$u
  
  ################ SECOND ###############################################################
  Cxx <- var(X2, na.rm = TRUE, use = "pairwise") + diag(0.00155,ncol(PK1))
  Cyy <- var(Y2, na.rm = TRUE, use = "pairwise") + diag(0.0154,ncol(BG1))
  Cxy <- cov(X2, Y2, use = "pairwise")
  ## Do SVD
  Bfac <- chol(Cxx)
  Cfac <- chol(Cyy)
  Bfacinv <- solve(Bfac)
  Cfacinv <- solve(Cfac)
  Dmat <- t(Bfacinv) %*% Cxy %*% Cfacinv
  
  result2 <- svd2(Dmat)
  xcoef2 <- Bfacinv %*% result2$u
  
  #rcc_obj1 <- rcc(X1,Y1,lambda1 = 0.00155,lambda2 = 0.0154)
  #rcc_obj2 <- rcc(X2,Y2,lambda1 = 0.00155,lambda2 = 0.0154)
  #hsu1[i] <- abs(cos(angle.calc(rcc_obj1$xcoef[,1],rcc_obj2$xcoef[,1])))
  #hsu2[i] <- abs(cos(angle.calc(rcc_obj1$xcoef[,2],rcc_obj2$xcoef[,2])))
  #hsu3[i] <- abs(cos(angle.calc(rcc_obj1$xcoef[,3],rcc_obj2$xcoef[,3])))
  #hsu4[i] <- abs(cos(angle.calc(rcc_obj1$xcoef[,4],rcc_obj2$xcoef[,4])))
  
  #hsu1[i] <- abs(cos(angle.calc(xcoef1[,1],xcoef2[,1])))
  #hsu2[i] <- abs(cos(angle.calc(xcoef1[,2],xcoef2[,2])))
  #hsu3[i] <- abs(cos(angle.calc(xcoef1[,3],xcoef2[,3])))
  #hsu4[i] <- abs(cos(angle.calc(xcoef1[,4],xcoef2[,4])))
  
  
  hsu1[i] <- abs(cos(angle.calc(result1$u[,1],result2$u[,1])))
  hsu2[i] <- abs(cos(angle.calc(result1$u[,2],result2$u[,2])))
  hsu3[i] <- abs(cos(angle.calc(result1$u[,3],result2$u[,3])))
  hsu4[i] <- abs(cos(angle.calc(result1$u[,4],result2$u[,4])))
  hsu5[i] <- abs(cos(angle.calc(result1$u[,5],result2$u[,5])))
  
  
  
  
  
  print(i)
  
}

jh <- data.frame(CC1 = hsu1,CC2 = hsu2,CC3 = hsu3,CC4 = hsu4,CC5 = hsu5)

jhh <- jh %>% gather(CC,Value,CC1:CC5)

ggplot(data = jhh,aes(x = CC,y = Value)) + geom_boxplot() + theme_bw() + ggtitle("Similarity between Canonical components in split half test") + xlab("Canonical Component 1-5") + ylab("Cosince similarity")


angle.calc(result1$u[,1],result2$u[,1])
