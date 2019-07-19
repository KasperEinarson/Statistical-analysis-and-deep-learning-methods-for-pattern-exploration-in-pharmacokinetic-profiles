#library()
library(dplyr)
library(CCA)
library(Rtsne)
library(FactoMineR)
library(ggplot2)
library(RColorBrewer)
library(factoextra)
library(CCP)
library(corrplot)
library(caret)
library(GGally)
library(car)
library(candisc)
library(grid)  
library(gtable)  
library(psych)
#library(QuantPsyc)
# ======================================= Correlation =======================================================
# Use for later:
Times1 <- c(3,6,9,12,15,20,30,45,60,75,90,105,120,180,240,300)
PK1 <- read.table("C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/PK_HER.txt")
BG1 <- read.table("C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/BG_HER.txt")


PK.u <- read.table("C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/PK_IKKE_GJORT_NOGET.txt")
BG.u <- read.table("C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/BG_IKKE_GJORT_NOGET.txt")

#PK1 <- scale(PK.u)
#BG1 <- scale(BG.u)


colnames(PK1) <- c("3","6","9","12","15","20","30","45","60","75","90","105","120","180","240","300")
colnames(BG1) <- c("3","6","9","12","15","20","30","45","60","75","90","105","120","180","240","300")
correl <- matcor(PK1,BG1)
img.matcor(correl, type = 1)
PK_cor <- round(correl$Xcor,2)
BG_cor <- round(correl$Ycor,2)
par(mfrow=c(1,2))
corrplot(PK_cor, type = "upper", 
         tl.col = "black", tl.srt = 45)
title("Correlation in time for insulin conc.", line = 1)

corrplot(BG_cor, type = "upper", 
         tl.col = "black", tl.srt = 45)
title("Correlation in time for blood sugar conc.", line = 1)

R <- cor(as.matrix(PK1), as.matrix(BG1))
colnames(R) <- paste0(colnames(R),".BG")

rownames(R) <- paste0(rownames(R),".PK")
redwhite <- c("red", "white") 
pal1 <- colorRampPalette(redwhite)(25)
whiteblue <- c("white", "blue") 
pal2 <- colorRampPalette(whiteblue)(25)
pal <- c(pal1,pal2)

color = colorRampPalette(brewer.pal(n = 7, name = "RdYlBu"))(100)
pheatmap(R,cluster_rows=F, cluster_cols=F,main = " Cross-correlation Insulin(PK)-Bloodglucose(BG)",color=color)


## =================================================
PK_long_o <- as.data.frame(PK1) %>% gather(Time,Value) %>% mutate(Time = factor(Time,levels=colnames(PK)))
BG_long_o <- as.data.frame(BG1) %>% gather(Time,Value) %>% mutate(Time = factor(Time,levels=colnames(PK)))
PK_long_o$Time <-as.numeric(as.character(PK_long_o$Time ))
BG_long_o$Time <-as.numeric(as.character(BG_long_o$Time ))
colnames(PK_long_o) <- c("Time","Var");colnames(BG_long_o) <- c("Time","Var")
par(mfrow=c(1,2))
quantile.plot2(Plot_quantile2(PK_long_o),main="Normalized Insulin concentration",Means=NULL,obs=NULL,ymax=3,ymin=0,Times1=Times1,Ylab="Normalized concentration [pm]")
quantile.plot2(Plot_quantile2(BG_long_o),main="Normalized BG concentration",Means=NULL,obs=NULL,ymax=10,ymin=0,Times1=Times1,Ylab="Normalized concentration [mmol/L]")


PK_long <- as.data.frame(PK1) %>% gather(Time,Value) %>% mutate(Time = factor(Time,levels=colnames(PK1)))
BG_long <- as.data.frame(BG1) %>% gather(Time,Value) %>% mutate(Time = factor(Time,levels=colnames(PK1)))
PK_long$Time <-as.numeric(as.character(PK_long$Time ))
BG_long$Time <-as.numeric(as.character(BG_long$Time ))
colnames(PK_long) <- c("Time","Var");colnames(BG_long) <- c("Time","Var")
par(mfrow=c(1,2))

quantile.plot2(Plot_quantile2(PK_long),main="Normalized Insulin concentration",Means=NULL,obs=NULL,ymax=3,ymin=-2,Times1=Times1,Ylab="Normalized concentration [pm]")
quantile.plot2(Plot_quantile2(BG_long),main="Normalized BG concentration",Means=NULL,obs=NULL,ymax=3,ymin=-2,Times1=Times1,Ylab="Normalized concentration [mmol/L]")
#ggplot(PK_long,aes(sample=Value,group=Time))+ stat_qq()+stat_qq_line() + facet_wrap(~Time,scales = "free") + ggtitle("QQplot row scaling")

#ggplot(BG_long,aes(sample=Value,group=Time))+ stat_qq()+stat_qq_line() + facet_wrap(~Time,scales = "free") + ggtitle("QQplot row scaling")

H1 <- ggplot(PK_long,aes(x=Time,y=Var,group=Time)) + theme_bw() + geom_boxplot() + ggtitle("Insulin time series mean centered data") + ylab("Concentration")
H2 <- ggplot(BG_long,aes(x=Time,y=Var,group=Time)) + theme_bw() + geom_boxplot() + ggtitle("Blood glucose time series mean centered data") + ylab("Concentration")

grid.arrange(H1,H2)


##### =============== 
library(mvoutlier)






## ===================PCA ======================== 
library(mctest)
library(gplots)

PK2 <- scale(PK1,scale=F)
BG2 <- scale(PK1,scale=F)

par(mfrow=c(2,2))
PK_PCA <- PCA(PK1,scale.unit = F,graph = T,ncp = 16)
PFFA <- prcomp(PK1, scale = FALSE)
PFFB <- prcomp(BG1, scale = FALSE)

g <- summary(PFFA)
g2 <- summary(PFFB)
plot(g$importance[3,],xlab="Dimension",ylim=c(0.2,1),ylab="Sum explained variance",type="l",main = "Explained variance PCA Insulin/blood sugar")
lines(g2$importance[3,],col="red",xlab="Dimension",ylim=c(0.2,1),ylab="Sum explained variance",type="l",main = "Explained variance PCA Insulin")
points(g2$importance[3,],col="red",xlab="Dimension",ylim=c(0.2,1),pch=20,cex=0.9)
points(g$importance[3,],xlab="Dimension",ylim=c(0.2,1),pch=20,cex=0.9)

legend("bottomright",c("PK","BG"),lty=1,col=c("black","red"))

BG_PCA <- PCA(BG1,scale.unit = F,graph = T,ncp=16)
PK_PCA2 <- PCA(PK2,scale.unit = F,graph = T,ncp = 16)
BG_PCA2 <- PCA(BG2,scale.unit = F,graph = T,ncp=16)
######### OTher PCA

pca.PK <- svd(PK1)
pca.BG <- svd(BG1)


pca.PK

pca.PK.data <- data.frame(
  Time = Times1,
  PC1 = pca.PK$v[,1],
  PC2 = pca.PK$v[,2])

pca.BG.data <- data.frame(
  Time = Times1,
  PC1= pca.BG$v[,1],
  PC2= pca.BG$v[,2])


plot1 <- pca.PK.data%>%
  gather(key = 'PCs', value = 'Loading', -Time) %>%
  ggplot(mapping = aes(x = Time, y = Loading)) + ylab("Correlation (Loading)") + 
  geom_line(size = 1) +
  facet_wrap(~ PCs) +
  labs(title = 'First 2 PCA Loadings for Insulin ',
       x = '') +
  theme_bw()


plot2 <- pca.BG.data%>%
  gather(key = 'PCs', value = 'Loading', -Time) %>%
  ggplot(mapping = aes(x = Time, y = Loading)) + ylab("Correlation (Loading)") + 
  geom_line(size = 1) +
  facet_wrap(~ PCs) +
  labs(title = 'First 2 PCA Loadings for Blood Sugar',
       x = 'Time[min]') +
  theme_bw()

grid.arrange(plot1,plot2)


get_eigenvalue(PK_PCA)
get_eigenvalue(BG_PCA)

PK_PCA$var
PK_PCA2 <- PCA(PK1[,-15],scale.unit = F,graph = F,ncp = 15)
BG_PCA2 <- PCA(BG1[,-5],scale.unit = F,graph = F,ncp=15)

get_eigenvalue(PK_PCA2)
get_eigenvalue(BG_PCA2)

#ggpairs(as.data.frame(PK1))
PK11 <- PK1
BG11 <- BG1
PK11[,15] <- (PK1[,15]+PK1[,16])/2
PK11 <- PK11[,-16]
#ggpairs(as.data.frame(PK11))
PK_PCA1 <- PCA(PK11,scale.unit = F,graph = F,ncp=15)
get_eigenvalue(PK_PCA1)
PK_PCA1$svd
#plot.PCA(PK_PCA, axes=c(1, 2), choix="var", habillage="ind")
#plot.PCA(BG_PCA, axes=c(1, 2), choix="ind", habillage="ind")

### Other ways to detect multicolinearity

## ================== RCCA ===========================================================================================

#estiam <- estim.regul(PK1, BG1, grid1 = seq(0.00001,0.01,len=14), grid2 =  seq(0.00001,0.1,len=14), plt = FALSE) # l1 = 0.001 l2 = 0.01. CV 0.7474335
#estiam$lambda2
#matmat <- estiam$mat
#colnames(matmat) <- seq(0.00001,0.1,len=14)
#rownames(matmat) <- seq(0.00001,0.01,len=14)
#ggplot(data = melt(matmat),aes(x = X1,y=X2,fill=value)) + geom_tile() + theme_bw() +scale_fill_gradient(name = "CV score",low = "#FFFFFF",high = "#012345") + ggtitle("CV score regularized CCA") + xlab("r1") + ylab("r2") + geom_point(aes(x = estiam$lambda1,y = estiam$lambda2,col="Highest CV score \n r1 = 0.00155 \n r2 =  0.0154"),size=3)
                                                                                                             
par(mfrow=c(1,1))
rcc_obj <- rcc(PK1,BG1,lambda1 = 0.00155,lambda2 = 0.0154)

# tests of canonical dimensions
rho_rcc <- rcc_obj$cor
rho_data <- data.frame(x=1:16,y=rho_rcc)
## Plot these
ggplot(data = rho_data,aes(x=x,y=rho_rcc)) + geom_bar(stat = "identity") + geom_text(aes(label=round(rho_rcc,2)), vjust=-0.3, size=3.5) + theme_bw() + xlab("Canonical dimension") + ylab("Canonical correlation") + ggtitle("Canonical correlation for each dimension")
## Calculate p-values using the F-approximations of different test statistics:a
options(scipen=999)
n <- dim(PK1)[1]
p <- dim(PK1)[2]
q <- dim(BG1)[2]
## Calculate p-values using the F-approximations of different test statistics:
col1 <- round(p.asym(rho_rcc, n, p, q, tstat = "Wilks")$p.value,5)
col2 <- round(p.asym(rho_rcc, n, p, q, tstat = "Hotelling")$p.value,5)
col3 <- round(p.asym(rho_rcc, n, p, q, tstat = "Pillai")$p.value,5)
master_frame <- data.frame(Wilks = col1,Hotelling = col2,Pillai = col3)
xtable(master_frame)
############# ======= PLOT CCA
plt.cc(rcc_obj,d1 = 1,d2 = 2,var.label = T)
title("Correlation circle for Dim 1 and 2",line = 3)
legend(-1,-0.4,c("PK","BG"),col=c("red","blue"),pch=c(20,20))

#pl

# ======================= XSCORES =======================================
par(mfrow=c(1,2))
plot(1, type="n", xlab="Correlation Dim 1", ylab="Correlation Dim 2", xlim=c(-1, 1), ylim=c(-1, 1),main="Canonical Loadings")
text(rcc_obj$scores$corr.X.xscores[,1],rcc_obj$scores$corr.X.xscores[,2],labels=c("3","6","9","12","15","20","30","45","60","75","90","105","120","180","240","300"), cex= 0.7,col="red")
text(rcc_obj$scores$corr.Y.xscores[,1],rcc_obj$scores$corr.Y.xscores[,2],labels=c("3","6","9","12","15","20","30","45","60","75","90","105","120","180","240","300"), cex= 0.7,col="blue")
abline(v = 0, h = 0)
lines(cos(seq(0, 2 * pi, l = 100)), sin(seq(0, 2 * pi, l = 100)))
legend("topright",c("PK","BG"),col=c("red","blue"),pch=c(20,20),cex=0.6,pt.cex = 2)
plot(1, type="n", xlab="Score Dim 1", ylab="Score Dim 2", xlim=c(-2, 5), ylim=c(-5, 2),main="Canonical Scores")
text(rcc_obj$scores$xscores[,1],rcc_obj$scores$xscores[,2], cex= 0.7)
abline(v = 0, h = 0)


par(mfrow=c(1,2))
plot(1, type="n", xlab="Correlation Dim 2", ylab="Correlation Dim 3", xlim=c(-1, 1), ylim=c(-1, 1),main="Canonical Loadings")
text(rcc_obj$scores$corr.X.xscores[,2],rcc_obj$scores$corr.X.xscores[,3],labels=c("3","6","9","12","15","20","30","45","60","75","90","105","120","180","240","300"), cex= 0.7,col="red")
text(rcc_obj$scores$corr.Y.xscores[,2],rcc_obj$scores$corr.Y.xscores[,3],labels=c("3","6","9","12","15","20","30","45","60","75","90","105","120","180","240","300"), cex= 0.7,col="blue")
abline(v = 0, h = 0)
lines(cos(seq(0, 2 * pi, l = 100)), sin(seq(0, 2 * pi, l = 100)))
legend("topright",c("PK","BG"),col=c("red","blue"),pch=c(20,20),cex=0.6,pt.cex = 2)
plot(1, type="n", xlab="Score Dim 2", ylab="Score Dim 3", xlim=c(-5, 2), ylim=c(-4, 5),main="Canonical Scores")
text(rcc_obj$scores$xscores[,2],rcc_obj$scores$xscores[,3], cex= 0.7)
abline(v = 0, h = 0)


###### ===============YSCORES =====================================================

par(mfrow=c(1,2))
plot(1, type="n", xlab="Correlation Dim 1", ylab="Correlation Dim 2", xlim=c(-1, 1), ylim=c(-1, 1),main="Canonical Loadings")
text(rcc_obj$scores$corr.X.yscores[,1],rcc_obj$scores$corr.X.yscores[,2],labels=c("3","6","9","12","15","20","30","45","60","75","90","105","120","180","240","300"), cex= 0.7,col="red")
text(rcc_obj$scores$corr.Y.yscores[,1],rcc_obj$scores$corr.Y.yscores[,2],labels=c("3","6","9","12","15","20","30","45","60","75","90","105","120","180","240","300"), cex= 0.7,col="blue")
abline(v = 0, h = 0)
lines(cos(seq(0, 2 * pi, l = 100)), sin(seq(0, 2 * pi, l = 100)))
legend("topright",c("PK","BG"),col=c("red","blue"),pch=c(20,20),cex=0.6,pt.cex = 2)
plot(1, type="n", xlab="Score Dim 1", ylab="Score Dim 2", xlim=c(-3, 4), ylim=c(2, -5),main="Canonical Scores")
text(rcc_obj$scores$yscores[,1],rcc_obj$scores$yscores[,2], cex= 0.7)
abline(v = 0, h = 0)




par(mfrow=c(1,2))
plot(1, type="n", xlab="Correlation Dim 2", ylab="Correlation Dim 3", xlim=c(-1, 1), ylim=c(-1, 1),main="Canonical Loadings")
text(rcc_obj$scores$corr.X.yscores[,2],rcc_obj$scores$corr.X.yscores[,3],labels=c("3","6","9","12","15","20","30","45","60","75","90","105","120","180","240","300"), cex= 0.7,col="red")
text(rcc_obj$scores$corr.Y.yscores[,2],rcc_obj$scores$corr.Y.yscores[,3],labels=c("3","6","9","12","15","20","30","45","60","75","90","105","120","180","240","300"), cex= 0.7,col="blue")
abline(v = 0, h = 0)
lines(cos(seq(0, 2 * pi, l = 100)), sin(seq(0, 2 * pi, l = 100)))
legend("topright",c("PK","BG"),col=c("red","blue"),pch=c(20,20),cex=0.6,pt.cex = 2)
plot(1, type="n", xlab="Score Dim 2", ylab="Score Dim 3", xlim=c(-5, 3), ylim=c(-4, 4),main="Canonical Scores")
text(rcc_obj$scores$yscores[,2],rcc_obj$scores$yscores[,3], cex= 0.7)
abline(v = 0, h = 0)





par(mfrow=c(1,1))
plot(1, type="n", xlab="", ylab="", xlim=c(-1, 1), ylim=c(-1, 1))
text(rcc_obj$scores$corr.X.yscores[,1],rcc_obj$scores$corr.X.yscores[,2],labels=c("3","6","9","12","15","20","30","45","60","75","90","105","120","180","240","300"), cex= 0.7,col="red")
text(rcc_obj$scores$corr.Y.yscores[,1],rcc_obj$scores$corr.Y.yscores[,2],labels=c("3","6","9","12","15","20","30","45","60","75","90","105","120","180","240","300"), cex= 0.7,col="blue")
abline(v = 0, h = 0)
lines(cos(seq(0, 2 * pi, l = 100)), sin(seq(0, 2 * pi, l = 100)))

######## =============== PLOT CC VARIANCE ON ORIGINAL DATA ===============================

Data_gg_fixed <- data.frame(X = as.numeric(as.character(colnames(PK1))),
                            Y1=colMeans(PK1)-2.5*(rcc_obj$cor[1])*rcc_obj$scores$corr.X.xscores[,1],
                            Y2=colMeans(PK1)+2.5*(rcc_obj$cor[1])*rcc_obj$scores$corr.X.xscores[,1],
                            
                            mu = colMeans(PK1),
                            XBG = colMeans(BG1), 
                            BGy =  colMeans(BG1)+ 2.5*(rcc_obj$cor[1]^2)*rcc_obj$scores$corr.Y.yscores[,1],
                            BGy2 =  colMeans(BG1) -2.5*(rcc_obj$cor[1]^2)*rcc_obj$scores$corr.Y.yscores[,1],
                            Q.PK95 = apply(PK1, 2, quantile, probs=c(0.95)),
                            Q.PK05 = apply(PK1, 2, quantile, probs=c(0.05)),
                            Q.BG95 = apply(BG1, 2, quantile, probs=c(0.95)),
                            Q.BG05 = apply(BG1, 2, quantile, probs=c(0.05))

                            )


## make sure to show what canonical variate part (U or V) we have plotted on. 
dev.new(width = 7,height = 4)
plot(1,type="n",xlim=c(0,300),main = "1. canonical variate (U1) on insulin data",ylim=c(-2,3),ylab="Value",xlab="Time[min]",xaxt="n",col=rgb(0.4,0.4,0.8,0.6),pch=16,cex=1.3)
axis(1, at=Times1, las=2)
lines(Times1,Data_gg_fixed$mu)
polygon(c(rev(Times1), Times1), c(rev(Data_gg_fixed$Q.PK95), Data_gg_fixed$Q.PK05), col = rgb(0.7,0.7,0.7,0.4) , border = NA)
lines(Times1,Data_gg_fixed$Y1,lty=2,col="red")
lines(Times1,Data_gg_fixed$Y2,lty=2,col="blue")
legend("topright",legend = c("mean","mean+2.5*sd1*U1","mean-2.5*sd1*U1","90% of data"),lty=c(1,2,2,0),pch=c(NA,NA,NA,NA),col=c("black","red","blue","black"),bty="n",border="white",fill=c(0,0,0,"grey"))
dev.off()

## First 
dev.new(width = 7,height = 4)
plot(1,type="n",xlim=c(0,300),main = "1. canonical variate (V1) on blood glucose data",ylim=c(-3,3),ylab="Value",xlab="Time[min]",xaxt="n",col=rgb(0.4,0.4,0.8,0.6),pch=16,cex=1.3)
axis(1, at=Times1, las=2)
lines(Times1,Data_gg_fixed$XBG)
polygon(c(rev(Times1), Times1), c(rev(Data_gg_fixed$Q.BG95), Data_gg_fixed$Q.BG05), col = rgb(0.7,0.7,0.7,0.4) , border = NA)
lines(Times1,Data_gg_fixed$BGy,lty=2,col="red")
lines(Times1,Data_gg_fixed$BGy2,lty=2,col="blue")
legend("bottomright",legend = c("mean","mean+2.5*sd1*V1","mean-2.5*sd1*V1","90% of data"),lty=c(1,2,2,0),pch=c(NA,NA,NA,NA),col=c("black","red","blue","black"),bty="n",border="white",fill=c(0,0,0,"grey"))
dev.off()

#write.table(PK1,"C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale Python/Untitled Folder 1/PK_3.txt")
#write.table(BG1,"C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale Python/Untitled Folder 1/BG_3.txt")

# =========================== TSNE =================================================================== 
Rtsne_1 <- Rtsne(rcc_obj$scores$xscores,dims=2,perplexity = 50,verbose = T,check_duplicates = F,pca = T)

plot(rcc_obj$scores$xscores[,1],rcc_obj$scores$yscores[,1])
plot(rcc_obj$scores$yscores[,1],rcc_obj$scores$yscores[,2])

plot(rcc_obj$scores$corr.X.xscores[,1],rcc_obj$scores$corr.Y.xscores[,1])


#
#
# Data augmentation on the treatment groups (from 3 ) - right now it is down sampled. 
# 
# 


########## RCC MANUALLY ======================================================================= 
Cxx <- var(PK1, use = "pairwise") + diag(0.00155,ncol(PK1)) # Sample covariance of insulin data
Cyy <- var(BG1, use = "pairwise") + diag(0.0154,ncol(BG1))# Sample covariance of blood glucose data
Cxy <- cov(PK1, BG1, use = "pairwise") # Cross correlation.

Cxx <- var(PK1, na.rm = TRUE, use = "pairwise") + diag(0.00155,ncol(PK1))
Cyy <- var(BG1, na.rm = TRUE, use = "pairwise") + diag(0.0154,ncol(BG1))
Cxy <- cov(PK1, BG1, use = "pairwise")
## Do SVD
Bfac <- chol(Cxx)
Cfac <- chol(Cyy)
Bfacinv <- solve(Bfac)
Cfacinv <- solve(Cfac)
Dmat <- t(Bfacinv) %*% Cxy %*% Cfacinv

result <- svd2(Dmat)
values <- result$d

xcoef <- Bfacinv %*% result$u
ycoef <- Cfacinv %*% result$v

xscores = as.matrix(PK1) %*% xcoef
yscores = as.matrix(BG1) %*% ycoef



corr.X.xscores = cor(PK1, xscores, use = "pairwise")
corr.Y.xscores = cor(BG1, xscores, use = "pairwise")
corr.X.yscores = cor(PK1, yscores, use = "pairwise")
corr.Y.yscores = cor(BG1, yscores, use = "pairwise")

sum(values)



standard_xcoef <- diag(sqrt(diag(cov(PK1)))) %*% xcoef
standard_ycoef <- diag(sqrt(diag(cov(BG1)))) %*% ycoef

xscores_standard <- as.matrix(PK1) %*% standard_xcoef
yscores_standard <- as.matrix(BG1) %*% standard_ycoef


S_corr.X.xscores = cor(PK1, xscores_standard, use = "pairwise")
S_corr.Y.xscores = cor(BG1, xscores_standard, use = "pairwise")
S_corr.X.yscores = cor(PK1, yscores_standard, use = "pairwise")
S_corr.Y.yscores = cor(BG1, yscores_standard, use = "pairwise")

## This goes into the table:
data.frame(round(standard_ycoef[,4],2))
as.data.frame(round(S_corr.Y.xscores[,4],2))
#variance:
fg <- data.frame(test = S_corr.Y.xscores[,4]^2/sum(S_corr.Y.xscores[,4]^2)*100)
ffg <- data.frame(round(fg$test,2))

al <- data.frame(round(standard_ycoef[,1],2),round(standard_ycoef[,2],2),round(standard_ycoef[,3],2),round(standard_ycoef[,4],2))

al2 <- data.frame(round(S_corr.Y.yscores[,1],2),round(S_corr.Y.yscores[,2],2),round(S_corr.X.xscores[,3],2),round(S_corr.X.xscores[,4],2))





Relu <- function(x){
  idx <- which(x < 0)
  x[idx] <- 0
  return(x)
}

x <- seq(-3,3,len=3000)
sigmoid <- 1/(1+exp(-x))
etanh <- (exp(x)-exp(-x))/(exp(x)+exp(-x))
tanh <- 2/(1+exp(-2*x))-1
plot(x,sigmoid,type="l", ylab=("f(x)"),ylim=c(-1,1),lwd=2,main="Activation functions")
lines(x,tanh,col="red",lwd=2)
lines(x,Relu(x),col="blue",lwd=2)
lines(x,etanh,col="red",lwd=2)

cube_variation <- x^3/(3+x)
#lines(x,cube_variation)
grid(5,5)
legend("bottomright",c("Sigmoid","Tanh","ReLu"),col=c("black","red","blue"),lty=c(1,1,1))





#variance:
fg1 <- data.frame(test = S_corr.X.xscores[,1]^2/sum(S_corr.X.xscores[,1]^2)*100,Time = Times1,ID=1)
fg2 <- data.frame(test = S_corr.X.xscores[,2]^2/sum(S_corr.X.xscores[,2]^2)*100,Time = Times1,ID=2)


fg3 <- data.frame(test = S_corr.Y.yscores[,1]^2/sum(S_corr.Y.yscores[,1]^2)*100,Time = Times1,ID=3)
fg4 <- data.frame(test = S_corr.Y.yscores[,2]^2/sum(S_corr.Y.yscores[,1]^2)*100,Time = Times1,ID=4)

fg_total <- rbind(fg1,fg2,fg3,fg4)
fg_total$ID <- as.factor(fg_total$ID)

ggplot(data = fg1,aes(x=Time,y = test)) + geom_bar(stat ="identity") +theme_bw() + ylab("Relative varians explained") + ggtitle("1. canonical variate (U1) explained variance from orginal variables")
ggplot(data = fg3,aes(x=Time,y = test)) + geom_bar(stat ="identity") +theme_bw() + ylab("Relative varians explained") + ggtitle("1. canonical variate (V1) explained variance from orginal variables")
ggplot(data = fg2,aes(x=Time,y = test)) + geom_bar(stat ="identity") +theme_bw() + ylab("Relative varians explained") + ggtitle("2. canonical variate (U2) explained variance from orginal variables")
ggplot(data = fg4,aes(x=Time,y = test)) + geom_bar(stat ="identity") +theme_bw() + ylab("Relative varians explained") + ggtitle("2. canonical variate (V2) explained variance from orginal variables")


ffg <- data.frame(round(fg$test,2))
ffgg <- data.frame(round(S_corr.Y.yscores[,3],2))
ffgg


cor(xscores[,1],yscores[,1])
cor(xscores[,2],yscores[,2])
cor(xscores[,3],yscores[,3])
cor(xscores[,4],yscores[,4])

##
par(mfrow=c(1,1))
plot(S_corr.X.xscores[,1],type="l",col="red",cex=2,lwd=2)
lines(S_corr.Y.xscores[,1],col="blue",lwd=2)
axis(1)
plot(S_corr.X.yscores[,2],type="l",col="red",cex=2,lwd=2)
lines(S_corr.Y.yscores[,2],col="blue",lwd=2)

legend("bottomright",c("Insulin","Blood sugar"),col=c("red","blue"),lty=1)
cor(S_corr.X.xscores[,1],S_corr.Y.xscores[,1])


plot(xscores[,1],type="l")
lines(yscores[,1],col="red")

plot(xscores_standard[,1],type="l")
lines(yscores_standard[,1],col="red")

cor(xscores_standard[,1], yscores_standard[,1])
cor(xscores[,1],xscores[,2])
## GG 
oq <- data.frame(X = 1:1720,xscores = xscores_standard[,1],yscores = yscores_standard[,1])
p1 <- ggplot() + geom_smooth(data =oq ,aes(x =X,y = xscores),col="red") + geom_smooth(data = oq,aes(x = X,y = yscores+0.42),col="blue") + theme_bw()
oq2 <- data.frame(X = 1:1720,xscores = xscores_standard[,2],yscores = yscores_standard[,2])
p1

p2 <- ggplot() + geom_smooth(data =oq2 ,aes(x =X,y = xscores+1),col="red") + geom_smooth(data = oq2,aes(x = X,y = yscores),col="blue") + theme_bw()
p2
oq3 <- data.frame(X = 1:1720,xscores = xscores_standard[,3],yscores = yscores_standard[,3])
p3 <- ggplot() + geom_smooth(data =oq3 ,aes(x =X,y = xscores),col="red") + geom_smooth(data = oq3,aes(x = X,y = yscores+0.42),col="blue") + theme_bw()

grid.arrange(p1,p2,p3,ncol=1)
oq4 <- data.frame(X = 1:1720,xscores = xscores_standard[,4],yscores = yscores_standard[,4])
ggplot() + geom_smooth(data =oq4 ,aes(x =X,y = xscores)) + geom_smooth(data = oq4,aes(x = X,y = yscores))


plot(standard_xcoef[,1],type="l",ylim=c(-0.3,0.4))
lines(standard_ycoef[,1],col="red")

plot(xcoef[,1],type="l",ylim=c(-1,0.8))
lines(ycoef[,1],col="red")




##### ========== 

YY1 = colMeans(PK1)-2.5*(rcc_obj$cor[1])*S_corr.X.xscores[,1]
YY2 = colMeans(PK1)+2.5*(rcc_obj$cor[1])*S_corr.X.xscores[,1]
YY11 = colMeans(PK1)-2.5*(rcc_obj$cor[2])*S_corr.X.xscores[,2]
YY22 = colMeans(PK1)+2.5*(rcc_obj$cor[2])*S_corr.X.xscores[,2]
YY111 = colMeans(PK1)-2.5*(rcc_obj$cor[3])*S_corr.X.xscores[,3]
YY222 = colMeans(PK1)+2.5*(rcc_obj$cor[3])*S_corr.X.xscores[,3]


BBGG1 = colMeans(BG1)-2.5*(rcc_obj$cor[2])*S_corr.Y.yscores[,2]
BBGG2 = colMeans(BG1)+2.5*(rcc_obj$cor[2])*S_corr.Y.yscores[,2]


dev.new(width = 7,height = 4)
plot(1,type="n",xlim=c(0,300),main = "1. canonical variate (L1) on mean of original data PK",ylim=c(-2,3),ylab="Value",xlab="Time[min]",xaxt="n",col=rgb(0.4,0.4,0.8,0.6),pch=16,cex=1.3)
axis(1, at=Times1, las=2)
lines(Times1,Data_gg_fixed$mu)
polygon(c(rev(Times1), Times1), c(rev(Data_gg_fixed$Q.PK95), Data_gg_fixed$Q.PK05), col = rgb(0.7,0.7,0.7,0.4) , border = NA)
lines(Times1,YY1,lty=2,col="red")
lines(Times1,YY2,lty=2,col="blue")
legend("topright",legend = c("mean","mean+2.5*sd1*L1","mean-2.5*sd1*L1","90% Quantile"),lty=c(1,2,2,0),pch=c(NA,NA,NA,NA),col=c("black","red","blue","black"),bty="n",border="white",fill=c(0,0,0,"grey"))
dev.off()


dev.new(width = 7,height = 4)
plot(1,type="n",xlim=c(0,300),main = "2. canonical variate (U2) on mean of original data PK",ylim=c(-2,3),ylab="Concentration",xlab="Time[min]",xaxt="n",col=rgb(0.4,0.4,0.8,0.6),pch=16,cex=1.3)
axis(1, at=Times1, las=2)
lines(Times1,Data_gg_fixed$mu)
polygon(c(rev(Times1), Times1), c(rev(Data_gg_fixed$Q.PK95), Data_gg_fixed$Q.PK05), col = rgb(0.7,0.7,0.7,0.4) , border = NA)
lines(Times1,YY11,lty=2,col="red")
lines(Times1,YY22,lty=2,col="blue")
legend("topright",legend = c("mean","mean+2.5*sd2*U2","mean-2.5*sd2*U2","90% Quantile"),lty=c(1,2,2,0),pch=c(NA,NA,NA,NA),col=c("black","red","blue","black"),bty="n",border="white",fill=c(0,0,0,"grey"))
dev.off()

dev.new(width = 7,height = 4)
plot(1,type="n",xlim=c(0,300),main = "2. canonical variate (V2) on mean of original blood glucose",ylim=c(-2,3),ylab="Concentration",xlab="Time[min]",xaxt="n",col=rgb(0.4,0.4,0.8,0.6),pch=16,cex=1.3)
axis(1, at=Times1, las=2)
lines(Times1,Data_gg_fixed$mu)
polygon(c(rev(Times1), Times1), c(rev(Data_gg_fixed$Q.PK95), Data_gg_fixed$Q.PK05), col = rgb(0.7,0.7,0.7,0.4) , border = NA)
lines(Times1,BBGG1,lty=2,col="red")
lines(Times1,BBGG2,lty=2,col="blue")
legend("topright",legend = c("mean","mean+2.5*sd2*V2","mean-2.5*sd2*V2","90% Quantile"),lty=c(1,2,2,0),pch=c(NA,NA,NA,NA),col=c("black","red","blue","black"),bty="n",border="white",fill=c(0,0,0,"grey"))
dev.off()


## First 
dev.new(width = 7,height = 4)
plot(1,type="n",xlim=c(0,300),main = "2. canonical variate (V2) on blood glucose data",ylim=c(-3,3),ylab="Value",xlab="Time[min]",xaxt="n",col=rgb(0.4,0.4,0.8,0.6),pch=16,cex=1.3)
axis(1, at=Times1, las=2)
lines(Times1,Data_gg_fixed$XBG)
polygon(c(rev(Times1), Times1), c(rev(Data_gg_fixed$Q.BG95), Data_gg_fixed$Q.BG05), col = rgb(0.7,0.7,0.7,0.4) , border = NA)
lines(Times1,BBGG1,lty=2,col="red")
lines(Times1,BBGG2,lty=2,col="blue")
legend("bottomright",legend = c("mean","mean+2.5*sd2*V2","mean-2.5*sd2*V2","90% of data"),lty=c(1,2,2,0),pch=c(NA,NA,NA,NA),col=c("black","red","blue","black"),bty="n",border="white",fill=c(0,0,0,"grey"))
dev.off()



plot(1,type="n",xlim=c(0,300),main = "3. canonical variate (L3) on mean of original data PK",ylim=c(-2,3),ylab="Value",xlab="Time[min]",xaxt="n",col=rgb(0.4,0.4,0.8,0.6),pch=16,cex=1.3)
axis(1, at=Times1, las=2)
lines(Times1,Data_gg_fixed$mu)
polygon(c(rev(Times1), Times1), c(rev(Data_gg_fixed$Q.PK95), Data_gg_fixed$Q.PK05), col = rgb(0.7,0.7,0.7,0.4) , border = NA)
lines(Times1,YY111,lty=2,col="red")
lines(Times1,YY222,lty=2,col="blue")
legend("topright",legend = c("mean","mean+2.5*sd1*L1","mean-2.5*sd1*L1","90% Quantile"),lty=c(1,2,2,0),pch=c(NA,NA,NA,NA),col=c("black","red","blue","black"),bty="n",border="white",fill=c(0,0,0,"grey"))




######### ======= plot manually ======================================================
par(mfrow=c(1,1))
d1 = 1
d2 = 2
plot(0, type = "n", xlim = c(-1, 1), ylim = c(-1,1), xlab = paste("Dimension ", d1), ylab = paste("Dimension ",d2),main ="Canonical Loadings for dim 1 and 2 on insulin")
Xnames = as.character(Times1)
points(S_corr.X.xscores[, d1], S_corr.X.xscores[,d2], col = "red",pch=20)
text(S_corr.X.xscores[-12, d1], S_corr.X.xscores[-12,d2], Xnames[-12], col = "red", font = 1,pos=2,cex=1.5)
points(S_corr.Y.xscores[, d1], S_corr.Y.xscores[,d2],col="blue",pch=20)
text(S_corr.Y.xscores[-c(8,15), d1], S_corr.Y.xscores[-c(8,15),d2], Xnames[-c(8,15)], col = "blue", font = 2,pos=4,cex=1.5)
abline(v = 0, h = 0)
lines(cos(seq(0, 2 * pi, l = 100)), sin(seq(0, 2 * pi, l = 100)))
legend("topleft",c("Insulin","Blood Sugar"),col=c("red","blue"),pch=c(20,20),cex=1,pt.cex = 2)

par(mfrow=c(1,1))
plot(0, type = "n", xlim = c(-1, 1), ylim = c(-1,1), xlab = paste("Dimension V1"), ylab = paste("Dimension V2" ),main ="Canonical Loadings for dim 1 and 2 on Blood sugar")
Xnames = as.character(Times1)
points(S_corr.Y.yscores[, 1], S_corr.Y.yscores[,2], col = "red",pch=20)
text(S_corr.Y.yscores[, 1], S_corr.Y.yscores[,d2], Xnames, col = "red", font = 1,pos=2,cex=1.5)
points(S_corr.X.yscores[, 1], S_corr.X.yscores[,2],col="blue",pch=20)
text(S_corr.X.yscores[, 1], S_corr.X.yscores[,2], Xnames, col = "blue", font = 2,pos=4,cex=1.5)
abline(v = 0, h = 0)
lines(cos(seq(0, 2 * pi, l = 100)), sin(seq(0, 2 * pi, l = 100)))
legend("topright",c("Insulin","Blood Sugar"),col=c("red","blue"),pch=c(20,20),cex=1,pt.cex = 2)

plot(yscores_standard[, d1], yscores_standard[, d2],type = "n", main = "Canonical Scores on Dim 1 and 2 on blood sugar", xlab = paste("Dimension V1"), ylab = paste("Dimension V2"))
text(yscores_standard[, d1], yscores_standard[, d2])
abline(v = 0, h = 0, lty = 2)




 ################################


try_data <- data.frame(X = xscores_standard[,1],Y = xscores_standard[,2],`Mean_late_insulin` = (PK1[,13]+PK1[,12]+PK1[,11]+PK1[,10])/4)
names(try_data)
gyt1 <- ggplot(data = try_data,aes(x= X,y = Y,col = `Mean_late_insulin`)) + geom_point(size=5,alpha =0.6) + scale_color_gradient(low = "#0091ff", high = "#f0650e") + theme_minimal() + ggtitle("Canonical scores onto the insulin data",subtitle = "Colored with mean level of late insulin variable ") + xlab("Dimension U1") + ylab("Dimension U2")


try_data2 <- data.frame(X = yscores_standard[,1],Y = yscores_standard[,2],`Mean_late_BG` = (BG1[,13]+BG1[,12]+BG1[,11]+BG1[,10])/4)
names(try_data2)
gyt2 <- ggplot(data = try_data2,aes(x= X,y = Y,col = `Mean_late_BG`)) + geom_point(size=5,alpha =0.6) + scale_color_gradient(low = "#0091ff", high = "#f0650e") + theme_minimal() + ggtitle("Canonical scores onto the blood glucose data",subtitle = "Colored with mean level of late blood glucose variable ") + xlab("Dimension V1") + ylab("Dimension V2")

grid.arrange(gyt1,gyt2,ncol=2)



PP <- as.numeric(Peak_time_PK)
PP1 <- replace(PP,PP==1,3) 
PP1 <- replace(PP1,PP1==2,6);PP1 <- replace(PP1,PP1==3,9) ;PP1 <- replace(PP1,PP1==4,12);PP1 <- replace(PP1,PP1==5,15);PP1 <- replace(PP1,PP1==6,20);PP1 <- replace(PP1,PP1==7,30);PP1 <- replace(PP1,PP1==8,45);PP1 <- replace(PP1,PP1==9,60);PP1 <- replace(PP1,PP1==10,75);PP1 <- replace(PP1,PP1==11,90);PP1 <- replace(PP1,PP1==12,105);PP1 <- replace(PP1,PP1==13,120);PP1 <- replace(PP1,PP1==14,180);PP1 <- replace(PP1,PP1==15,240);PP1 <- replace(PP1,PP1==16,300) 
BGPP <- as.numeric(Peak_time_BG)
GG1 <- replace(BGPP,BGPP==1,3) 
GG1 <- replace(GG1,GG1==2,6);GG1 <- replace(GG1,GG1==3,9) ;GG1 <- replace(GG1,GG1==4,12);GG1 <- replace(GG1,GG1==5,15);GG1 <- replace(GG1,GG1==6,20);GG1 <- replace(GG1,GG1==7,30);GG1 <- replace(GG1,GG1==8,45);GG1 <- replace(GG1,GG1==9,60);GG1 <- replace(GG1,GG1==10,75);GG1 <- replace(GG1,GG1==11,90);GG1 <- replace(GG1,GG1==12,105);GG1 <- replace(GG1,GG1==13,120);GG1 <- replace(GG1,GG1==14,180);GG1 <- replace(GG1,GG1==15,240);GG1 <- replace(GG1,GG1==16,300) 

try_data3 <- data.frame(X = xscores_standard[,1],Y = yscores_standard[,1],Insulin_Peak = PP1,BG_minimum=GG1,inssssss = BGPP)
names(try_data3)
#try_data3 <- data.frame(X = xscores_standard[,1],Y = yscores_standard[,1],Peak_time = as.numeric(Peak_time_BG))
#try_data3 <- data.frame(X = xscores_standard[,1],Y = yscores_standard[,1],Peak_time = PP1)

plot(try_data3$X,try_data3$Y,col=try_data3$Peak_time_PK,pch=20)
names(try_data3)

ggplot(data = try_data3,aes(x= X,y = Y,col = BG_minimum)) + geom_point(size=5,alpha =1) + theme_minimal() + ggtitle("Score of first canonical variates",subtitle = "Color indicates time [min] of blood sugar minimum") + xlab("Dimension U1") + ylab("Dimension V1")+ 
  scale_color_gradient(low = "#0091ff", high = "#f0650e") +
guides(col = guide_colourbar(barwidth = 1, barheight = 20))






try_data3 <- data.frame(X = xscores_standard[,1],Y = yscores_standard[,1],Insulin_Peak = PP1,BG_minimum=GG1,inssssss = BGPP)
names(try_data3)
#try_data3 <- data.frame(X = xscores_standard[,1],Y = yscores_standard[,1],Peak_time = as.numeric(Peak_time_BG))
#try_data3 <- data.frame(X = xscores_standard[,1],Y = yscores_standard[,1],Peak_time = PP1)
param
ggplot(data = try_data3,aes(x= X,y = Y,col = BG_minimum)) + geom_point(size=5,alpha =1) + theme_minimal() + ggtitle("Score of first canonical variates",subtitle = "Color indicates time [min] of blood sugar minimum") + xlab("Dimension U1") + ylab("Dimension V1")+ 
  scale_color_gradient(low = "#0091ff", high = "#f0650e") +
  guides(col = guide_colourbar(barwidth = 1, barheight = 20))


ggplot(data = try_data3,aes(x= X,y = Y,col =Peak_time_PK)) + geom_point(size=5,alpha =1) + theme_minimal() + ggtitle("Score of first canonical variates",subtitle = "Color indicates time [min] of blood sugar minimum") + xlab("Dimension U1") + ylab("Dimension V1")+ 
  scale_color_gradient(low = "#0091ff", high = "#f0650e") +
  guides(col = guide_colourbar(barwidth = 1, barheight = 20))


ggplot(data = try_data3,aes(x= X,y = Y,col = BG_minimum)) + geom_point(size=5,alpha =1) + theme_minimal() + ggtitle("Score of first canonical variates",subtitle = "Color indicates time [min] of blood sugar minimum") + xlab("Dimension U1") + ylab("Dimension V1")+ 
  scale_color_gradient(low = "#0091ff", high = "#f0650e") +
  guides(col = guide_colourbar(barwidth = 1, barheight = 20))



data_nu <- Wide_for_cor.u[unique(which(xscores_standard[,1]< -1 & yscores_standard[,1] < -1)),]
yt <- sort(table(data_nu$Treatment),decreasing = T)


yt <- as.data.frame(yt)
yt <- yt[which(yt$Freq > 9),]
ggplot(yt,aes(x =Var1,y=Freq)) + geom_bar(stat ="identity") + theme_bw() + ggtitle("6 Most frequent formulars in retangular area interest") + xlab("Formula") + ylab("Frequence")






cor(xscores_standard[, d1], xscores_standard[, d2])
cor(yscores_standard[, d1], yscores_standard[, d2])
cor(xscores_standard[, d1], yscores_standard[, d1])

plot(xscores_standard[, 1], yscores_standard[, 1],type = "n", main = "4. Canonical Scores for insulin and blood glucose", xlab = "Dimension 4 Insulin", ylab = "Dimension 4 Blood glucose")
text(xscores_standard[, 1], yscores_standard[, 1])
abline(v = 0, h = 0, lty = 2)


try_data <- data.frame(X = xscores_standard[,1],Y = xscores_standard[,2],Start = PK1[,1],dose = fot1$Dose)
names(try_data)
gyt1 <- ggplot(data = try_data,aes(x= X,y = Y,col = dose)) + geom_point(size=5,alpha =0.6) + scale_color_gradient(low = "#0091ff", high = "#f0650e") + theme_minimal() + ggtitle("Canonical scores onto the insulin data",subtitle = "Colored with mean level of late insulin variable ") + xlab("Dimension U1") + ylab("Dimension U2")



plot(0, type = "n", xlim = c(-1, 1), ylim = c(-1,1), xlab = paste("Dimension U1"), ylab = paste("Dimension U3"),main ="Canonical Loadings for dim 1 and 3 on insulin")
Xnames = as.character(Times1)
points(S_corr.X.xscores[, 1], S_corr.X.xscores[,3], col = "red",pch=20)
text(S_corr.X.xscores[, 1], S_corr.X.xscores[,3], Xnames, col = "red", font = 1,pos=2,cex=1.5)
points(S_corr.Y.xscores[, 1], S_corr.Y.xscores[,3],col="blue",pch=20)
text(S_corr.Y.xscores[, 1], S_corr.Y.xscores[,3], Xnames, col = "blue", font = 2,pos=4,cex=1.5)
abline(v = 0, h = 0)
lines(cos(seq(0, 2 * pi, l = 100)), sin(seq(0, 2 * pi, l = 100)))
legend("topleft",c("Insulin","Blood Sugar"),col=c("red","blue"),pch=c(20,20),cex=1,pt.cex = 2)

plot(xscores_standard[, 1], xscores_standard[, 3],type = "n", ylim=c(-4,2),main = "Canonical scores dim 1 and dim 3 for insulin", xlab = "Dimension U1", ylab = "Dimension U3")
text(xscores_standard[, 1], xscores_standard[, 3])
abline(v = 0, h = 0, lty = 2)




############# ==================== NOT STANDARDIZED ==============================================
par(mfrow=c(1,1))
d1 = 1
d2 = 2
plot(0, type = "n", xlim = c(-1, 1), ylim = c(-1,1), xlab = paste("Dimension ", d1), ylab = paste("Dimension ",d2),main ="Canonical Loadings for dim 1 and 2 on insulin")
Xnames = as.character(Times1)
points(corr.X.xscores[, d1], corr.X.xscores[,d2], col = "red",pch=20)
text(corr.X.xscores[, d1], corr.X.xscores[,d2], Xnames, col = "red", font = 2,pos=4)
points(corr.Y.xscores[, d1], corr.Y.xscores[,d2],col="blue",pch=20)
text(corr.Y.xscores[, d1], corr.Y.xscores[,d2], Xnames, col = "blue", font = 2,pos=2)
abline(v = 0, h = 0)
lines(cos(seq(0, 2 * pi, l = 100)), sin(seq(0, 2 * pi, l = 100)))
legend("topleft",c("Insulin","Blood Sugar"),col=c("red","blue"),pch=c(20,20),cex=1,pt.cex = 2)


plot(xscores[, d1], xscores[, d2],type = "n", main = "Canonical Scores on Dim 1 and 2 on insulin", xlab = paste("Dimension ",d1), ylab = paste("Dimension ", d2))
text(xscores[, d1], xscores[, d2])
abline(v = 0, h = 0, lty = 2)


par(mfrow=c(1,2))
plot(corr.X.xscores[,1],type="l",ylim=c(-0.7,0.7))
lines(corr.X.xscores[,2],type="l",lty=2)

plot(corr.Y.xscores[,1],type="l",col="red",ylim=c(-0.7,0.7),xlab="Dimension")
lines(corr.Y.xscores[,2],type="l",col="red",lty=2)

par(mfrow=c(1,2))
plot(corr.X.xscores[,1],type="l",ylim=c(-0.7,0.7))
lines(corr.Y.xscores[,1],type="l",lty=1,col="red")


plot(corr.X.xscores[,2],type="l",ylim=c(-0.7,0.7),xlab="Variables")
lines(corr.Y.xscores[,2],type="l",col="red",lty=1)









### ====================== Test of significant correlations =========================
rho <- rcc_obj$cor
## Define number of observations, number of variables in first set, and number of variables in the second set.
n <- dim(PK1)[1]
p <- dim(PK1)[2]
q <- dim(BG1)[2]

## Calculate p-values using the F-approximations of different test statistics:
dr <- p.asym(rho, n, p, q, tstat = "Wilks") # 4 canonical variables under 0.05
dr1 <- p.asym(rho, n, p, q, tstat = "Hotelling") # 4 canonical varaibles under 0.05
dr2 <- p.asym(rho, n, p, q, tstat = "Pillai")# 4 canonical varaibles under 0.05
dr3 <- p.asym(rho, n, p, q, tstat = "Roy")# 4 canonical varaibles under 0.05


Peak_time_PK <- apply(PK1,1,which.max)
Peak_time_BG <- apply(BG1,1,which.min)
table(Peak_time_BG)
group1 = c(1,3); group2 = c(4,7);group3 = c(8,16)
grupper3_PK <- ifelse(Peak_time_PK >=group1[1] & Peak_time_PK <=group1[2],1,ifelse(Peak_time_PK >=group2[1] & Peak_time_PK <=group2[2],2,ifelse(Peak_time_PK >=group3[1] & Peak_time_PK <=group3[2],3,"NA")))
#grupper3_BG <- ifelse(Peak_time_BG >=1 & Peak_time_BG <=7,1,ifelse(Peak_time_BG >=8 & Peak_time_BG <=9,2,ifelse(Peak_time_BG >=10 & Peak_time_BG <=16,3,"NA")))

Times_char <- as.character(Times1)
par(mfrow=c(1,1))
plot(rcc_obj$scores$xscores[, d1],rcc_obj$scores$xscores[, d2],type = "n", main = "Canonical scores on groups of insulin reaction time", xlab = paste("Dimension ",d1), ylab = paste("Dimension ", d2))
points(rcc_obj$scores$xscores[, d1], rcc_obj$scores$xscores[, d2],col=grupper3_PK,pch=20,cex=2)
abline(v = 0, h = 0, lty = 2)
legend("bottomleft",c("Fast","Intermediate","Slow"),col=c(1,2,3),pch=c(20,20,20),cex=0.8,pt.cex = 1.4)

#plot(xscores[, d1], xscores[, d2],type = "n", main = "", xlab = paste("Dimension ",d1), ylab = paste("Dimension ", d2))
#points(xscores[, d1], xscores[, d2],col=grupper3_BG,pch=20,cex=2)
#abline(v = 0, h = 0, lty = 2)
#legend("topleft",c("Fast","Intermediate","Slow"),col=c(1,2,3),pch=c(20,20,20))
### 



tsnePK <- Rtsne(rcc_obj$scores$xscores, dims = 2, perplexity=30, verbose=TRUE, max_iter = 1500,pca = F)
plot_data_tsne <- data.frame(X = tsnePK$Y[,1],Y=tsnePK$Y[,2],grupper = grupper3_PK)
plot(plot_data_tsne$X,plot_data_tsne$Y,col=plot_data_tsne$grupper)
unique(plot_data_tsne$grupper)

########## PLot variables with GGPlot
#xscores_standard[,11]+xscores_standard[,12]+xscores_standard[,13]+xscores_standard[,14]
Ins_group1 <- (xscores_standard[,15]+xscores_standard[,16])/2
blood_group1 <- (yscores_standard[,11]+yscores_standard[,12]+yscores_standard[,13]+yscores_standard[,14]+yscores_standard[,15]+yscores_standard[,16])/6


Ny_data_her <- data.frame(X = xscores_standard[,2],Y=yscores_standard[,2],Late_insulin =Ins_group1,Late_blood =blood_group1)

ggplot(data = Ny_data_her,aes(x= X,y = Y,col =Late_insulin)) + geom_point(size=5,alpha =1) + theme_minimal() + ggtitle("Score of second canonical variates",subtitle = "Color indicates mean of late insulin variables") + xlab("Dimension U2") + ylab("Dimension V2")+ 
  scale_color_gradient(low = "#0091ff", high = "#f0650e") +
  guides(col = guide_colourbar(barwidth = 1, barheight = 20))

ggplot(data = Ny_data_her,aes(x= X,y = Y,col =Late_blood)) + geom_point(size=5,alpha =1) + theme_minimal() + ggtitle("Score of second canonical variates",subtitle = "Color indicates mean of late blood glucose variables") + xlab("Dimension U2") + ylab("Dimension V2")+ 
  scale_color_gradient(low = "#0091ff", high = "#f0650e") +
  guides(col = guide_colourbar(barwidth = 1, barheight = 20))



gjh <- cbind(Ny_data_her,group = as.numeric(fot1$PDtmin))
ggplot(data = gjh,aes(x= X,y = Y,col =group)) + geom_point(size=5,alpha =1) + theme_minimal() + ggtitle("Score of second canonical variates",subtitle = "Color indicates mean of late blood glucose variables") + xlab("Dimension U2") + ylab("Dimension V2")+ 
  scale_color_gradient(low = "#0091ff", high = "#f0650e") +
  guides(col = guide_colourbar(barwidth = 1, barheight = 20))











png("C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Billeder R/Week24/insulin_DA.png",width = 650, height = 450) 

plot(1,type="n",xlim=c(0,300),main = "Insulin time series",ylim=c(-2,3),ylab="Concentration (standardized)",xlab="Time[min]",xaxt="n",col=rgb(0.4,0.4,0.8,0.6),pch=16,cex=1.3,cex.main = 1.5,cex.lab=1.7)
axis(1, at=Times1, las=2)
lines(Times1,Data_gg_fixed$mu,lwd=2)
polygon(c(rev(Times1), Times1), c(rev(Data_gg_fixed$Q.PK95), Data_gg_fixed$Q.PK05), col = rgb(0.7,0.7,0.7,0.4) , border = NA)

X11PK <- apply(PK1[which(Wide_for_cor.u == "X11"),],2,mean)
X127PK <- apply(PK1[which(Wide_for_cor.u == "X127"),],2,mean)
XBBPK <- apply(PK1[which(Wide_for_cor.u == "B"),],2,mean)
XAPK <- apply(PK1[which(Wide_for_cor.u == "A"),],2,mean)
lines(Times1,X11PK,col="red",lwd=2)
#lines(Times1,X127PK,col="blue",lwd=2)
lines(Times1,XBBPK,col="blue",lwd=2)
lines(Times1,XAPK,col="orange",lwd=2)


legend("topright",3.3,legend = c("Mean","Formula X11","Formula B","Formula A","90% of data"),lty=c(1,1,1,1, 0),lwd = c(2,2,2,2,0),pch=c(NA,NA,NA,NA,NA),col=c("black","red","blue","orange"),bty="n",border="white",fill=c(0,0,0,0,"grey"),cex=1.6)
dev.off()


png("C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Billeder R/Week24/glucose_DA.png",width = 650, height = 450) 
plot(1,type="n",xlim=c(0,300),main = "Blood sugar time series ",ylim=c(-2,3),ylab="Concentration (standardized)",xlab="Time[min]",xaxt="n",col=rgb(0.4,0.4,0.8,0.6),pch=16,cex=1.3,cex.main = 1.5,cex.lab=1.7)
axis(1, at=Times1, las=2)
lines(Times1,Data_gg_fixed$XBG)
polygon(c(rev(Times1), Times1), c(rev(Data_gg_fixed$Q.BG95), Data_gg_fixed$Q.BG05), col = rgb(0.7,0.7,0.7,0.4) , border = NA)

X11BG <- apply(BG1[which(Wide_for_cor.u == "X11"),],2,mean)
X127BG <- apply(BG1[which(Wide_for_cor.u == "X127"),],2,mean)
XBBBG <- apply(BG1[which(Wide_for_cor.u == "B"),],2,mean)
XABG <- apply(BG1[which(Wide_for_cor.u == "A"),],2,mean)

lines(Times1,X11BG,col="red",lwd=2)
#lines(Times1,X127BG,col="blue",lwd=2)
lines(Times1,XBBBG,col="blue",lwd=2)
lines(Times1,XABG,col="orange",lwd=2)


legend("topright",3.3,legend = c("Mean","Formula X11","Formula B","Formula A","90% of data"),lty=c(1,1,1,1, 0),lwd = c(2,2,2,2,0),pch=c(NA,NA,NA,NA,NA),col=c("black","red","blue","orange"),bty="n",border="white",fill=c(0,0,0,0,"grey"),cex=1.5)
dev.off()









######################## 
png("C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Billeder R/Week22/insulin.png",width = 700, height = 450) 
plot(1,type="n",xlim=c(0,300),main = "Insulin time series",ylim=c(-2,3),ylab="Value",xlab="Time[min]",xaxt="n",col=rgb(0.4,0.4,0.8,0.6),pch=16,cex=1.3)
axis(1, at=Times1, las=2)
lines(Times1,Data_gg_fixed$mu)
polygon(c(rev(Times1), Times1), c(rev(Data_gg_fixed$Q.PK95), Data_gg_fixed$Q.PK05), col = rgb(0.7,0.7,0.7,0.4) , border = NA)

lines(Times1,PK1[1128,],col="red")
lines(Times1,PK1[956,],col="red")
lines(Times1,PK1[314,],col="red")
lines(Times1,PK1[565,],col="red")
lines(Times1,PK1[1356,],col="red")

lines(Times1,PK1[715,],col="blue")

plot(NA,type="n",xlim=c(-0.05,0.05),ylim=c(-2,4))
text(xscores_standard[,1],xscores_standard[,2])

#  tværsnit:
lines(Times1,PK1[1029,],col="blue")
lines(Times1,PK1[1031,],col="darkblue")
lines(Times1,PK1[857,],col="blue")
lines(Times1,PK1[745,],col="blue")

lines(Times1,PK1[314,],col="blue")
lines(Times1,PK1[538,],col="blue")
lines(Times1,PK1[715,],col="blue")
lines(Times1,PK1[140,],col="blue")

legend(200,3.3,legend = c("mean","Ex. 1165","Ex. 565","Ex 564","90% of data"),lty=c(1,1,1,1, 0),pch=c(NA,NA,NA,NA,NA),col=c("black","red","blue","orange"),bty="n",border="white",fill=c(0,0,0,0,"grey"),cex=1.6)
dev.off()
png("C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Billeder R/Week22/blood.png",width = 700, height = 450)
plot(1,type="n",xlim=c(0,300),main = "Blood sugar time series ",ylim=c(-2,3),ylab="Value",xlab="Time[min]",xaxt="n",col=rgb(0.4,0.4,0.8,0.6),pch=16,cex=1.3)
axis(1, at=Times1, las=2)
lines(Times1,Data_gg_fixed$XBG)
polygon(c(rev(Times1), Times1), c(rev(Data_gg_fixed$Q.BG95), Data_gg_fixed$Q.BG05), col = rgb(0.7,0.7,0.7,0.4) , border = NA)

lines(Times1,BG1[1165,],col="red")
lines(Times1,BG1[565,],col="blue")
lines(Times1,BG1[564,],col="orange")
legend(200,3.3,legend = c("mean","Ex. 1165","Ex. 565","Ex 564","90% of data"),lty=c(1,1,1,1, 0),pch=c(NA,NA,NA,NA,NA),col=c("black","red","blue","orange"),bty="n",border="white",fill=c(0,0,0,0,"grey"),cex=1.4)
dev.off()



lines(Times1,PK1[867,],col="green")
lines(Times1,PK1[1100,],col="green")
lines(Times1,PK1[1158,],col="green")

positive13 <- unique(which(apply(PK1[,c(12,13)],1,diff)>0))
positive14 <- unique(which(apply(PK1[,c(13,14)],1,diff)>0))
positive15 <- unique(which(apply(PK1[,c(14,15)],1,diff)>0))
positive16 <- unique(which(apply(PK1[,c(15,16)],1,diff)>0))
Times_data <- matrix(rep(Times1,length(positive)),ncol=16)

matplot(t(PK1[positive,c(15,16)]),type="l")


matplot(t(PK1[positive,]),type="l")
legend("topright",legend = c("mean","mean+2.5*sd1*L1","mean-2.5*sd1*L1","90% Quantile"),lty=c(1,2,2,0),pch=c(NA,NA,NA,NA),col=c("black","red","blue","black"),bty="n",border="white",fill=c(0,0,0,"grey"))


########## PERMUTATION TEST


Kasper.perm(PK1,BG1,rhostart = 1,type="Wilks")



#### ========= 4X4 scores matrix:
data_four <- data.frame(xscores_standard[,1:4],yscores_standard[,1:4])
colnames(data_four) <- c("U1","U2","U3","U4","V1","V2","V3","V4")
# Diag
alpha2 <- 0.05
gg_diag1 <- ggplot(data = data_four,aes(x=U1,y=V1)) + geom_point(alpha=alpha2) + theme_bw() + geom_smooth() + ggtitle("Correlation: 0.75")
gg_diag2 <- ggplot(data = data_four,aes(x=U2,y=V2)) + geom_point(alpha=alpha2) + theme_bw() + geom_smooth()+ ggtitle("Correlation: 0.46")
gg_diag3 <- ggplot(data = data_four,aes(x=U3,y=V3)) + geom_point(alpha=alpha2) + theme_bw() + geom_smooth()+ ggtitle("Correlation: 0.30")
#gg_diag4 <- ggplot(data = data_four,aes(x=U4,y=V4)) + geom_point(alpha=alpha2) + theme_bw() + geom_smooth()+ ggtitle("Correlation: 0.19")
# next to diag
gg_U12 <- ggplot(data = data_four,aes(x=U1,y=U2)) + geom_point(alpha=alpha2) + theme_bw() + geom_smooth()
gg_U13 <- ggplot(data = data_four,aes(x=U1,y=U3)) + geom_point(alpha=alpha2) + theme_bw() + geom_smooth()
#gg_U14 <- ggplot(data = data_four,aes(x=U1,y=U4)) + geom_point(alpha=alpha2) + theme_bw() + geom_smooth()
gg_V12 <- ggplot(data = data_four,aes(x=V1,y=V2)) + geom_point(alpha=alpha2) + theme_bw() + geom_smooth()
gg_V13 <- ggplot(data = data_four,aes(x=V1,y=V3)) + geom_point(alpha=alpha2) + theme_bw() + geom_smooth()
#gg_V14 <- ggplot(data = data_four,aes(x=V1,y=V4)) + geom_point(alpha=alpha2) + theme_bw() + geom_smooth()
## Next next to dig
gg_U23 <- ggplot(data = data_four,aes(x=U2,y=U3)) + geom_point(alpha=alpha2) + theme_bw() + geom_smooth()
#gg_U24 <- ggplot(data = data_four,aes(x=U2,y=U4)) + geom_point(alpha=alpha2) + theme_bw() + geom_smooth()
#gg_U34 <- ggplot(data = data_four,aes(x=U3,y=U4)) + geom_point(alpha=alpha2) + theme_bw() + geom_smooth()

gg_V23 <- ggplot(data = data_four,aes(x=V2,y=V3)) + geom_point(alpha=alpha2) + theme_bw() + geom_smooth()
#gg_V24 <- ggplot(data = data_four,aes(x=V2,y=V4)) + geom_point(alpha=alpha2) + theme_bw() + geom_smooth()
#gg_V34 <- ggplot(data = data_four,aes(x=V3,y=V4)) + geom_point(alpha=alpha2) + theme_bw() + geom_smooth()


tr <- grid.arrange(gg_diag1,gg_U12,gg_U13,gg_V12,gg_diag2,gg_U23,gg_V13,gg_V23,gg_diag3,ncol=3)



ggplot(data = data_four,aes(x=U1,y=U3)) + geom_point() + theme_bw()


######### ====== TEST 
library(MASS)

# We will use the command mvrnorm to draw a matrix of variables

# Let's keep it simple, 
mu <- rep(0,4)
Sigmalol <- matrix(.7, nrow=4, ncol=4) + diag(4)*.7

rawvars <- mvrnorm(n=10000, mu=mu, Sigma=Sigmalol)

cov(rawvars); cor(rawvars)
# We can see our normal sample produces results very similar to our 
#specified covariance levels.
# No lets transform some variables
pvars <- pnorm(rawvars)

XX <- data.frame(X1 = rawvars[,1],X2 = rnorm(10000,0,1))
YY <- data.frame(Y1 = rawvars[,2],Y2 = rnorm(10000,0,1))

test_cc <- cc(XX,YY)

plot(test_cc$scores$xscores[,1],test_cc$scores$yscores[,1])
plot(test_cc$scores$xscores[,2],test_cc$scores$yscores[,2])

plot(test_cc)




############### Treatments on score plot ===========================================================================
Compare_wide <- Wide_for_cor.u[-c(691,692),]
B_index <- which(Compare_wide$Treatment == "B")
A_index <- which(Compare_wide$Treatment == "A")
X11_index <- which(Compare_wide$Treatment == "X11")
X127_index <- which(Compare_wide$Treatment == "X127")



data_f <- data.frame(xscores = xscores_standard[,1],yscores_standard[,1])
plot(xscores_standard[,1],yscores_standard[,1])
points(xscores_standard[B_index,1],yscores_standard[B_index,1],col="red",pch=20)
points(xscores_standard[X11_index,1],yscores_standard[X11_index,1],col="purple",pch=20)
points(xscores_standard[A_index,1],yscores_standard[A_index,1],col="blue",pch=20)
points(xscores_standard[X127_index,1],yscores_standard[X127_index,1],col="orange",pch=20)
points(xscores_standard[1446,1],yscores_standard[1446,1],col="green",pch=20)


Wide_for_cor.u[1064,]
PK1[1064,]

add <- Wide_for_cor.u[unique(which(xscores_standard[,1] < -0.5 & yscores_standard[,1] < -1 )),1]

sort(table(add),decreasing = T)
barplot(sort(table(add),decreasing = T)[1:10])

text(xscores_standard[,1],yscores_standard[,1],xlim=c(-1.5,-1),ylim=c(-2,-1.5))
Wide_for_cor.u



plot(1,type="n",xlim=c(0,300),main = "Insulin time series",ylim=c(-2,3),ylab="Value",xlab="Time[min]",xaxt="n",col=rgb(0.4,0.4,0.8,0.6),pch=16,cex=1.3)
axis(1, at=Times1, las=2)
lines(Times1,Data_gg_fixed$mu)
polygon(c(rev(Times1), Times1), c(rev(Data_gg_fixed$Q.PK95), Data_gg_fixed$Q.PK05), col = rgb(0.7,0.7,0.7,0.4) , border = NA)


mean_X127 <- apply(PK1[X127_index,],2,mean)
mean_B <- apply(PK1[B_index,],2,mean)

lines(Times1,t(mean_X127),col="red")
lines(Times1,t(mean_B),col="blue")
