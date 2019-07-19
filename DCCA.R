###  LOAD PROJECTIONS FROM MATLAB ============================================================
D_xscores <- read.table("C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/xscores.txt",sep = ",")
D_yscores <- read.table("C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/yscores.txt",sep = ",")
D_corrXxscores <- read.table("C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/CorrXxscores.txt",sep = ",")
D_corrYxscores <- read.table("C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/CorrYxscores.txt",sep = ",")
D_corrXyscores <- read.table("C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/CorrXyscores.txt",sep = ",")
D_corrYyscores <- read.table("C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/CorrYyscores.txt",sep = ",")

# 16101010_1
D_xscores <- read.table("C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/xscores_s.txt",sep = ",")
D_yscores <- read.table("C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/yscores_s.txt",sep = ",")
D_corrXxscores <- read.table("C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/CorrXxscores_s.txt",sep = ",")
D_corrYxscores <- read.table("C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/CorrYxscores_s.txt",sep = ",")
D_corrXyscores <- read.table("C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/CorrXyscores_s.txt",sep = ",")
D_corrYyscores <- read.table("C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/CorrYyscores_s.txt",sep = ",")
# 16101010_2
D_xscores <- read.table("C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/xscores_s1.txt",sep = ",")
D_yscores <- read.table("C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/yscores_s1.txt",sep = ",")
D_corrXxscores <- read.table("C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/CorrXxscores_s1.txt",sep = ",")
D_corrYxscores <- read.table("C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/CorrYxscores_s1.txt",sep = ",")
D_corrXyscores <- read.table("C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/CorrXyscores_s1.txt",sep = ",")
D_corrYyscores <- read.table("C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/CorrYyscores_s1.txt",sep = ",")
# 161610_1
D_xscores <- read.table("C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/xscores_s16.txt",sep = ",")
D_yscores <- read.table("C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/yscores_s16.txt",sep = ",")
D_corrXxscores <- read.table("C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/CorrXxscores_s16.txt",sep = ",")
D_corrYxscores <- read.table("C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/CorrYxscores_s16.txt",sep = ",")
D_corrXyscores <- read.table("C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/CorrXyscores_s16.txt",sep = ",")
D_corrYyscores <- read.table("C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/CorrYyscores_s16.txt",sep = ",")

#### 161610_ny
X1_proj <- read.table('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/X1_proj.txt',sep = ",")
XV1_proj <- read.table('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/XV1_proj.txt',sep = ",")
XTe1_proj <- read.table('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/XTe1_proj.txt',sep = ",")

X2_proj <- read.table('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/X2_proj.txt',sep = ",")
XV2_proj <- read.table('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/XV2_proj.txt',sep = ",")
XTe2_proj <- read.table('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/XTe2_proj.txt',sep = ",")

## Test med dropout 0.5 
X1_proj <- read.table('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/X1_proj_test.txt',sep = ",")
XV1_proj <- read.table('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/XV1_proj_test.txt',sep = ",")
XTe1_proj <- read.table('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/XTe1_proj_test.txt',sep = ",")

X2_proj <- read.table('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/X2_proj_test.txt',sep = ",")
XV2_proj <- read.table('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/XV2_proj_test.txt',sep = ",")
XTe2_proj <- read.table('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/XTe2_proj_test.txt',sep = ",")


##

X1_proj <- read.table('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/X1_proj_bay.txt',sep = ",")
XV1_proj <- read.table('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/XV1_proj_bay.txt',sep = ",")
XTe1_proj <- read.table('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/XTe1_proj_bay.txt',sep = ",")

X2_proj <- read.table('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/X2_proj_bay.txt',sep = ",")
XV2_proj <- read.table('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/XV2_proj_bay.txt',sep = ",")
XTe2_proj <- read.table('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/XTe2_proj_bay.txt',sep = ",")



# Small K
X1_proj <- read.table('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/X1_smallK.txt',sep = ",")
XV1_proj <- read.table('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/XV1_smallK.txt',sep = ",")
XTe1_proj <- read.table('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/XTe1_smallK.txt',sep = ",")

X2_proj <- read.table('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/X2_smallK.txt',sep = ",")
XV2_proj <- read.table('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/XV2_smallK.txt',sep = ",")
XTe2_proj <- read.table('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/XTe2_smallK.txt',sep = ",")



################# ==============================================================================================================

Cxx <- var(XTe1_proj, na.rm = TRUE, use = "pairwise")
Cyy <- var(XTe2_proj, na.rm = TRUE, use = "pairwise")
Cxy <- cov(XTe1_proj, XTe2_proj, use = "pairwise")
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

xscores = as.matrix(XTe1_proj) %*% xcoef
yscores = as.matrix(XTe2_proj) %*% ycoef

standard_xcoef <- diag(sqrt(diag(cov(XTe1_proj)))) %*% xcoef
standard_ycoef <- diag(sqrt(diag(cov(XTe2_proj)))) %*% ycoef

xscores_standard <- as.matrix(XTe1_proj) %*% standard_xcoef
yscores_standard <- as.matrix(XTe2_proj) %*% standard_ycoef

cor(xscores_standard,yscores_standard)

cor(xscores_standard[,1],yscores_standard[,1])
plot(xscores_standard[,1],yscores_standard[,1],pch=20)
plot(xscores_standard[,1],yscores_standard[,1],xlim=c((median(xscores_standard[,1])-0.1),(median(xscores_standard[,1])+0.1)),ylim=c((median(yscores_standard[,1])-0.1),(median(yscores_standard[,1])+0.1)),pch=20)


cor(xscores_standard[,2],yscores_standard[,2])
plot(xscores_standard[,2],yscores_standard[,2],pch=20)
plot(xscores_standard[,3],yscores_standard[,3],pch=20)
plot(xscores_standard[,4],yscores_standard[,4],pch=20)

plot(xscores_standard[,2],yscores_standard[,2],xlim=c((median(xscores_standard[,2])-0.1),(median(xscores_standard[,2])+0.1)),ylim=c((median(yscores_standard[,2])-0.1),(median(yscores_standard[,2])+0.1)),pch=20)


#### ========= 4X4 scores matrix:
data_unlinear <- data.frame(xscores[,1:10],yscores[,1:10])
colnames(data_unlinear) <- c("U1","U2","U3","U4","U5","U6","U7","U8","U9","U10", "V1","V2","V3","V4","V5","V6","V7","V8","V9","V10")
#long <- data_unlinear %>% gather(Value,Var,U1:V10)

# Diag
alpha2 <- 1
gg_diag1 <- ggplot(data = data_unlinear,aes(x=U1,y=V1)) + geom_point(alpha=alpha2) + theme_bw() + geom_smooth() 
gg_diag2 <- ggplot(data = data_unlinear,aes(x=U2,y=V2)) + geom_point(alpha=alpha2) + theme_bw() + geom_smooth()
gg_diag3 <- ggplot(data = data_unlinear,aes(x=U3,y=V3)) + geom_point(alpha=alpha2) + theme_bw() + geom_smooth()
gg_diag4 <- ggplot(data = data_unlinear,aes(x=U4,y=V4)) + geom_point(alpha=alpha2) + theme_bw() + geom_smooth()

# next to diag
gg_U12 <- ggplot(data = data_unlinear,aes(x=U1,y=U2)) + geom_point(alpha=alpha2) + theme_bw() + geom_smooth()
gg_U13 <- ggplot(data = data_unlinear,aes(x=U1,y=U3)) + geom_point(alpha=alpha2) + theme_bw() + geom_smooth()
gg_U14 <- ggplot(data = data_unlinear,aes(x=U1,y=U4)) + geom_point(alpha=alpha2) + theme_bw() + geom_smooth()
gg_V12 <- ggplot(data = data_unlinear,aes(x=V1,y=V2)) + geom_point(alpha=alpha2) + theme_bw() + geom_smooth()
gg_V13 <- ggplot(data = data_unlinear,aes(x=V1,y=V3)) + geom_point(alpha=alpha2) + theme_bw() + geom_smooth()
gg_V14 <- ggplot(data = data_unlinear,aes(x=V1,y=V4)) + geom_point(alpha=alpha2) + theme_bw() + geom_smooth()
## Next next to dig
gg_U23 <- ggplot(data = data_unlinear,aes(x=U2,y=U3)) + geom_point(alpha=alpha2) + theme_bw() + geom_smooth()
gg_U24 <- ggplot(data = data_unlinear,aes(x=U2,y=U4)) + geom_point(alpha=alpha2) + theme_bw() + geom_smooth()
gg_U34 <- ggplot(data = data_unlinear,aes(x=U3,y=U4)) + geom_point(alpha=alpha2) + theme_bw() + geom_smooth()

gg_V23 <- ggplot(data = data_unlinear,aes(x=V2,y=V3)) + geom_point(alpha=alpha2) + theme_bw() + geom_smooth()
gg_V24 <- ggplot(data = data_unlinear,aes(x=V2,y=V4)) + geom_point(alpha=alpha2) + theme_bw() + geom_smooth()
gg_V34 <- ggplot(data = data_unlinear,aes(x=V3,y=V4)) + geom_point(alpha=alpha2) + theme_bw() + geom_smooth()


tr <- grid.arrange(gg_diag1,gg_U12,gg_U13,gg_U14,gg_V12,gg_diag2,gg_U23,gg_U24,gg_V13,gg_V23,gg_diag3,gg_U34,gg_V14,gg_V24,gg_V34,gg_diag4,ncol=4)




# ============================
d1 = 1
d2 = 2
plot(0, type = "n", xlim = c(-1, 1), ylim = c(-1,1), xlab = paste("Dimension ", d1), ylab = paste("Dimension ",d2))
Xnames = as.character(Times1)
text(D_corrXxscores[, d1], D_corrXxscores[,d2], Xnames, col = "red", font = 2)
text(D_corrYxscores[, d1], D_corrYxscores[,d2], Xnames, col = "blue", font = 3)
abline(v = 0, h = 0)
lines(cos(seq(0, 2 * pi, l = 100)), sin(seq(0, 2 * pi, l = 100)))


plot(D_xscores[, d1], D_xscores[, d2],type = "n", main = "", xlab = paste("Dimension ",d1), ylab = paste("Dimension ", d2))
points(D_xscores[, d1], D_xscores[, d2],pch=20)
abline(v = 0, h = 0, lty = 2)

plot(D_xscores[, 2], D_xscores[, 3],type = "n", main = "", xlab = paste("Dimension ",d1), ylab = paste("Dimension ", d2))
text(D_xscores[, 2], D_xscores[, 3])
abline(v = 0, h = 0, lty = 2)


plot(D_xscores[, 3], D_xscores[, 4],type = "n", main = "", xlab = paste("Dimension ",d1), ylab = paste("Dimension ", d2))
text(D_xscores[, 3], D_xscores[, 4])
abline(v = 0, h = 0, lty = 2)

cor(D_xscores[, 3], D_xscores[, 4])
################ 
#### Peak time on canonical dimension ==================================================0
IDs <- Wide_for_cor.u[-c(691,692),2][ID3,]
fot <- param  %>% filter(ID %in% IDs$ID)
fot[258,] <- fot[257,]
Peak_test_set <- Peak_time_PK[ID3]
grupper3_PK_test <- ifelse(Peak_test_set >=1 & Peak_test_set <=3,1,ifelse(Peak_test_set >=4 & Peak_test_set <=6,2,ifelse(Peak_test_set >=7 & Peak_test_set <=16,3,"NA")))

Test_data <- cbind(D_xscores,fot,grupper3_PK_test)
colnames(Test_data)[69] <- "Reaction time Groups"

## Plot with groups 
par(mfrow=c(1,1))
plot(Test_data[, d1], Test_data[, d2],type = "n", main = "ANN Canonical scores on groups of insulin reaction time", xlab = paste("Dimension ",d1), ylab = paste("Dimension ", d2))
points(Test_data[, d1], Test_data[, d2],pch=20,col = grupper3_PK_test,cex=1.5)
abline(v = 0, h = 0, lty = 2)
legend("topleft",c("Fast","Intermediate","Slow"),col=c(1,2,3),pch=c(20,20,20),cex=0.6,pt.cex = 1.4)

par(mfrow=c(1,1))
set.seed(415113326)
tsnePK <- Rtsne(D_xscores, dims = 2, perplexity=30, verbose=TRUE, max_iter = 1500,pca = F)

## Select variable to investigate
Var4 <- Test_data$PKAUC15
var_cut <- cut(Var4,breaks = seq(min(Var4,na.rm=T),max(Var4,na.rm=T),len = 5),right = F,labels = c(1:4))
Test_data$VAR <- var_cut
# Plot this variable:
ggplot(data = Test_data,aes(x = V1,y = V2,col =VAR)) + geom_point() + ggtitle("ANN Canonical scores on groups of insulin reaction time") + ylab("Dimension 2")+xlab("Dimension 1")
ggplot(data = data.frame(V1 = tsnePK$Y[,1],V2 = tsnePK$Y[,2],Variable = var_cut),aes(x = V1,y = V2,col =Variable)) + geom_point() + ggtitle("ANN Canonical scores on groups of insulin reaction time") + ylab("Dimension 2")+xlab("Dimension 1")





plot_data_tsne <- data.frame(X = tsnePK$Y[,1],Y=tsnePK$Y[,2],grupper = grupper3_PK_test)
plot(plot_data_tsne$X,plot_data_tsne$Y,col=plot_data_tsne$grupper,pch=20,xlab="Dimension1",ylab="Dimension2",main="2 Tsne dimensions with groups of insulin reaction time")
legend("bottomright",c("Fast","Intermediate","Slow"),col=c(1,2,3),pch=c(20,20,20),cex=0.6,pt.cex = 1.4)






### VARIANCE ===========
ColM <- colMeans(PK_test)
Q.PK95 = apply(PK1, 2, quantile, probs=c(0.95))
Q.PK05 = apply(PK1, 2, quantile, probs=c(0.05))
Q.BG95 = apply(BG1, 2, quantile, probs=c(0.95))
Q.BG05 = apply(BG1, 2, quantile, probs=c(0.05))
Y1=ColM-2.5*sqrt(0.75)*D_corrXxscores[,1]
Y2=ColM+2.5*sqrt(0.75)*D_corrXxscores[,1]
par(mfrow=c(1,1))
plot(1,type="n",xlim=c(0,300),main = "1. canonical variate (L1) on mean of original data PK",ylim=c(-2,3),ylab="Value",xlab="Time[min]",xaxt="n",col=rgb(0.4,0.4,0.8,0.6),pch=16,cex=1.3)
axis(1, at=Times1, las=2)
lines(Times1,ColM)
polygon(c(rev(Times1), Times1), c(rev(Q.PK95),Q.PK05), col = rgb(0.7,0.7,0.7,0.4) , border = NA)
lines(Times1,Y1,lty=2,col="red")
lines(Times1,Y2,lty=2,col="blue")







######### CORRELATION 
cor(D_xscores[,1],D_yscores[,1])
oq <- data.frame(X = 1:258,xscores = D_xscores[,1],yscores = D_yscores[,1])
p1 <- ggplot() + geom_smooth(data =oq ,aes(x =X,y = xscores),col="red") + geom_smooth(data = oq,aes(x = X,y = yscores+0.42),col="blue") + theme_bw()
p1


cor(D_xscores[, d1], D_xscores[, d2])
cor(yscores_standard[, d1], yscores_standard[, d2])




#### ========= 4X4 scores matrix:
data_five <- data.frame(D_xscores[,1:4],D_yscores[,1:4])
colnames(data_five) <- c("U1","U2","U3","U4","V1","V2","V3","V4")
# Diag
alpha2 <- 0.05
gg_diag1 <- ggplot(data = data_five,aes(x=U1,y=V1)) + geom_point(alpha=alpha2) + theme_bw() + geom_smooth() + ggtitle("Correlation: ")
gg_diag2 <- ggplot(data = data_five,aes(x=U2,y=V2)) + geom_point(alpha=alpha2) + theme_bw() + geom_smooth()+ ggtitle("Correlation: ")
gg_diag3 <- ggplot(data = data_five,aes(x=U3,y=V3)) + geom_point(alpha=alpha2) + theme_bw() + geom_smooth()+ ggtitle("Correlation: ")
gg_diag4 <- ggplot(data = data_five,aes(x=U4,y=V4)) + geom_point(alpha=alpha2) + theme_bw() + geom_smooth()+ ggtitle("Correlation: ")
# next to diag
gg_U12 <- ggplot(data = data_five,aes(x=U1,y=U2)) + geom_point(alpha=alpha2) + theme_bw() + geom_smooth()
gg_U13 <- ggplot(data = data_five,aes(x=U1,y=U3)) + geom_point(alpha=alpha2) + theme_bw() + geom_smooth()
gg_U14 <- ggplot(data = data_five,aes(x=U1,y=U4)) + geom_point(alpha=alpha2) + theme_bw() + geom_smooth()
gg_V12 <- ggplot(data = data_five,aes(x=V1,y=V2)) + geom_point(alpha=alpha2) + theme_bw() + geom_smooth()
gg_V13 <- ggplot(data = data_five,aes(x=V1,y=V3)) + geom_point(alpha=alpha2) + theme_bw() + geom_smooth()
gg_V14 <- ggplot(data = data_five,aes(x=V1,y=V4)) + geom_point(alpha=alpha2) + theme_bw() + geom_smooth()
## Next next to dig
gg_U23 <- ggplot(data = data_five,aes(x=U2,y=U3)) + geom_point(alpha=alpha2) + theme_bw() + geom_smooth()
gg_U24 <- ggplot(data = data_five,aes(x=U2,y=U4)) + geom_point(alpha=alpha2) + theme_bw() + geom_smooth()
gg_U34 <- ggplot(data = data_five,aes(x=U3,y=U4)) + geom_point(alpha=alpha2) + theme_bw() + geom_smooth()

gg_V23 <- ggplot(data = data_five,aes(x=V2,y=V3)) + geom_point(alpha=alpha2) + theme_bw() + geom_smooth()
gg_V24 <- ggplot(data = data_five,aes(x=V2,y=V4)) + geom_point(alpha=alpha2) + theme_bw() + geom_smooth()
gg_V34 <- ggplot(data = data_five,aes(x=V3,y=V4)) + geom_point(alpha=alpha2) + theme_bw() + geom_smooth()


tr2 <- grid.arrange(gg_diag1,gg_U12,gg_U13,gg_U14,gg_V12,gg_diag2,gg_U23,gg_U24,gg_V13,gg_V23,gg_diag3,gg_U34,gg_V14,gg_V24,gg_V34,gg_diag4,ncol=4)




