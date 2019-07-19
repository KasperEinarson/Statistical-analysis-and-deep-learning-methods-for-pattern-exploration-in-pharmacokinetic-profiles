



############## MODEL A ========================
## No dropout
XTe1_proj <- read.table('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/A_nodrop_XTe1proj.txt',sep = ",")
XTe2_proj <- read.table('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/A_nodrop_XTe2proj.txt',sep = ",")
## Dropout
XTe1_proj <- read.table('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/A_XTe1proj.txt',sep = ",")
XTe2_proj <- read.table('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/A_XTe2proj.txt',sep = ",")


############## MODEL B ========================
## No dropout
XTe1_proj <- read.table('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/B_nodrop_XTe1proj.txt',sep = ",")
XTe2_proj <- read.table('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/B_nodrop_XTe2proj.txt',sep = ",")
## Dropout
XTe1_proj <- read.table('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/B_XTe1proj.txt',sep = ",")
XTe2_proj <- read.table('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/B_XTe2proj.txt',sep = ",")


############## MODEL C ========================
## No dropout
XTe1_proj <- read.table('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/C_nodrop_XTe1proj.txt',sep = ",")
XTe2_proj <- read.table('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/C_nodrop_XTe2proj.txt',sep = ",")
## Dropout
XTe1_proj <- read.table('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/C_XTe1proj.txt',sep = ",")
XTe2_proj <- read.table('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/C_XTe2proj.txt',sep = ",")


############## MODEL D ========================
## No dropout
XTe1_proj <- read.table('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/D_nodrop_XTe1proj.txt',sep = ",")
XTe2_proj <- read.table('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/D_nodrop_XTe2proj.txt',sep = ",")
## Dropout
XTe1_proj <- read.table('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/D_XTe1proj.txt',sep = ",")
XTe2_proj <- read.table('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/D_XTe2proj.txt',sep = ",")





############ 

XTe1_proj <- read.table('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/X1_train.txt',sep = ",")
XTe2_proj <- read.table('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/X2_train.txt',sep = ",")



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
sum(values)
xcoef <- Bfacinv %*% result$u
ycoef <- Cfacinv %*% result$v

xscores = as.matrix(XTe1_proj) %*% xcoef
yscores = as.matrix(XTe2_proj) %*% ycoef

standard_xcoef <- diag(sqrt(diag(cov(XTe1_proj)))) %*% xcoef
standard_ycoef <- diag(sqrt(diag(cov(XTe2_proj)))) %*% ycoef

xscores_standard <- as.matrix(XTe1_proj) %*% standard_xcoef
yscores_standard <- as.matrix(XTe2_proj) %*% standard_ycoef


cor(xscores_standard[,1],yscores_standard[,1])
cor(xscores_standard[,2],yscores_standard[,2])


####### ============== 
par(mfrow=c(1,1))
plot(xscores_standard[,1],yscores_standard[,1],pch=20,xlab="U1",ylab="V1",main="U1,V1 Correlation 0.75")
plot(xscores_standard[,2],yscores_standard[,1],pch=20,xlab="U1",ylab="V1",main="U1,V1")
plot(xscores_standard[,3],yscores_standard[,1],pch=20,xlab="U1",ylab="V1",main="U1,V1")
plot(xscores_standard[,4],yscores_standard[,1],pch=20,xlab="U1",ylab="V1",main="U1,V1")

plot(xscores_standard[,5],yscores_standard[,1],pch=20,xlab="U1",ylab="V1",main="U1,V1")
plot(xscores_standard[,6],yscores_standard[,1],pch=20,xlab="U1",ylab="V1",main="U1,V1")
plot(xscores_standard[,7],yscores_standard[,1],pch=20,xlab="U1",ylab="V1",main="U1,V1")
plot(xscores_standard[,8],yscores_standard[,1],pch=20,xlab="U1",ylab="V1",main="U1,V1")


####========

cor(xscores_standard[,1],yscores_standard[,1])
cor(xscores_standard[,2],yscores_standard[,2])
cor(xscores_standard[,3],yscores_standard[,3])

par(mfrow=c(3,3))

plot(xscores_standard[,1],yscores_standard[,1],pch=20,xlab="U1",ylab="V1",main="U1,V1")
plot(xscores_standard[,1],xscores_standard[,2],pch=20,xlab="U1",ylab="U2",main="U1,U2")
plot(xscores_standard[,1],xscores_standard[,3],pch=20,xlab="U1",ylab="U3",main="U1,U3")

plot(yscores_standard[,1],yscores_standard[,2],pch=20,xlab="V1",ylab="V2",main="V1,V2")
plot(xscores_standard[,2],yscores_standard[,2],pch=20,xlab="U2",ylab="V2",main="U2,V2")
plot(xscores_standard[,2],xscores_standard[,3],pch=20,xlab="U2",ylab="U3",main="U2,U3")

plot(yscores_standard[,1],yscores_standard[,3],pch=20,xlab="V1",ylab="V3",main="V1,V3")
plot(yscores_standard[,2],yscores_standard[,3],pch=20,xlab="V2",ylab="V3",main="V2,V3")
plot(xscores_standard[,3],yscores_standard[,3],pch=20,xlab="U3",ylab="V3",main="U3,V3")

library(gridExtra)
######## ============== plots:

data_unlinear <- data.frame(xscores_standard[,1:3],yscores[,1:3])
colnames(data_unlinear) <- c("U1","U2","U3", "V1","V2","V3")
#long <- data_unlinear %>% gather(Value,Var,U1:V10)

# Diag
alpha2 <- 1
gg_diag1 <- ggplot(data = data_unlinear,aes(x=U1,y=V1)) + geom_point(alpha=alpha2) + theme_bw() + geom_smooth() 
gg_diag2 <- ggplot(data = data_unlinear,aes(x=U2,y=V2)) + geom_point(alpha=alpha2) + theme_bw()+ geom_smooth() 
gg_diag3 <- ggplot(data = data_unlinear,aes(x=U3,y=V3)) + geom_point(alpha=alpha2) + theme_bw()+ geom_smooth() 

# next to diag
gg_U12 <- ggplot(data = data_unlinear,aes(x=U1,y=U2)) + geom_point(alpha=alpha2) + theme_bw()+ geom_smooth() 
gg_U13 <- ggplot(data = data_unlinear,aes(x=U1,y=U3)) + geom_point(alpha=alpha2) + theme_bw() + geom_smooth() 
gg_V12 <- ggplot(data = data_unlinear,aes(x=V1,y=V2)) + geom_point(alpha=alpha2) + theme_bw()+ geom_smooth() 
gg_V13 <- ggplot(data = data_unlinear,aes(x=V1,y=V3)) + geom_point(alpha=alpha2) + theme_bw() + geom_smooth() 
## Next next to dig
gg_U23 <- ggplot(data = data_unlinear,aes(x=U2,y=U3)) + geom_point(alpha=alpha2) + theme_bw() + geom_smooth() 

gg_V23 <- ggplot(data = data_unlinear,aes(x=V2,y=V3)) + geom_point(alpha=alpha2) + theme_bw() + geom_smooth() 

tr <- grid.arrange(gg_diag1,gg_U12,gg_U13,gg_V12,gg_diag2,gg_U23,gg_V13,gg_V23,gg_diag3,ncol=3)





#### ====================== 
IDDD <- IDD$x[-1]

IDs <- Wide_for_cor.u[-c(691,692),2][ID3,]
Peak_test_set <- Peak_time_PK[ID3]

PP <- as.numeric(Peak_time_PK[ID3])
PP1 <- replace(PP,PP==1,3) 
PP1 <- replace(PP1,PP1==2,6);PP1 <- replace(PP1,PP1==3,9) ;PP1 <- replace(PP1,PP1==4,12);PP1 <- replace(PP1,PP1==5,15);PP1 <- replace(PP1,PP1==6,20);PP1 <- replace(PP1,PP1==7,30);PP1 <- replace(PP1,PP1==8,45);PP1 <- replace(PP1,PP1==9,60);PP1 <- replace(PP1,PP1==10,75) 
BGPP <- as.numeric(Peak_time_BG[ID3])
GG1 <- replace(BGPP,BGPP==1,3) 
GG1 <- replace(GG1,GG1==2,6);GG1 <- replace(GG1,GG1==3,9) ;GG1 <- replace(GG1,GG1==4,12);GG1 <- replace(GG1,GG1==5,15);GG1 <- replace(GG1,GG1==6,20);GG1 <- replace(GG1,GG1==7,30);GG1 <- replace(GG1,GG1==8,45);GG1 <- replace(GG1,GG1==9,60);GG1 <- replace(GG1,GG1==10,75);GG1 <- replace(GG1,GG1==11,90);GG1 <- replace(GG1,GG1==12,105);GG1 <- replace(GG1,GG1==13,120);GG1 <- replace(GG1,GG1==14,180);GG1 <- replace(GG1,GG1==15,240);GG1 <- replace(GG1,GG1==16,300) 

Late_peak <- (PK1[ID3,14] +PK1[ID3,15]+PK1[ID3,16])/3
Late_min <- (BG1[ID3,14] +BG1[ID3,15]+BG1[ID3,16])/3

try_data3 <- data.frame(X = xscores_standard[,1],Y = yscores_standard[,1],X2 = xscores_standard[,2], Y2 = yscores_standard[,2],Insulin_Peak=PP,BG_minimum = BGPP,Late_peak=Late_peak)
#try_data_again <- data.frame(X = xscores_standard[-which(xscores_standard==max(xscores_standard)),1],Y = yscores_standard[-which(xscores_standard==max(xscores_standard)),1],Insulin_Peak = PP1[-which(xscores_standard==max(xscores_standard))])
#try_data3 <- data.frame(X = xscores_standard[,1],Y = yscores_standard[,1],Peak_time = as.numeric(Peak_time_BG))
#try_data3 <- data.frame(X = xscores_standard[,1],Y = yscores_standard[,1],Peak_time = PP1)

names(try_data3)
L1 <- ggplot(data = try_data3,aes(x= X,y = Y,col = Insulin_Peak)) + geom_point(size=5,alpha =1) + theme_minimal() + ggtitle("Score of first canonical variates. Non linear CCA. Training data",subtitle = "Color indicates time [min] of insulin peak[min]") + xlab("Dimension U1") + ylab("Dimension V1")+ 
  scale_color_gradient(low = "#0091ff", high = "#f0650e") +
  guides(col = guide_colourbar(barwidth = 1, barheight = 20))

L2 <- ggplot(data = try_data3,aes(x= X,y = Y,col = BG_minimum)) + geom_point(size=5,alpha =1) + theme_minimal() + ggtitle("Score of first canonical variates. Non linear CCA. Training data",subtitle = "Color indicates time [min] of blood sugar minimum") + xlab("Dimension U1") + ylab("Dimension V1")+ 
  scale_color_gradient(low = "#0091ff", high = "#f0650e") +
  guides(col = guide_colourbar(barwidth = 1, barheight = 20))


ggplot(data = try_data3,aes(x= X2,y = Y2,col = BG_minimum)) + geom_point(size=5,alpha =1) + theme_minimal() + ggtitle("Score of first canonical variates",subtitle = "Color indicates time [min] of blood sugar minimum") + xlab("Dimension U2") + ylab("Dimension V2")+ 
  scale_color_gradient(low = "#0091ff", high = "#f0650e") +
  guides(col = guide_colourbar(barwidth = 1, barheight = 20))

ggplot(data = try_data3,aes(x= X2,y = Y2,col = Insulin_Peak)) + geom_point(size=5,alpha =1) + theme_minimal() + ggtitle("Score of first canonical variates. Non linear CCA",subtitle = "Color indicates time [min] of blood sugar minimum") + xlab("Dimension U2") + ylab("Dimension V2")+ 
  scale_color_gradient(low = "#0091ff", high = "#f0650e") +
  guides(col = guide_colourbar(barwidth = 1, barheight = 20))
############ ================================= 

ggplot(data = try_data3,aes(x= X2,y = Y2,col = Late_peak)) + geom_point(size=5,alpha =1) + theme_minimal() + ggtitle("Score of second canonical variates. Non linear CCA",subtitle = "Color indicates time [min] of blood sugar minimum") + xlab("Dimension U2") + ylab("Dimension V2")+ 
  scale_color_gradient(low = "#0091ff", high = "#f0650e") +
  guides(col = guide_colourbar(barwidth = 1, barheight = 20))

ggplot(data = try_data3,aes(x= X2,y = Y2,col = Late_min)) + geom_point(size=5,alpha =1) + theme_minimal() + ggtitle("Score of second canonical variates. Non linear CCA",subtitle = "Color indicates time [min] of blood sugar minimum") + xlab("Dimension U2") + ylab("Dimension V2")+ 
  scale_color_gradient(low = "#0091ff", high = "#f0650e") +
  guides(col = guide_colourbar(barwidth = 1, barheight = 20))


par(mfrow=c(1,1))
plot(try_data3$X,try_data3$Y,col=try_data3$Insulin_Peak,pch=20)
plot(xscores_standard[,1],xscores_standard[,2])





















Peak_test_set <- Peak_time_PK[ID1]

PP <- as.numeric(Peak_time_PK[ID1])
PP1 <- replace(PP,PP==1,3) 
PP1 <- replace(PP1,PP1==2,6);PP1 <- replace(PP1,PP1==3,9) ;PP1 <- replace(PP1,PP1==4,12);PP1 <- replace(PP1,PP1==5,15);PP1 <- replace(PP1,PP1==6,20);PP1 <- replace(PP1,PP1==7,30);PP1 <- replace(PP1,PP1==8,45);PP1 <- replace(PP1,PP1==9,60);PP1 <- replace(PP1,PP1==10,75) 
BGPP <- as.numeric(Peak_time_BG[ID1])
GG1 <- replace(BGPP,BGPP==1,3) 
GG1 <- replace(GG1,GG1==2,6);GG1 <- replace(GG1,GG1==3,9) ;GG1 <- replace(GG1,GG1==4,12);GG1 <- replace(GG1,GG1==5,15);GG1 <- replace(GG1,GG1==6,20);GG1 <- replace(GG1,GG1==7,30);GG1 <- replace(GG1,GG1==8,45);GG1 <- replace(GG1,GG1==9,60);GG1 <- replace(GG1,GG1==10,75);GG1 <- replace(GG1,GG1==11,90);GG1 <- replace(GG1,GG1==12,105);GG1 <- replace(GG1,GG1==13,120);GG1 <- replace(GG1,GG1==14,180);GG1 <- replace(GG1,GG1==15,240);GG1 <- replace(GG1,GG1==16,300) 

try_data3 <- data.frame(X = xscores_standard[,1],Y = yscores_standard[,1],X2 = xscores_standard[,2], Y2 = yscores_standard[,2],Insulin_Peak=PP)
#try_data_again <- data.frame(X = xscores_standard[-which(xscores_standard==max(xscores_standard)),1],Y = yscores_standard[-which(xscores_standard==max(xscores_standard)),1],Insulin_Peak = PP1[-which(xscores_standard==max(xscores_standard))])
#try_data3 <- data.frame(X = xscores_standard[,1],Y = yscores_standard[,1],Peak_time = as.numeric(Peak_time_BG))
#try_data3 <- data.frame(X = xscores_standard[,1],Y = yscores_standard[,1],Peak_time = PP1)

names(try_data3)
L1 <- ggplot(data = try_data3,aes(x= X2,y = Y2,col = Insulin_Peak)) + geom_point(size=5,alpha =1) + theme_minimal() + ggtitle("Score of first canonical variates. Non linear CCA. Training data",subtitle = "Color indicates time [min] of insulin peak[min]") + xlab("Dimension U1") + ylab("Dimension V1")+ 
  scale_color_gradient(low = "#0091ff", high = "#f0650e") +
  guides(col = guide_colourbar(barwidth = 1, barheight = 20))

L2 <- ggplot(data = try_data3,aes(x= X,y = Y,col = Insulin_Peak)) + geom_point(size=5,alpha =1) + theme_minimal() + ggtitle("Score of first canonical variates. Non linear CCA. Training data",subtitle = "Color indicates time [min] of blood sugar minimum") + xlab("Dimension U1") + ylab("Dimension V1")+ 
  scale_color_gradient(low = "#0091ff", high = "#f0650e") +
  guides(col = guide_colourbar(barwidth = 1, barheight = 20))











# Diag
alpha2 <- 1
gg_diag1 <- ggplot(data = data_unlinear,aes(x=U1,y=V1)) + geom_point(alpha=alpha2) + theme_bw() + geom_smooth() 
gg_diag2 <- ggplot(data = data_unlinear,aes(x=U2,y=V2)) + geom_point(alpha=alpha2) + theme_bw() + geom_smooth()
gg_diag3 <- ggplot(data = data_unlinear,aes(x=U3,y=V3)) + geom_point(alpha=alpha2) + theme_bw() + geom_smooth()

# next to diag
gg_U12 <- ggplot(data = data_unlinear,aes(x=U1,y=U2)) + geom_point(alpha=alpha2) + theme_bw() + geom_smooth()
gg_U13 <- ggplot(data = data_unlinear,aes(x=U1,y=U3)) + geom_point(alpha=alpha2) + theme_bw() + geom_smooth()
gg_V12 <- ggplot(data = data_unlinear,aes(x=V1,y=V2)) + geom_point(alpha=alpha2) + theme_bw() + geom_smooth()
gg_V13 <- ggplot(data = data_unlinear,aes(x=V1,y=V3)) + geom_point(alpha=alpha2) + theme_bw() + geom_smooth()
## Next next to dig
gg_U23 <- ggplot(data = data_unlinear,aes(x=U2,y=U3)) + geom_point(alpha=alpha2) + theme_bw() + geom_smooth()

gg_V23 <- ggplot(data = data_unlinear,aes(x=V2,y=V3)) + geom_point(alpha=alpha2) + theme_bw() + geom_smooth()

tr <- grid.arrange(gg_diag1,gg_U12,gg_U13,gg_V12,gg_diag2,gg_U23,gg_V13,gg_V23,gg_diag3,ncol=3)

