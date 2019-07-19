library(mixOmics)
library(tidyr)
library(dplyr)
library(CCA)
library(Rtsne)
library(FactoMineR)
library(ggplot2)
library(factoextra)
library(CCP)
PK_scaled_row <- read.table("PK_scaled_row.txt")
BG_scaled_row <- read.table("BG_scaled_row.txt")
colnames <- c("3","6","9","12","15","20","30","45","60","75","90","105","120","180","240","300")

colnames(BG_scaled_row) <- paste0("BG.",colnames)
colnames(PK_scaled_row) <- paste0("PK.",colnames)
levels_BG <- colnames(PK)

Joy_data_PK_row <- as.data.frame(as.data.frame(PK_scaled_row)%>% gather(Time,PK))
Joy_data_BG_row <- as.data.frame(as.data.frame(BG_scaled_row)%>% gather(Time,BG))

Joy_data_BG_row$Time <- factor(Joy_data_BG_row$Time,levels = levels_BG)
Joy_data_PK_row$Time <- factor(Joy_data_PK_row$Time,levels = levels_BG)



Joy_data_PK_col <- as.data.frame(as.data.frame(PK_scaled_col)%>% gather(Time,PK))
Joy_data_BG_col <- as.data.frame(as.data.frame(BG_scaled_col)%>% gather(Time,BG))

Joy_data_BG_col$Time <- factor(Joy_data_BG_col$Time,levels = levels_BG)
Joy_data_PK_col$Time <- factor(Joy_data_PK_col$Time,levels = levels_BG)


Joy_data_PK <- as.data.frame(PK%>% gather(Time,PK))
Joy_data_PK$index <- seq(1,length(Joy_data_PK$Time))
Joy_data_PK$Time <- as.factor(Joy_data_PK$Time)
Joy_data_PK$Time<- factor(Joy_data_PK$Time,levels = levels_BG)

Joy_data_BG <- as.data.frame(BG%>% gather(Time,BG))
Joy_data_BG$index <- seq(1,length(Joy_data_BG$Time))

rt <- Joy_data_PK %>% group_by(Time) %>% arrange(-PK) %>% top_n(n=5,wt = PK)
rt_BG <- Joy_data_BG %>% group_by(Time) %>% arrange(-BG) %>% top_n(n=5,wt = BG)

sort_PK <- Joy_data_PK %>% filter(index %!in% rt$index)
sort_BG <- Joy_data_BG %>% filter(index %!in% rt_BG$index)

### Check distribution of each variable
levels_BG <- colnames(BG)
Joy_data_PK <- as.data.frame(as.data.frame(PK_scaled_row) %>% gather(Time,PK))
BG_scaled_row <- as.data.frame(BG_scaled_row)
Joy_data_BG <- as.data.frame(BG_scaled_row %>% gather(Time,PK))
Joy_data_BG$Time <- as.factor(Joy_data_BG$Time)
Joy_data_BG$Time <- factor(Joy_data_BG$Time,levels = levels_BG)

Joy_data_PK$Time <- as.factor(Joy_data_PK$Time)
Joy_data_PK$Time <- factor(Joy_data_PK$Time,levels = levels_BG)

ggplot(data = Joy_data_BG,aes(x = PK, y = Time)) + geom_density_ridges()
ggplot(data = Joy_data_PK,aes(x = PK, y = Time)) + geom_density_ridges()

ggplot(data = sort_BG,aes(x = PK, y = as.factor(Time))) + geom_density_ridges()
ggplot(data = Joy_data_PK,aes(x = PK, y = Time)) + geom_density_ridges()

boxplot(as.matrix(PK[,9]))

estim.regul(PK, BG, grid1 = seq(0.001, 1, length=50), grid2 = seq(0.001, 1, length = 50), plt=TRUE) 

obj_rcc <- rcc(PK,BG,lambda1=lambda,lambda2=lambda)
obj_rcc$cor
plt.cc(obj_rcc,d1 = 1,d2 = 2,var.label = T,type="v")
title("Correlation circle for Dim 3 and 4")
legend("topleft",c("PK","BG"),col=c("red","blue"),pch=c(20,20))

obj_cca_col <- cc(PK_scaled_col,BG_scaled_col)
sum(obj_cca_col$cor)
lambda <- 0.00000000000001
grid1 = seq(0.000001, 0.1, length = 10)
grid2 = seq(0.0001, 0.1, length = 10)

tuned <- tune.rcc(PK_scaled_row,BG_scaled_row,grid1 = grid1,grid2 = grid1, validation = "Mfold",folds = 100)
catuned$opt.lambda1 # 1e-06
tuned$opt.lambda2 # 0.01
tuned$opt.score # 0.755

obj_rcc <- rcc(X=PK_scaled_row,Y = BG_scaled_row,lambda1 = 1e-06,lambda2 = 0.01)
obj_cca <- cc(PK_scaled_row[,-16],BG_scaled_row[,-16])

plt.cc(obj_cca,var.label = T)
sum(obj_rcc$cor)
sum(obj_cca$cor)
obj_cca$xcoef
plt.cc(obj_rcc,d1 = 1,d2 = 2,var.label = T)
title("Correlation onto Dim 1,2 canonical variate")
legend("center",c("PK","BG"),col=c("red","blue"),pch=c(20,20))

plt.cc(obj_rcc,d1 = 3,d2 = 4,var.label = T)
title("Correlation onto Dim 3,4 canonical variate")
legend("center",c("PK","BG"),col=c("red","blue"),pch=c(20,20))

plt.cc(obj_rcc,d1 = 5,d2 = 6,var.label = T)
title("Correlation onto Dim 5,6 canonical variate")
legend("center",c("PK","BG"),col=c("red","blue"),pch=c(20,20))




method = "shrinkage"


dat <- data.frame(cor = cc1$cor, dim = seq(1,16))
ggplot(data = dat,aes(x = dim,y = cor)) + geom_bar(stat = "identity") + theme_minimal() + ylab("Canonical Correlation") + xlab("Dimensions")+ggtitle("Canonical correlations") + 
  scale_y_continuous(breaks=seq(0,1,len =5))

## plot data
#ggpairs(PK.scaled)

correl <- matcor(PK.scaled,BG.scaled)
img.matcor(correl, type = 2)
## Correlations
matcor(PK.scaled, BG.scaled)
cc1 <- cc(PK,BG)
dat <- data.frame(cor = cc1$cor, dim = seq(1,16))
ggplot(data = dat,aes(x = dim,y = cor)) + geom_bar(stat = "identity") + theme_minimal() + ylab("Canonical Correlation") + xlab("Dimensions")+ggtitle("Canonical correlations") + 
  scale_y_continuous(breaks=seq(0,1,len =5))

cc2 <- cc(PK_scaled_row[,-16],BG_scaled_row[-16])
plt.cc(cc2, d1 = 1, d2 = 2,var.label = TRUE,type="v")
title("Correlation circle for Dim 1 and 2")
legend("topleft",c("PK","BG"),col=c("red","blue"),pch=c(20,20))


plt.cc(cc2,d1 = 3,d2 = 4,var.label = T,type="v")
title("Correlation circle for Dim 3 and 4")
legend("topleft",c("PK","BG"),col=c("red","blue"),pch=c(20,20))

plt.cc(cc2,d1 = 5,d2 = 6,var.label = T,type="v")
title("Correlation circle for Dim 5 and 6")
legend("topleft",c("PK","BG"),col=c("red","blue"),pch=c(20,20))


legend("topleft",c("PK","BG"),col=c("red","blue"),pch=c(20,20))
title("Correlation Circle",line=-0.8)
cc2 <- comput(PK, BG, cc1)

# tests of canonical dimensions
rho <- obj_rcc$cor
## Define number of observations, number of variables in first set, and number of variables in the second set.
n <- dim(PK_scaled_row)[1]
p <- dim(PK_scaled_row)[2]
q <- dim(BG_scaled_row)[2]

## Calculate p-values using the F-approximations of different test statistics:
dr <- p.asym(rho, n, p, q, tstat = "Wilks") # 6 canonical variables under 0.05
dr1 <- p.asym(rho, n, p, q, tstat = "Hotelling") # 6 canonical varaibles under 0.05
dr2 <- p.asym(rho, n, p, q, tstat = "Pillai")# 6 canonical varaibles under 0.05
dr3 <- p.asym(rho, n, p, q, tstat = "Roy")# 6 canonical varaibles under 0.05


xtable(data.frame(Wilks = dr$p.value, Hotelling = dr1$p.value,Pillai = dr2$p.value))
## USe cancor

X <- as.matrix(PK.scaled)
Y <- as.matrix(BG.scaled)   # the aptitude/ability variables)

cc <- cancor(X, Y, set.names= c("PK","BG"))

plot(cc)
# exercise some options
plot(cc, smooth=TRUE, id.n=3, ellipse.args=list(fill=TRUE))










############### ====================== TRY AGAIN ================= #############################################
# Fix PK data
Joy_data_PK <- as.data.frame(PK%>% gather(Time,PK))
Joy_data_PK$index <- seq(1,length(Joy_data_PK$Time))
Joy_data_PK$Time <- as.factor(Joy_data_PK$Time)
Joy_data_PK$Time<- factor(Joy_data_PK$Time,levels = levels_BG)
# fix BG data
Joy_data_BG <- as.data.frame(BG%>% gather(Time,BG))
Joy_data_BG$index <- seq(1,length(Joy_data_BG$Time))
Joy_data_BG$Time <- as.factor(Joy_data_BG$Time)
Joy_data_BG$Time <- factor(Joy_data_BG$Time,levels = levels_BG)

# raw PK multivariate distributions:
p1 <- ggplot(Joy_data_PK,aes(x=Time,y = PK,group=Time)) + geom_boxplot() + theme_bw() + ggtitle("Insulin concentration") + ylab("Concentration [pm]")


# raw BG multivariate distributions:
p2 <- ggplot(Joy_data_BG,aes(x=Time,y = BG,group=Time)) + geom_boxplot() + theme_bw() + ggtitle("Blood glucose concentration") + ylab("Concentration [mmol/L]") 

grid.arrange(p1,p2,ncol=2)
qqplot.data(as.data.frame(BG[,3]))


# raw PK multivariate distributions:
p11 <- ggplot(Joy_data_PK_row,aes(x=Time,y = PK,group=Time)) + geom_boxplot() + theme_bw() + ggtitle("Insulin concentration") + ylab("Concentration [pm]")


# raw BG multivariate distributions:
p22 <- ggplot(Joy_data_BG_row,aes(x=Time,y = BG,group=Time)) + geom_boxplot() + theme_bw() + ggtitle("Blood glucose concentration") + ylab("Concentration [mmol/L]") 

grid.arrange(p11,p22,ncol=2)
qqplot.data(as.data.frame(BG[,3]))

p111 <- ggplot(Joy_data_PK_col,aes(x=Time,y = PK,group=Time)) + geom_boxplot() + theme_bw() + ggtitle("Insulin concentration") + ylab("Concentration [pm]")


# raw BG multivariate distributions:
p222 <- ggplot(Joy_data_BG_col,aes(x=Time,y = BG,group=Time)) + geom_boxplot() + theme_bw() + ggtitle("Blood glucose concentration") + ylab("Concentration [mmol/L]") 


grid.arrange(p111,p222,ncol=2)

### Plot distribution of variables on each scaling for PK:
ggplot(Joy_data_PK,aes(sample=PK,group=Time))+ stat_qq()+stat_qq_line() + facet_wrap(~Time,scales = "free") + ggtitle("QQplot no scaling")
ggplot(Joy_data_PK_row,aes(sample=PK,group=Time))+ stat_qq()+stat_qq_line() + facet_wrap(~Time,scales = "free") + ggtitle("QQplot row-wise scaling")
ggplot(Joy_data_PK_col,aes(sample=PK,group=Time))+ stat_qq()+stat_qq_line() + facet_wrap(~Time,scales = "free")
### Plot distribution of variables on each scaling for BG:
ggplot(Joy_data_BG,aes(sample=BG,group=Time))+ stat_qq()+stat_qq_line() + facet_wrap(~Time,scales = "free")
ggplot(Joy_data_BG_row,aes(sample=BG,group=Time))+ stat_qq()+stat_qq_line() + facet_wrap(~Time,scales = "free")
ggplot(Joy_data_BG_col,aes(sample=BG,group=Time))+ stat_qq()+stat_qq_line() + facet_wrap(~Time,scales = "free")



### Multilineraity
PK_PCA <- PCA(PK_scaled_row,scale.unit = F,graph = F)
BG_PCA <- PCA(BG_scaled_row,scale.unit = F,graph = F)

eig.val_PK <- get_eigenvalue(PK_PCA)
eig.val_BG <- get_eigenvalue(BG_PCA)

PK_scaled_row_new <- PK_scaled_row
PK_scaled_row_new[,15] <- (PK_scaled_row[,15]+PK_scaled_row[,16])/2
PK_scaled_row_new <- PK_scaled_row_new[,-16]
PK_PCA_new <- PCA(PK_scaled_row_new,scale.unit = F,graph = F)
eig.val_PK_new <- get_eigenvalue(PK_PCA_new)
pairs(PK_scaled_row_new)

PK_1516 <- data.frame(var15 = PK_scaled_row[,15],var16=PK_scaled_row[,16])
BG_1516 <- data.frame(var15 = BG_scaled_row[,15],var16=BG_scaled_row[,16])

plot(PK_1516$var15[1:100],type="l",main ="Time 240 and Time 300 first 100 obs PK",ylab="Insulin Concentration (scaled)")
lines(PK_1516$var16[1:100],type="l",col = "red")
legend("topright",c("240","300"),col=c(1,2),lty = c(1,1))


plot(BG_1516$var15[1:100],type="l",main ="Time 240 and Time 300 first 100 obs BG",ylab="Blood sugar Concentration (scaled)")
lines(BG_1516$var16[1:100],type="l",col = "red")
legend("topright",c("240","300"),col=c(1,2),lty = c(1,1))


plot(PK$`240`[1:100],type="l")
lines(PK$`300`[1:100],type="l",col="red")

cor(PK$`240`,PK$`300`)
cor(PK_scaled_col$`240`,PK_scaled_col$`300`)
cor(BG_scaled_col$`240`,BG_scaled_col$`300`)

cor(BG_1516$var15,BG_1516$var16)

cor(BG_scaled_row[,5],BG_scaled_row[,6])


cc4 <- cc(PK_scaled_row[,-c(1,10,11,12)],BG_scaled_row[,-c(3,4,5,15)])
plt.cc(cc4, d1 = 1, d2 = 2,var.label = TRUE,type="v")
title("Correlation circle for Dim 1 and 2")
legend("topleft",c("PK","BG"),col=c("red","blue"),pch=c(20,20))

plt.cc(cc4, d1 = 3, d2 = 4,var.label = TRUE,type="v")
title("Correlation circle for Dim 1 and 2")
legend("topleft",c("PK","BG"),col=c("red","blue"),pch=c(20,20))


plt.cc(cc4, d1 = 5, d2 = 6,var.label = TRUE,type="v")
title("Correlation circle for Dim 1 and 2")
legend("topleft",c("PK","BG"),col=c("red","blue"),pch=c(20,20))


plt.cc(cc4, d1 = 7, d2 = 8,var.label = TRUE,type="v")
title("Correlation circle for Dim 1 and 2")
legend("topleft",c("PK","BG"),col=c("red","blue"),pch=c(20,20))





############ Test

f1 <- rnorm(100,1,0.00001)
f2 <- rnorm(100,1,0.00001)
j1 <- rnorm(100,1,0.00001)
j2 <- rnorm(100,1,0.00001)

y1 = f1+f2+j2
y2 = f1+f2+j1
x1 = j1+j2+f2
x2 = j1+j2+f1

X_f <- data.frame(x1,x2);Y_f <- data.frame(y1,y2)

cor(X_f,Y_f)
cc(X_f,Y_f)


plt.cc(cc5, d1 = 1, d2 = 2,var.label = TRUE,type="v")
title("Correlation circle for Dim 1 and 2")
legend("topleft",c("PK","BG"),col=c("red","blue"),pch=c(20,20))

plt.cc(cc5, d1 = 1, d2 = 2,var.label = TRUE,type="v")
title("Correlation circle for Dim 1 and 2")
legend("topleft",c("PK","BG"),col=c("red","blue"),pch=c(20,20))

