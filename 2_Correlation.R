### Investigate correlation between PK and BG
# begin by plotting what we are looking at
source("C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/R code/Data_prep.R")
rr1 <- ggplot(data = pro,aes(x = Time,y = PK,group=index,col = index)) + geom_line()+ theme(legend.position="none") + xlab("Time[min]") + ylab("Concentration[pm]") + ggtitle("Insulin Concentration Profiles") 
rr2 <- ggplot(data = pro,aes(x = Time,y = BG,group=index,col = index)) + geom_line()+ theme(legend.position="none") + xlab("Time[min]") + ylab("Concentration[mmol/L]") + ggtitle("Blood Sugar Concentration Profiles")
grid.arrange(rr1,rr2,ncol = 2)

## Calculate cross correlation between each rep for PK and BG
crosscor <- pro %>% group_by(index) %>% summarise(crossc = ccf(PK,BG,lag.max = 0,na.action = na.pass,plot = F)$acf[1])

Rep1 <- pro %>% filter(index == 1)
corr <- ccf(Rep1$PK,Rep1$BG,lag.max = 0)
corr$acf[1]
### Investigate correlation within PK and BG
## plot data
#ggpairs(PK)
## 
#ggpairs(BG)
### Correlation matrices:
correl <- matcor(PK.scaled,BG.scaled)
img.matcor(correl, type = 1)
PK_cor <- round(correl$Xcor,2)
BG_cor <- round(correl$Ycor,2)

corrplot(PK_cor, type = "upper", 
         tl.col = "black", tl.srt = 45)
title("Correlation in time for insulin concentration", line = 1)

corrplot(BG_cor, type = "upper", 
         tl.col = "black", tl.srt = 45)
title("Correlation in time for blood sugar conc.", line = 1)

## Cross correlation
PK.scaled1 <- PK.scaled[,c(3,4,5,6,1,2,13,14,15,16,7,8,9,11,12,10)]
BG.scaled1 <- BG.scaled[,c(5,6,1,2,3,4,11,12,13,16,14,15,7,8,9,10)]
R <- cor(PK.scaled, BG.scaled)
colnames(R) <- paste0(colnames(R),".BG")

rownames(R) <- paste0(rownames(R),".PK")
pheatmap(R,cluster_rows=F, cluster_cols=F,main = "Cross-correlation Insulin(PK)-Bloodglucose(BG)")
## Correlation within each DAtaset
PK.u <- t(as.tbl(Wide.u) %>% select(ends_with("PK")))
PK.uu <- PK.u[c(8,12,15,2,4,6,9,11,13,14,16,1,3,5,7,10),]
#corr <- correlationTable(PK.uu[,1:100])
View(R)

View(PK.uu)


matplot(t(BG.scaled[1:20,]),type="l")
matplot(t(BG[1:20,]),type="l")


#write.table(PK.scaled,"C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/PK_scal.txt")
#write.table(BG.scaled,"C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/BG_scal.txt")


gf <- matrix(rep(hist(rnorm(4000,0,1))$counts[1:16],2071),ncol=16)

gf2 <- matrix(rep(hist(rnorm(4000,0,1))$counts[1:16],2071),ncol=16)

matplot(gf[1:20,],type="l")
cor(gf[,2],gf2[,2])
BG2_random <- matrix(runif(2071*16,0,1),ncol = 16)

R <- cor(PK_standard, BG_standard)
colnames(R) <- paste0(colnames(R),".BG")

rownames(R) <- paste0(rownames(R),".PK")
pheatmap(R,cluster_rows=F, cluster_cols=F,main = "Cross-correlation correlated data")

write.table(gf,"C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/PK_cor.txt")
write.table(gf2,"C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/BG_cor.txt")

#write.table(PK_ones,"C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/PK_ones.txt")
#write.table(BG_ones,"C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/BG_ones.txt")


############## ============================ HER CROSS ROW BASED ############## ====================================
### Correlation matrices:
correl <- matcor(PK_scaled_row,BG_scaled_row)
img.matcor(correl, type = 1)
PK_cor <- round(correl$Xcor,2)
BG_cor <- round(correl$Ycor,2)

corrplot(PK_cor, type = "upper", 
         tl.col = "black", tl.srt = 45)
title("Correlation in time for insulin concentration", line = 3.2)

corrplot(BG_cor, type = "upper", 
         tl.col = "black", tl.srt = 45)
title("Correlation in time for blood sugar conc.", line = 3.2)

## Cross correlation
PK.scaled1 <- PK_scaled_row[,c(3,4,5,6,1,2,13,14,15,16,7,8,9,11,12,10)]
BG.scaled1 <- BG_scaled_row[,c(5,6,1,2,3,4,11,12,13,16,14,15,7,8,9,10)]
R <- cor(PK_scaled_row, BG_scaled_row)
colnames(R) <- paste0(colnames(R),".BG")

rownames(R) <- paste0(rownames(R),".PK")
pheatmap(R,cluster_rows=F, cluster_cols=F,main = "Cross-correlation Insulin(PK)-Bloodglucose(BG)")

