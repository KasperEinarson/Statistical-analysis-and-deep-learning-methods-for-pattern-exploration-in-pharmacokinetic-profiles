## Get the right data:
# Load libraries
library(ggplot2)
library(tidyr)
library(readr)
library(gridExtra)
library(RGCCA)
library(xtable)
library(ggjoy)
library(zoo)
library(CCA)
library(Rtsne)
library(FactoMineR)
library(ggplot2)
library(factoextra)
set.seed(123)
library(ggridges)
library(Rtsne)
library(GGally)
library(dtw)
library(dtwclust)
library(CCA)
library(inline)
library(corrplot)
library(pheatmap)
library(magrittr)
library(lubridate)
library(CCP)
library(gghighlight)
library(devtools)
#library(mixOmics)
#library(ggiraphExtra)
library(Rcpp)
library(dplyr)
'%!in%' <- function(x,y)!('%in%'(x,y))
## OR maybe here..
prof <- read_csv("C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/Foerste dataudtraek/0014_pig_profiles.csv")
param <- read_csv("C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/Foerste dataudtraek/0014_pig_parameters.csv")
param$ID <- paste0(param$Treatment,param$Study,param$Animal,param$BW)
# Make unique ID
prof$ID <- paste0(prof$Treatment,prof$Study,prof$Animal,prof$BW)
prof <- as.data.frame(prof)
# Remove time observations that we dont want!
prof <- prof %>% filter(!is.na(Time))
prof <- prof %>% filter(Time != -1)
prof <- prof %>% filter(Time != 150)
## Remove IDs we cant find
gem <- prof %>% group_by(ID) %>% tally() %>% filter(n != 16)
# Throw away "Ahare120803160.5" since it only has repeated measurements for time 3 and 6 and then single measurements. 
gem1 <- data.frame(gem[-8,],reps = gem[-8,]$n/16)
# Check that this will work:
(dim(prof)[1] - dim(temp)[1]) - (16*sum(gem1$reps)) # gives 0 - yes - check. 
# Start:
temp <- prof %>% filter(ID %!in% gem$ID)

start1 <- prof %>% filter(ID == as.character(gem1[1,1]))
start11 <- start1[1:16,]; start11$ID <- paste0(start11$ID,"1")
start12 <- start1[17:32,]; start12$ID <- paste0(start12$ID,"2")


start2 <- prof %>% filter(ID == as.character(gem1[2,1]))
start21 <- start2[seq(1,32,2),]; start21$ID <- paste0(start21$ID,"1")
start22 <- start2[seq(2,32,2),]; start22$ID <- paste0(start22$ID,"2")

start3 <- prof %>% filter(ID == as.character(gem1[3,1]))
start31 <- start3[seq(1,32,2),]; start31$ID <- paste0(start31$ID,"1")
start32 <- start3[seq(2,32,2),]; start32$ID <- paste0(start32$ID,"2")

start4 <- prof %>% filter(ID == as.character(gem1[4,1]))
start41 <- start4[seq(1,48,3),]; start41$ID <- paste0(start41$ID,"1")
start42 <- start4[seq(2,48,3),]; start42$ID <- paste0(start42$ID,"2")
start43 <- start4[seq(3,48,3),]; start43$ID <- paste0(start43$ID,"3")


start5 <- prof %>% filter(ID == as.character(gem1[5,1]))
start51 <- start5[seq(1,48,3),]; start51$ID <- paste0(start51$ID,"1")
start52 <- start5[seq(2,48,3),]; start52$ID <- paste0(start52$ID,"2")
start53 <- start5[seq(3,48,3),]; start53$ID <- paste0(start53$ID,"3")

start6 <- prof %>% filter(ID == as.character(gem1[6,1]))
start61 <- start6[seq(1,32,2),]; start61$ID <- paste0(start61$ID,"1")
start62 <- start6[seq(2,32,2),]; start62$ID <- paste0(start62$ID,"2")


start7 <- prof %>% filter(ID == as.character(gem1[7,1]))
start71 <- start7[seq(1,32,2),]; start71$ID <- paste0(start71$ID,"1")
start72 <- start7[seq(2,32,2),]; start72$ID <- paste0(start72$ID,"2")

start8 <- prof %>% filter(ID == as.character(gem1[8,1]))
start81 <- start8[seq(1,32,2),]; start81$ID <- paste0(start81$ID,"1")
start82 <- start8[seq(2,32,2),]; start82$ID <- paste0(start82$ID,"2")

start9 <- prof %>% filter(ID == as.character(gem1[9,1]))
start91 <- start9[seq(1,32,2),]; start91$ID <- paste0(start91$ID,"1")
start92 <- start9[seq(2,32,2),]; start92$ID <- paste0(start92$ID,"2")

start10 <- prof %>% filter(ID == as.character(gem1[10,1]))
start101 <- start10[seq(1,32,2),]; start101$ID <- paste0(start101$ID,"1")
start102 <- start10[seq(2,32,2),]; start102$ID <- paste0(start102$ID,"2")


start_el<- prof %>% filter(ID == as.character(gem1[11,1]))
start_el1 <- start_el[seq(1,48,3),]; start_el1$ID <- paste0(start_el1$ID,"1")
start_el2 <- start_el[seq(2,48,3),]; start_el2$ID <- paste0(start_el2$ID,"2")
start_el3 <- start_el[seq(3,48,3),]; start_el3$ID <- paste0(start_el3$ID,"3")

start_to<- prof %>% filter(ID == as.character(gem1[12,1]))
start_to1 <- start_to[seq(1,48,3),]; start_to1$ID <- paste0(start_to1$ID,"1")
start_to2 <- start_to[seq(2,48,3),]; start_to2$ID <- paste0(start_to2$ID,"2")
start_to3 <- start_to[seq(3,48,3),]; start_to3$ID <- paste0(start_to3$ID,"3")

re <- rbind(start11,start12,start21,start22,start31,start32,start41,start42,start43,start51,
            start52,start53,start61,start62,start71,start72,start81,start82,start91,start92,
            start101,start102,start_el1,start_el2,start_el3,start_to1,start_to2,start_to3)

pro <- rbind(temp,re)

## Handle missing values:
pro$BG <- na.approx(pro$BG)
pro$PK[is.na(pro$PK)] <- 0
##################### Investigate study for flow chart ===========================================
dim(pro)
fg <- pro %>% group_by(Study) %>% summarise(uni = length(unique(Treatment)),n = n(),rep = n/uni/16/8)
fg <- fg[-1,]
df <- pro %>% filter(Study == "hare120404")
#View(df)

pro %>% group_by(Study) %>% tally()
############ ======================  WIDE FORMAT ===================================================0
Wide_for_cor <- pro %>% select(Time,PK,BG) %>%
  gather(variable, value, -(Time)) %>%
  unite(temp, Time,variable) %>%
  group_by(temp) %>%
  mutate(id=1:n()) %>% 
  spread(temp, value)

########### ====================== DIVIDE INTO PK AND BG ==============================================
Wide <- Wide_for_cor
PK <- Wide[, seq(1,33,2)]
PK <- PK[,-1]
PK <- PK[,c(8,12,15,2,4,6,9,11,13,14,16,1,3,5,7,10)]
colnames(PK) <- c("3","6","9","12","15","20","30","45","60","75","90","105","120","180","240","300")
BG <- Wide[, seq(2,32,2)]
BG <- BG[,c(8,12,15,2,4,6,9,11,13,14,16,1,3,5,7,10)]
colnames(BG) <- c("3","6","9","12","15","20","30","45","60","75","90","105","120","180","240","300")
####### ==================================  ROW SCALE   ==============================================================
PK_scaled_row <- t(scale(t(PK)))
BG_scaled_row <- t(scale(t(BG)))
##### ====================================UNDER SAMPLE A AND B ==========================================================================
TreatA <- pro %>% filter(Treatment == "A") %>% select(Study) %>% unique() 
TreatB <- pro %>% filter(Treatment == "B") %>% select(Study) %>% unique() 


TAS <- pro %>% filter(Treatment == "A",Study %in% TreatA$Study)
TBS <- pro %>% filter(Treatment == "B",Study %in% TreatB$Study)

#futA <- sample(unique(TAS$Study),1,replace = F)
#futB <- sample(unique(TBS$Study),1,replace = F)

pro.usample <- bind_rows(
  TAS%>% filter(Study %in% "hare121006"),
  TBS%>% filter(Study %in% "hare130405"),
  pro %>% filter(Treatment != "A",Treatment != "B"))


######## =================================WIDE FORMAT UNDERSAMPLED ============================================

Wide_for_cor.u <- pro.usample %>% select(Time,PK,BG,Treatment,ID) %>%
  gather(variable, value, -c(Time,Treatment,ID)) %>%
  unite(temp, Time,variable) %>%
  group_by(temp) %>%
  mutate(id=1:n()) %>% 
  spread(temp, value)


Wide_test <- pro.usample %>% select(Time,PK,BG,Treatment,ID,BW,Dose) %>%
  gather(variable, value, -c(Time,Treatment,ID,BW,Dose)) %>%
  unite(temp, Time,variable) %>%
  group_by(temp) %>%
  mutate(id=1:n()) %>% 
  spread(temp, value)


PK.u <- Wide_for_cor.u[, seq(5,35,2)]
PK.u <- PK.u[,c(8,12,15,2,4,6,9,11,13,14,16,1,3,5,7,10)]
colnames(PK.u) <- c("3","6","9","12","15","20","30","45","60","75","90","105","120","180","240","300")
PK.u[is.na(PK.u)] <- 0

BG.u <- Wide_for_cor.u[, seq(4,34,2)]
BG.u <- BG.u[,c(8,12,15,2,4,6,9,11,13,14,16,1,3,5,7,10)]
colnames(BG.u) <- c("3","6","9","12","15","20","30","45","60","75","90","105","120","180","240","300")


####### ========================
PK.u_scaled_row <- t(scale(t(PK.u)))
BG.u_scaled_row <- t(scale(t(BG.u)))


# Remember what treatment it came from:
Treat <- Wide_for_cor.u[, 1]
####### Split:
Tr <- seq(1:length(PK.u_scaled_row[,1]))
#IDD <- sample(1:length(PK.u_scaled_row[,1]),round((length(PK.u_scaled_row[,1])/100)*70))
#write.table(IDD,"C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/R code/Solid ID/ID_train.txt")
IDD <- read.table("C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/R code/Solid ID/ID_train.txt")

ID1 <- IDD$x[-which(IDD$x %in% c(1722))]
Tr1 <- Tr[-ID1]
ID2 <- Tr1[1:round(length(Tr1)/2)]
ID3 <- Tr1[((round(length(Tr1)/2))+1):length(Tr1)]

PK_train <- PK.u_scaled_row[ID1,]
PK_val <- PK.u_scaled_row[ID2,]
PK_test <- PK.u_scaled_row[ID3,]


BG_train <- BG.u_scaled_row[ID1,]
BG_val <- BG.u_scaled_row[ID2,]
BG_test <- BG.u_scaled_row[ID3,]


PK_TV <- rbind(PK_train,PK_val)
BG_TV <- rbind(BG_train,BG_val)

PK_train_T <- cbind(PK_train,Treatment = Treat[ID1,])
PK_val_T <- cbind(PK_val,Treatment = Treat[ID2,])
PK_test_T <- cbind(PK_test,Treatment = Treat[ID3,])

BG_train_T <- cbind(BG_train,Treatment = Treat[ID1,])
BG_val_T <- cbind(BG_val,Treatment = Treat[ID2,])
BG_test_T <- cbind(BG_test,Treatment = Treat[ID3,])

## Export this:
PK_TV_T <- rbind(PK_train_T,PK_val_T)
BG_TV_T <- rbind(BG_train_T,BG_val_T)

# Make database of Treatment A and B that can be augmented into PK and BG train_val

AB <- rbind(TAS,TBS)

AB1 <- AB %>% select(Time,PK,BG,Treatment,ID) %>%
  gather(variable, value, -c(Time,Treatment,ID)) %>%
  unite(temp, Time,variable) %>%
  group_by(temp) %>%
  mutate(id=1:n()) %>% 
  spread(temp, value)


AB1[is.na(AB1)] <- 0

PK.up_AB <- AB1[, seq(5,35,2)]
PK.up_AB <- PK.up_AB[,c(8,12,15,2,4,6,9,11,13,14,16,1,3,5,7,10)]
colnames(PK.up_AB) <- c("3","6","9","12","15","20","30","45","60","75","90","105","120","180","240","300")

BG.up_AB <- AB1[, seq(4,34,2)]
BG.up_AB <- BG.up_AB[,c(8,12,15,2,4,6,9,11,13,14,16,1,3,5,7,10)]
colnames(BG.up_AB) <- c("3","6","9","12","15","20","30","45","60","75","90","105","120","180","240","300")



###### ================================================================WRITE for python and matlab==================================================
write.table(PK_train,"C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/PK_train.txt")
write.table(PK_val,"C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/PK_val.txt")
write.table(PK_test,"C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/PK_test.txt")


write.table(BG_train,"C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/BG_train.txt")
write.table(BG_val,"C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/BG_val.txt")
write.table(BG_test,"C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/BG_test.txt")

write.table(PK_TV,"C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/PK_TV.txt")
write.table(BG_TV,"C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/BG_TV.txt")

write.table(PK.u_scaled_row,"C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/PK_HER.txt")
write.table(BG.u_scaled_row,"C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/BG_HER.txt")


write.table(PK.u,"C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/PK_IKKE_GJORT_NOGET.txt")
write.table(BG.u,"C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/BG_IKKE_GJORT_NOGET.txt")

############# LOAD FUNCTIONS 


quantile.plot2 <- function(x, Means = NULL, main, obs = NULL,ymax=3,ymin=0, ...)#, yside = 2, 
{
  nx <- x
  plot(x = c(1,ncol(nx)), y = c(ymin,ymax), main = main,xlab = "Time",ylab="Respons")
  abline(v = 1:ncol(nx), lty = 3, col = "grey")
  for(i in 1:floor(nrow(nx)/2))
    polygon(x = c(1:ncol(nx),ncol(nx):1), y = c(nx[i,],rev(nx[nrow(nx)-i+1,])),
            col = grey(1 - i/ceiling(nrow(nx)/2+1)), border = grey(1 - i/ceiling(nrow(nx)/2+1)))
  if(is.null(Means)) {
    Means <- apply(nx, 2, mean)
    lines(x = 1:ncol(nx), y = Means, col = "red", lwd = 2)
  }
  if(!is.null(obs)) lines(obs, type = "b", lwd = 2, col = "blue")
  legend("topright","Mean",lty=1,col="red")
}


Plot_quantile2 <- function(Data,main = ""){
  Data_d <- Data %>% group_by(Time) %>% summarise(kvartil1 = quantile(Var,0.025),
                                                  kvartil2 = quantile(Var,0.1),
                                                  kvartil3 = quantile(Var,0.15),
                                                  kvartil4 = quantile(Var,0.2),
                                                  kvartil5 = quantile(Var,0.25),
                                                  kvartil6 = quantile(Var,0.3),
                                                  kvartil7 = quantile(Var,0.35),
                                                  kvartil8 = quantile(Var,0.4),
                                                  kvartil9 = quantile(Var,0.45),
                                                  kvartil10 = quantile(Var,0.475),
                                                  kvartil11 = quantile(Var,0.525),
                                                  kvartil12 = quantile(Var,0.55),
                                                  kvartil13 = quantile(Var,0.6),
                                                  kvartil14 = quantile(Var,0.65),
                                                  kvartil15 = quantile(Var,0.70),
                                                  kvartil16 = quantile(Var,0.75),
                                                  kvartil17 = quantile(Var,0.8),
                                                  kvartil18 = quantile(Var,0.85),
                                                  kvartil19 = quantile(Var,0.9),
                                                  kvartil20 = quantile(Var,0.975)
                                                  
                                                  
                                                  
                                                  
                                                  
  )
  
  
  Data_d1 <- Data_d[,-1]
  Data_d2 <- t(Data_d1)
  return(Data_d2)
}


