  # Load libraries
  library(ggplot2)
  library(tidyr)
  library(readr)
  library(gridExtra)
  library(RGCCA)
  library(xtable)
  library(ggjoy)
  
  library(CCA)
  library(Rtsne)
  library(FactoMineR)
  library(ggplot2)
  library(factoextra)
  
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
  library(mixOmics)
  #library(ggiraphExtra)
  library(Rcpp)
  library(dplyr)
  '%!in%' <- function(x,y)!('%in%'(x,y))
  ## OR maybe here..
  prof <- read_csv("C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/Foerste dataudtraek/0014_pig_profiles.csv")
  prof$seq <- seq(1,length(prof$Treatment))
  prof$ID <- paste0(prof$Treatment,prof$Animal,prof$Study)
  prof <- as.data.frame(prof)
  prof2 <- prof
  prof <- prof %>% filter(!is.na(Time))
  
  # Observations from time -1 all have NA in PK
  prof1 <- prof
  prof <- prof %>% filter(Time != -1)
  
  ff <- prof %>% group_by(Treatment,Animal,Study,Time) %>% tally()
  ones <- ff %>% filter(n == 1) %>% select(Study)
  
  re <- prof %>% filter(Study %in% unique(ones$Study))
  
  for ( i in 2:length(re$Treatment)){
    if(re$Time[i] == re$Time[i-1]){
      print(i) 
    }
  }
  ## FINd the dublicates that we dont want!
  profe <- prof %>% filter(seq %!in% c(16088,16091))
  profe %>% group_by(Time) %>% tally()
  profe <- profe %>% filter(Time != 150)
  profe %>% group_by(Time) %>% tally()
  ff <- profe %>% group_by(Treatment,Animal,Study,Time) %>% tally()
  ones <- ff%>% filter(n == 1)
  ones$ID <- paste0(ones$Treatment,ones$Animal,ones$Study)
  
  re1 <- profe %>% filter(ID %in% unique(ones$ID))
  re1$index <- rep(1:920,each = 16)
  
  ########## FOR REPLICATES 2
  twos <- ff %>% filter(n == 2)
  twos$ID <- paste0(twos$Treatment,twos$Animal,twos$Study)
  
  re2 <- profe %>% filter(ID %in% unique(twos$ID))
  
  qw <- re2 %>% group_by(Treatment,Animal,Study,Time) %>% tally()
  
  for ( i in 2:length(re2$Treatment)){
    if(re2$Time[i] == re2$Time[i-1]){
      print(i) 
    } 
  }
  
  re2$index <- rep((1:1054)+920,each=16)
  ## For replicates more than 2
  more <- ff %>% filter(n > 2)
  more$ID <- paste0(more$Treatment,more$Animal,more$Study)
  re3 <- profe %>% filter(ID %in% unique(more$ID))
 
  
  
  hej <- rep(0,nrow(re3))
  for ( i in 2:nrow(re3)){
    if(re3$Time[i] == re3$Time[i-1]){
      hej[i] <- i 
    }
  }
  hej <- as.data.frame(hej[which(hej != 0)])
  
  
  re4 <- re3[1:1024,]
  
  re4$index <-rep((1:64)+1974,each = 16)
  
  
 
  

  
  re5 <- re3[1025:nrow(re3),] # Hvad er tilbage
  
  re6 <- re5[1:128,] # Første 128 rækker af det
  re61 <- re6[c(TRUE, FALSE),]
  re61$index <- rep((1:4)+2038,each=16)
  re62 <- re6[c(FALSE, TRUE),]
  re62$index <- rep((1:4)+2042,each=16)
  
  re7 <- re5[129:(128+(16*4)),] # Først 192 af det
  re7$index <- rep((1:4)+2046,each=16)
  
  re8 <- re5[193:(192 + (4*16)),] # Først 256 af det
  re81 <-  re8[c(TRUE, FALSE, FALSE, FALSE),]
  re82 <- re8[c(FALSE, TRUE, FALSE, FALSE),]
  re83 <- re8[c(FALSE, FALSE, TRUE, FALSE),]
  re84 <- re8[c(FALSE, FALSE, FALSE, TRUE),]
  
  re81$index <- 2051
  re82$index <- 2052
  re83$index <- 2053
  re84$index <- 2054
  re9 <- re5[257:(256 + 4*16),]
  re9$index <- rep((1:4)+2054,each=16)
  re10 <- re5[321:(321+191),]
  re101 <-  re10[c(TRUE, FALSE, FALSE),]
  re102 <- re10[c(FALSE, TRUE, FALSE),]
  re103 <- re10[c(FALSE, FALSE, TRUE),]
  re101$index <- 2059
  re102$index <- 2060
  re103$index <- 2061
  
  re11 <- re5[513:nrow(re5),]
  re11$index <- 2062
  
  pro <- bind_rows(re1,re2,re4,re61,re62,re7,re81,re82,re83,re84,re9,re101,re102,re103,re11)
  pro$index <- as.factor(pro$index)

    ################################ WIDE FOR CORRELATION ###############################################
  Wide_for_cor <- pro %>% select(Time,PK,BG) %>%
    gather(variable, value, -(Time)) %>%
    unite(temp, Time,variable) %>%
    group_by(temp) %>%
    mutate(id=1:n()) %>% 
    spread(temp, value)
dim(Wide_for_cor)

Wide_for_cor[is.na(Wide_for_cor)] <- 0
  Wide <- Wide_for_cor
  PK <- Wide[, seq(1,33,2)]
  PK <- PK[,-1]
  PK <- PK[,c(8,12,15,2,4,6,9,11,13,14,16,1,3,5,7,10)]
  colnames(PK) <- c("3","6","9","12","15","20","30","45","60","75","90","105","120","180","240","300")
  BG <- Wide[, seq(2,32,2)]
  BG <- BG[,c(8,12,15,2,4,6,9,11,13,14,16,1,3,5,7,10)]
  colnames(BG) <- c("3","6","9","12","15","20","30","45","60","75","90","105","120","180","240","300")

PK_scaled_row <- t(scale(t(PK)))
BG_scaled_row <- t(scale(t(BG)))
 
 
##
#write.table(PK[1:1656,],file="PK_train.txt")
#write.table(BG[1:1656,],file="BG_train.txt")
#write.table(BG[1657:2071,],file="PK_val.txt")
#write.table(BG[1657:2071,],file="BG_val.txt")

#write.table(PK.scaled,file="C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale Python/Try yourself/PK_scaled.txt")
#write.table(BG.scaled,file="C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale Python/Try yourself/BG_scaled.txt")

PK_col_scale <- scale(PK) 
BG_col_scale <- scale(BG)
PK_scaled_col <- scale(PK)
BG_scaled_col <- scale(BG)

write.table(PK_col_scale,file="C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale Python/Untitled Folder 1/PK_col_scale.txt")
write.table(BG_col_scale,file="C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale Python/Untitled Folder 1/BG_col_scale.txt")


PK_trans <- t(PK_scaled_row)
BG_trans <- t(BG_scaled_row) 

PK_uni <- matrix(runif(4500*16,0,1),nrow=4500,ncol=16)
BG_uni <- matrix(runif(4500*16,0,1),nrow=4500,ncol=16)

write.table(PK_uni,file="C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale Python/Untitled Folder 1/PK_uni.txt")
write.table(BG_uni,file="C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale Python/Untitled Folder 1/BG_uni.txt")

write.table(PK,file="C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale Python/Untitled Folder 1/PK_not.txt")
write.table(BG,file="C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale Python/Untitled Folder 1/BG_not.txt")

## UNDERSAMPLE A 

TreatA <- pro %>% filter(Treatment == "A") 
TreatB <- pro %>% filter(Treatment == "B") 

pro.usample <- bind_rows(
   pro%>% filter(index %in% sample(TreatA$index,3)),
   pro%>% filter(index %in% sample(TreatB$index,3)),
   pro %>% filter(Treatment != "A",Treatment != "B"))

length(unique(pro$IDID)) 
gem <- pro %>% group_by(IDID) %>% tally() %>% filter(n != 16)
pro_mid <- pro %>% filter(IDID %in% gem$IDID)
pro_uden <- pro %>% filter(IDID %!in% gem$IDID)

length(unique(pro$IDID))
length(unique(pro_uden$IDID))

for(i in 1:28){
pro_midmid <- pro_mid[1:16,]
pro_midmid$IDID <-paste0(pro_midmid$IDID,i) 
pro_uden <- rbind(pro_uden,pro_midmid)
pro_mid <- pro_mid[-c(1:16),]
}

length(unique(pro_uden$IDID))

pro_uden %>% group_by(IDID) %>% summarise(antal = n()) %>% filter(antal != 16)

pro%>% group_by(index) %>% summarise(antal = n()) %>% filter(antal != 16)
max(pro.uindex)

 Wide_for_cor.u <- pro.usample %>% select(Time,PK,BG) %>%
   gather(variable, value, -(Time)) %>%
   unite(temp, Time,variable) %>%
   group_by(temp) %>%
   mutate(id=1:n()) %>% 
   spread(temp, value)

 pro.usample[1,]

 #Wide_for_cor.u[is.na(Wide_for_cor.u)] <- 0
 
Wide.u <- Wide_for_cor.u[,-1]
View(pro.usample)
 
### 
Wide_all <- pro %>% select(Time,PK,BG) %>%
   gather(variable, value, -(Time)) %>%
   unite(temp, Time,variable) %>%
   group_by(temp) %>%
   mutate(index=1:n()) %>% 
   spread(temp, value)
#### 

rq <- pro %>% select(Treatment,Animal,index,Study,BW)
Merfed <- merge(Wide_all,rq,by.x = "index")

Wide.all <- Merfed %>% group_by(index) %>% slice(1)


ad <- pro %>% mutate(her = ifelse(Time >= 3 & Time <=120 & is.na(PK),1,0)) %>% filter(her ==1)

ad1 <- pro %>% filter(is.na(BG))

g1 <- ggplot(data = pro,aes(x = Treatment,y = n,fill = `NA`)) + geom_bar(stat = "identity") + geom_text(aes(y=n, label=n), vjust=1.6, 
                                                                                                           color="white", size=3.5)+
  scale_fill_brewer(palette="Paired")+
  theme_minimal() + ylab("Count") + ggtitle("Count of observations",subtitle = "For insulin concentration (PK)")

param <- read_csv("C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/Foerste dataudtraek/0014_pig_parameters.csv")
param$Date <- dmy(param$Date)
param$IDID <- paste0(param$Treatment,param$Study,param$Animal,param$BW)
param_nu <- param %>% select(IDID,Date)
pro$IDID <- paste0(pro$Treatment,pro$Study,pro$Animal,pro$BW)
length(pro$Treatment)
pro$index <-as.numeric(as.character(pro$index))


param$Animal <- as.factor(param$Animal)
param$StudyYear <- paste0(param$Study,"[",year(param$Date),"]")
param44 <- param %>% filter(StudyYear != "hare131203[2014]")


param1 <- param

param1[is.na(param1)] <- 0
tmp2 <- param %>% filter(StudyYear == "hare131203[2014]")

tmp2$StudyYear <- "hare131203[2013]"
param <- bind_rows(param44,tmp2)


# Merge
Total <- merge(pro,param_nu,by = "IDID")

test <- pro %>% arrange(index) %>% mutate(diff1 = c(diff(Time),1))  %>% select(diff1)
sum(test$diff1 %in% c(3,5,10,15,60,-297))



## 
par(mfrow=c(1,2))
matplot(t(PK_scaled_row[1:2071,]),type="l",main = "PK scaled concentration",xlab = "Time index",ylab="Scaled concentration")
matplot(t(BG_scaled_row[1:2071,]),type="l",main = "Blood glucose scaled concentration",xlab = "Time index",ylab="Scaled concentration")



par(mfrow=c(1,1))
matplot(t(PK_col_scale[1:2071,]),type="l",main = "PK scaled concentration",xlab = "Time index",ylab="Scaled concentration")
matplot(t(BG_col_scale[1:2071,]),type="l",main = "Blood glucose scaled concentration",xlab = "Time index",ylab="Scaled concentration")

matplot(t(PK_col_scale[1:20,]),type="l",main = "PK scaled concentration",xlab = "Time index",ylab="Scaled concentration")
matplot(t(BG_col_scale[1:20,]),type="l",main = "Blood glucose scaled concentration",xlab = "Time index",ylab="Scaled concentration")

write.csv()
### REMOVE ALL OTHER VARIABLES



# Functions

qqplot.data <- function (vec,main="") # argument: vector of numbers
{
  # following four lines from base R's qqline()
  y <- quantile(vec[!is.na(vec)], c(0.25, 0.75))
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]
  
  d <- data.frame(resids = vec)
  
  ggplot(d, aes(sample = resids)) + stat_qq() + geom_abline(slope = slope, intercept = int) + ggtitle(as.character(main))
  
}

grid.arrange(qqplot.data(PK1[,1],main = "3"),qqplot.data(PK1[,2],"6"),qqplot.data(PK1[,3],"9"),qqplot.data(PK1[,4],"12"),qqplot.data(PK1[,5],"15"),qqplot.data(PK1[,6],"20"),qqplot.data(PK1[,7],"30"),qqplot.data(PK1[,8],"45"),qqplot.data(PK1[,9],"60"),qqplot.data(PK1[,10],"75"),qqplot.data(PK1[,11],"90"),qqplot.data(PK1[,12],"105"),qqplot.data(PK1[,13],"120"),qqplot.data(PK1[,14],"180"),qqplot.data(PK1[,15],"240"),qqplot.data(PK1[,16],"300"))
grid.arrange(qqplot.data(BG1[,1],main = "3"),qqplot.data(BG1[,2],"6"),qqplot.data(BG1[,3],"9"),qqplot.data(BG1[,4],"12"),qqplot.data(BG1[,5],"15"),qqplot.data(BG1[,6],"20"),qqplot.data(BG1[,7],"30"),qqplot.data(BG1[,8],"45"),qqplot.data(BG1[,9],"60"),qqplot.data(BG1[,10],"75"),qqplot.data(BG1[,11],"90"),qqplot.data(BG1[,12],"105"),qqplot.data(BG1[,13],"120"),qqplot.data(BG1[,14],"180"),qqplot.data(BG1[,15],"240"),qqplot.data(BG1[,16],"300"))

gg_qq(PK1[,10])

gg_qq <- function(x, distribution = "norm", ..., line.estimate = NULL, conf = 0.95,
                  labels = names(x)){
  q.function <- eval(parse(text = paste0("q", distribution)))
  d.function <- eval(parse(text = paste0("d", distribution)))
  x <- na.omit(x)
  ord <- order(x)
  n <- length(x)
  P <- ppoints(length(x))
  df <- data.frame(ord.x = x[ord], z = q.function(P, ...))
  
  if(is.null(line.estimate)){
    Q.x <- quantile(df$ord.x, c(0.25, 0.75))
    Q.z <- q.function(c(0.25, 0.75), ...)
    b <- diff(Q.x)/diff(Q.z)
    coef <- c(Q.x[1] - b * Q.z[1], b)
  } else {
    coef <- coef(line.estimate(ord.x ~ z))
  }
  
  zz <- qnorm(1 - (1 - conf)/2)
  SE <- (coef[2]/d.function(df$z)) * sqrt(P * (1 - P)/n)
  fit.value <- coef[1] + coef[2] * df$z
  df$upper <- fit.value + zz * SE
  df$lower <- fit.value - zz * SE
  
  if(!is.null(labels)){ 
    df$label <- ifelse(df$ord.x > df$upper | df$ord.x < df$lower, labels[ord],"")
  }
  
  p <- ggplot(df, aes(x=z, y=ord.x)) +
    geom_point() + 
    geom_abline(intercept = coef[1], slope = coef[2]) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha=0.2) 
  if(!is.null(labels)) p <- p + geom_text( aes(label = label))
  print(p)
  coef
}
