# Load libraries
library(ggplot2)
library(tidyr)
library(dplyr)
library(readr)
library(gridExtra)
library(Rtsne)

'%!in%' <- function(x,y)!('%in%'(x,y))
# Read data
prof <- read_csv("C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/Foerste dataudtraek/0014_pig_profiles.csv")
param <-read_csv("C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/Foerste dataudtraek/0014_pig_parameters.csv")
prof$Animal <- as.factor(prof$Animal)
prof$seq <- seq(1,length(prof$Treatment))
# percentage of data on each treatment
prof14 <- prof %>% mutate(Treatment = ifelse(Treatment == "A","A",ifelse(Treatment == "B","B","X1-X172"))) 

prof14 %>% group_by(Treatment) %>% summarise(n = n(),percentage = n/37256)

prof11 <-prof %>% mutate(ff = ifelse(Time > 0 & Time < 150 & is.na(PK),1,0))
prof111 <- prof11[-which(prof11$ff==1),]
# Merge together
prof1 <- as.data.frame(prof %>% mutate(ID = paste0(Treatment,Study,Animal,BW)))
param1 <- as.data.frame(param %>% mutate(ID = paste0(Treatment,Study,Animal,BW)))
Total <- merge(prof1,param1,by= "ID")

Total2 <- Total %>% spread(Time,PK)
Total3 <- Total2 %>% select(-c(Study.x,Time_unit,PK_unit,BG_unit,BW_unit,Dose_unit,New,APIamount.x,
                               API_unit.x,Treatment.y,Animal.y,BW.y,PKanimalName,Dose.y,APIamount.y,API_unit.y))
View(Total2)
# Dim reduction
#tsne <- Rtsne(Total4, dims = 2, perplexity=30, verbose=TRUE, max_iter = 500)

## Explorative analysis: 
# Remove NA (should be discussed)

prof15 <- prof14 %>% mutate(`NA` = is.na(PK)) %>% group_by(Treatment,`NA`) %>% tally()


prof16 <- prof1 %>% mutate(`NA` = is.na(BG)) %>% group_by(Treatment,`NA`) %>% tally()

prof17 <- prof14 %>% filter(!is.na(PK))

g1 <- ggplot(data = prof15,aes(x = Treatment,y = n,fill = `NA`)) + geom_bar(stat = "identity") + geom_text(aes(y=n, label=n), vjust=1.6, 
                                                                                                    color="white", size=3.5)+
  scale_fill_brewer(palette="Paired")+
  theme_minimal() + ylab("Count") + ggtitle("Count of observations",subtitle = "For insulin concentration (PK)")

g2 <- ggplot(data = prof16,aes(x = Treatment,y = n,fill = `NA`)) + geom_bar(stat = "identity") + geom_text(aes(y=n, label=n), vjust=1.6, 
                                                                                                           color="white", size=3.5)+
  scale_fill_brewer(palette="Paired")+
  theme_minimal() + ylab("Count") + ggtitle("Count of observations",subtitle = "For Blood sugar concentration (PD)")
g2





## Maybe start here.. 
prof <- read_csv("C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/Foerste dataudtraek/0014_pig_profiles.csv")
prof$seq <- seq(1,length(prof$Treatment))
prof <- as.data.frame(prof)
prof <- prof %>% filter(!is.na(Time))
# Observations from time -1 all have NA in PK
prof <- prof %>% filter(Time != -1)
prof %>% group_by(Time) %>% tally()
prof <- prof %>% filter(Time != 150)
prof %>% group_by(Time) %>% tally()

hej <- rep(0,length(prof$Treatment))
for ( i in 2:length(prof$Treatment)){
  if(prof$Time[i] == prof$Time[i-1] & is.na(prof$PK[i] + prof$PK[i-1])){
    hej[i] <- i 
  }
}
hej <- as.data.frame(hej[which(hej != 0)])

range(hej)
View(hej)
View(prof %>% filter(seq %in% hej))
## Remove dublicates
seqtmp <- prof %>% filter(is.na(PK) & Time %in% c(3,6)) %>% select(seq)
prof <- prof %>% filter(seq %!in% seqtmp[c(1,23),1]) 
prof %>% group_by(Time) %>% tally()
## Now it is balanced on Time points. Divide into unique experiments:
prof$rep <- as.factor(rep(1:2071,each=16))  
# Spread for canonical correlation analysis
test <- prof %>% select(Time,PK,BG)

wide <- test %>% gather(variable, value, -(Time)) %>%
  unite(temp, Time,variable) %>%
group_by(temp) %>%
  mutate(id=1:n()) %>% 
  spread(temp, value)

## Plot data on each Animal
ggplot(data = prof %>% filter(Animal == 1),aes(x = Time,y = PK,col = as.factor(rep))) + geom_line()+ theme(legend.position="none")
ggplot(data = prof %>% filter(Animal == 1,rep %in% c(1:10)),aes(x = Time,y = PK,col = as.factor(rep))) + geom_line()
ggplot(data = prof,aes(x = Time,y = PK,col = as.factor(rep))) + geom_line()+ theme(legend.position="none") + xlab("Time[min]") + ylab("Concentration[pm]") + ggtitle("PK curve on pigs 1-8") + facet_wrap(~as.factor(Animal))

meantmp <- prof %>% group_by(Animal,Time) %>% summarise(mean = mean(PK,na.rm =T))
meantmpglu <- prof %>% group_by(Animal,Time) %>% summarise(mean = mean(BG,na.rm =T))

names(prof)
f1 <- ggplot(data = meantmp,aes(x = Time, y = mean,col = Animal)) + geom_line() + xlab("Time[min]") + ylab("Mean concentration[pm]") + ggtitle("Mean concentatio")
f2 <- ggplot(data = meantmpglu,aes(x = Time, y = mean,col = Animal)) + geom_line() + xlab("Time[min]") + ylab("Mean concentration[pm]") + ggtitle("Mean concentatio")

grid.arrange(f1,f2,ncol = 2)

## plot B treatment on each animal
ggplot(data = prof %>% filter(Treatment == "B"),aes(x = Time,y = PK,group = rep,col = Animal)) + geom_line()  + xlab("Time[min]") + ylab("Concentration[pm]") + ggtitle("PK curve treatment B")

meantmp2 <- prof %>% group_by(Animal,Time,Treatment) %>% summarise(mean = mean(PK,na.rm =T))
ggplot(data = meantmp2 %>% filter(Animal == 1),aes(x = Time, y = mean,col = Treatment)) + geom_line()+ theme(legend.position="none")+  xlab("Time[min]") + ylab("Concentration[pm]") + ggtitle("") + facet_wrap(~Animal)

### treatment as features
tmp <- prof %>% group_by(rep) %>% summarise( diffPK = max(PK,na.rm = T)-min(PK,na.rm=T),diffGlu = max(BG,na.rm=T)-min(BG,na.rm=T))
tmp1 <- merge(prof,tmp,by.x = "rep",by.y = "rep") %>% group_by(rep) %>% slice(1) %>% select(rep,Treatment,BW,Dose,diffPK,diffGlu)


wide_prof1 <- prof %>% spread(Time,PK)
wide_prof <- wide_prof1 %>% spread(Treatment,PK)
View(wide_prof1)



## OR maybe here..
prof <- read_csv("C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/Foerste dataudtraek/0014_pig_profiles.csv")
prof$seq <- seq(1,length(prof$Treatment))
prof$ID <- paste0(prof$Treatment,prof$Animal,prof$Study)
prof <- as.data.frame(prof)
prof <- prof %>% filter(!is.na(Time))
# Observations from time -1 all have NA in PK
prof <- prof %>% filter(Time != -1)
prof %>% group_by(Time) %>% tally()

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
View(re1)
########## FOR REPLICATES 2
twos <- ff %>% filter(n == 2)
twos$ID <- paste0(twos$Treatment,twos$Animal,twos$Study)

re2 <- profe %>% filter(ID %in% unique(twos$ID))
View(re2)
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
View(re3)


hej <- rep(0,nrow(re3))
for ( i in 2:nrow(re3)){
  if(re3$Time[i] == re3$Time[i-1]){
    hej[i] <- i 
  }
}
hej <- as.data.frame(hej[which(hej != 0)])
View(more)

re4 <- re3[1:1024,]
View(re4)
re4$index <-rep((1:64)+1974,each = 16)


View(re4)


sum(unique(re1$ID) %in% unique(re2$ID))
sum(unique(re2$ID) %in% unique(re4$ID))

View(ak)

re5 <- re3[1025:nrow(re3),] # Hvad er tilbage

re6 <- re5[1:128,] # Første 128 rækker af det
re61 <- re6[c(TRUE, FALSE),]
re61$index <- rep((1:4)+2038,each=16)
re62 <- re6[c(FALSE, TRUE),]
re62$index <- rep((1:4)+2042,each=16)
View(re3)
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

ak <- bind_rows(re1,re2,re4,re61,re62,re7,re81,re82,re83,re84,re9,re101,re102,re103,re11)
ak %>% group_by(Time) %>% tally()

## HERE:
prof <- read_csv("C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/Foerste dataudtraek/0014_pig_profiles.csv")
prof$seq <- seq(1,length(prof$Treatment))
prof$index <-0
for( i in 1:length(prof$Treatment)){
  if(prof$Time[i] == 3){
    while(prof$Time[i] != 3){
      prof$index[i] <- 1  
  } 
 }
}

View(prof)
