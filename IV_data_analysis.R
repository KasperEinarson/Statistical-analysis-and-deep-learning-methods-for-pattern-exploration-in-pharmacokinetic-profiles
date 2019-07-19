library("readxl")
IV <- read_excel("C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/iv data aspart.xls")
IV <- IV[-1,]
colnames(IV)[c(7,4,2)] <- c("PK","Time","Animal")

IV[which(IV$PK=="<"),] <- 0
IV[which(is.na(IV$PK)),] <- 0

View(IV)
names(IV)
IV_im <- IV[,c(2,4,7,16)]
View(IV_im)
Times2 <- c(0,3,6,9,12,15,20,30,45,60,75,90,105,120,150,180,240,300)
dim(IV)
rep_Time <- rep(Times2,12)
IV_im$Time1 <- rep_Time[-1]
View(IV_im1)
IV_im1 <- IV_im %>% filter(Time1 != 0 & Time1 != 150)
IV_im2 <- IV_im %>% filter(Time1 < 61 & Time != 0)
View(IV_im1)
IV_im1$rep <- rep(1:12,each = 16)
IV_im2$rep <- rep(1:12,each = 9)

View(IV_im1)

ggplot(data = IV_im2,aes(x = as.factor(Time1),y = PK,col=rep,group=rep)) + geom_line()
View(IV_im2)
plot(IV_im2$Time1,IV_im2$PK,col=as.factor(IV_im2$rep),type="l")
plot(IV_im2$Time1,IV_im2$PG,col=as.factor(IV_im2$rep),type="l")

Wide_IV <- IV_im1 %>% select(Time,PK,PG) %>%
  gather(variable, value, -(Time)) %>%
  unite(temp, Time,variable) %>%
  group_by(temp) %>%
  mutate(id=1:n()) %>% 
  spread(temp, value)

Wide_IV[is.na(Wide_IV)] <- 0
Wide_IV <- Wide_IV[,-c(2,3)]
IV_PK <- Wide_IV[,c(13,19,25,5,9,11,15,17,21,23,27,3,7)]
IV_BG <- Wide_IV[,c(13,19,25,5,9,11,15,17,21,23,27,3,7)-1]

