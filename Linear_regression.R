ID_test <- Wide_for_cor.u[,2][ID3,]
test_data <- Wide_for_cor.u[ID3,]


pro_test <- pro %>% filter(as.character(ID) %in% ID_test$ID)

names(pro_test)


## Create data:

Data_full <- cbind(D_xscores[,c(1,2)],Treat$Treatment[ID3],Wide_test$BW[ID3],grupper3_PK[ID3],Wide_test$ID[ID3])
colnames(Data_full) <- c("V1","V2","Treatment","BW","Groups","ID")
names(Data_full)
lm1 <- lm(cbind(V1,V2) ~Treatment*(BW+Groups),data = Data_full)
lm2 <- lm(V1 ~Treatment*(BW+Groups),data = Data_full)

anova(lm1)
anova(lm2)
plot(lm2)

