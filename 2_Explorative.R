


dfg <- prof1 %>% mutate(NA_PK = ifelse(is.na(PK),1,0)) %>% filter(NA_PK == 1) %>% group_by(Time) %>% tally()
dfg2 <- prof1 %>% mutate(NA_BG = ifelse(is.na(BG),1,0)) %>% filter(NA_BG == 1) %>% group_by(Time) %>% tally()

sd3 <- ggplot(data = dfg,aes(x = Time,y = n)) + geom_bar(stat = "Identity") + theme_minimal() + ylab("Count") + xlab("Time[min]") + ggtitle("Distribution of NA values (PK)",sub = "For insulin concentration (PK) Total NA count: 3329")
sd4 <- ggplot(data = dfg2,aes(x = Time,y = n)) + geom_bar(stat = "Identity") + theme_minimal() + ylab("Count") + xlab("Time[min]") + ggtitle("Distribution of NA values (BG)",sub = "For Blood sugar concentration (BG) Total NA count: 29")
grid.arrange(sd3,sd4,ncol = 1)

### ========================================================== FIRST =========================================================

ggplot() + geom_line(aes(x = Time,y = PK,group=index,colour = "grey"),data = pro,alpha = 0.7)+ 
  theme(legend.position="none") + xlab("Time[min]") + ylab("Concentration[pm]") + ggtitle("Insulin Concentration Profiles") + 
  geom_line(aes(x = Time,y=PK_mean,colour = "red"),lwd = 1.4,data = dat_fil) 
twos <- c(1859,820)
rr2 <- ggplot() + 
  geom_line(aes(x = Time,y = BG,group=index),data = pro,colour = alpha("grey",0.7))+ 
  theme(legend.position="none") + xlab("Time[min]") + ylab("Concentration[mmol/L]") + theme(legend.position="none")+ ggtitle("Blood Sugar Concentration Profiles (BG)",sub = "Experiment 820 and 1859 to exemplify general trend") + 
  geom_line(aes(x = Time,y=BG,group=index,colour = "red"),lwd = 1,data = pro %>% filter(index %in% twos)) + theme_minimal()+ theme(legend.position="none") 

rr1 <- ggplot() + 
  geom_line(aes(x = Time,y = PK,group=index),data = pro,colour = alpha("grey",0.7))+ 
  theme(legend.position="none") + xlab("Time[min]") + ylab("Concentration[pm]")+ ggtitle("Insulin Concentration Profiles (PK) ",sub = "Experiment 820 and 1859 to exemplify general trend") +
  geom_line(aes(x = Time,y=PK,group=index,colour = "red"),lwd = 1,data = pro %>% filter(index %in% twos)) + theme_minimal()+ theme(legend.position="none") 

########## ================================  PIG WEIGHT AND DATE ===========================================================
View(param)
ggplot(data = param,aes(x = Date,y = BW,col = Animal)) + geom_line() + facet_wrap(~StudyYear,scales = "free") + ylab("Bodyweight[Kg]")

ggplot(data = param,aes(x = month(Date),y = BW,group=Study,col = Study)) + geom_smooth(method = "lm")

########### ============= DIM REDUCTION ============================================================================================00
PKPK <- Wide.all %>% select(ends_with("PK"),Treatment,Animal,Study,BW)
PKPK1 <- PKPK #%>% filter(Treatment != "A")
PKPK1[is.na(PKPK1)] <- 0
tsne <- Rtsne(PKPK1, dims = 2, perplexity=15, verbose=TRUE, max_iter = 1000,check_duplicates = FALSE)

tsne_data <- data.frame(data = tsne$Y,treatment = PKPK1$Treatment,animal = PKPK1$Animal,BW = PKPK1$BW)

try <- tsne_data %>% group_by(treatment) %>% summarise(X = mean(data.1),Y=mean(data.2))
try2 <- tsne_data %>% group_by(treatment) %>% summarise(X = median(data.1),Y=median(data.2))
try3 <- tsne_data %>% group_by(animal) %>% summarise(X = mean(data.1),Y=mean(data.2))

tsne_data$Treatment_1 <- ifelse(tsne_data$treatment == "A","A",ifelse(tsne_data$treatment == "B","B","X1-X172")) 

ggplot(data = tsne_data,aes(x = data.1,y = data.2,col = treatment)) + geom_point() + theme(legend.position = "none")+ geom_text(aes(label=treatment),hjust=0, vjust=0)
ggplot(data = try,aes(x = X,y = Y,col = treatment)) + geom_point() + theme(legend.position = "none")+ geom_text(aes(label=treatment),hjust=0, vjust=0)
ggplot(data = try2,aes(x = X,y = Y,col = treatment)) + geom_point() + theme(legend.position = "none")+ geom_text(aes(label=treatment),hjust=0, vjust=0)
ggplot(data = tsne_data,aes(x = data.1,y = data.2,col = as.factor(animal))) + geom_point() + theme(legend.position = "none")+ geom_text(aes(label=animal),hjust=0, vjust=0)
ggplot(data = tsne_data,aes(x = data.1,y = data.2,col = (BW))) + geom_point() #+ theme(legend.position = "none")#+ geom_text(aes(label=animal),hjust=0, vjust=0)





### Look at treatment 35 and 148
tre <- pro %>% filter(Treatment %in% c("X35","X148"))
ggplot(tre,aes(x = Time,y = PK,group = ID,col = Treatment)) + geom_line()
meantre <- tre %>% group_by(Time,Treatment) %>% summarise(mean1 = mean(PK,na.rm=T))
ggplot(meantre,aes(x = Time,y = mean1,col = Treatment)) + geom_line()

fj1 <- meantre %>% filter(Treatment == "X148") %>% select(mean1)
fj1 <- fj1[,2]

fj2 <- meantre %>% filter(Treatment == "X35") %>% select(mean1)
fj2 <- fj2[,2]
obj <- ccf(fj1,fj2,na.action = na.pass,lag.max = 0)
obj$acf[1]

PK.pca <- prcomp(PK, center = TRUE,scale. = TRUE)

prop <- pro %>% mutate(Treatment1 = ifelse(Treatment == "A","A",ifelse(Treatment == "B","B","X1-X172")))
prop1 <- prop %>% group_by(Treatment1) %>% tally() %>% mutate(pct = round(prop.table(n) * 100,2))
###### ============== BARPLOT OF COUNT OF TREATMENT A,B and X1-X172 =========================================================
ggplot(data = prop1,aes(x = Treatment1,y = n)) + geom_bar(stat = "identity") + theme_minimal() + geom_text(aes(y = n + 700,    # nudge above top of bar
                                                                                                               label = paste0(pct, '%')),    # prettify
                                                                                                           position = position_dodge(width = .9), 
                                                                                                           size = 4) + ggtitle("Amount of data on each treatment group",sub = "X1-X172 treatments visualized together") + ylab("Count") + xlab("Treatment")
# ======================================================================================================================================================================================================================================================================


aes(,y))+stat_density2d(geom="polygon",aes(alpha = ..level..),fill="orangered",color="red4",linetype=2)+ theme_bw()+scale_x_continuous("X-coordinate")+scale_y_continuous("Y-coordinate")
