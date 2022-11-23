#install.packages(dplyr)
#install.packages(tidyr)
library(tidyr)
library(dplyr)

#Uploading and Merging Data----
library(readxl)
CC <- read_excel("Quant Neuro_Fall 2022/FTD CC data.xlsx")
View(CC)

Cing <- read_excel("Quant Neuro_Fall 2022/FTD Cingulum data.xlsx")
View(Cing)


Serial_Markers <- read_excel("Quant Neuro_Fall 2022/FTD Irwin Serial Markers.xlsx")
View(Serial_Markers)

Serial_Marks2 <-  rename(Serial_Markers, AutopsyID = AutopsyID...2)
View(Serial_Marks2)

CC_Data <- left_join(CC, Serial_Marks2, by="AutopsyID")
View(CC_Data)

Cing_Data <- left_join(Cing, Serial_Marks2, by="AutopsyID")
View(Cing_Data)

CC_Data <- rename(CC_Data, CC_2SD = "Full FTD CC -2SD MBP threshold: Positive %")
Cing_Data <- rename(Cing_Data, Cing_2SD = "MBP Cingulum -2SD: Positive %")

CC_Data <- mutate(CC_Data, GM_aCing = rowMeans(CC_Data[,c("L_GM_aCING","R_GM_aCING")], na.rm = TRUE)) 
CC_Data <- mutate(CC_Data, WM_aCing = rowMeans(CC_Data[,c("L_WM_aCING","R_WM_aCING")], na.rm = TRUE))

Cing_Data <- mutate(Cing_Data, WM_aCing = rowMeans(Cing_Data[,c("L_WM_aCING","R_WM_aCING")], na.rm = TRUE))
Cing_Data <- mutate(Cing_Data, GM_aCing = rowMeans(Cing_Data[,c("L_GM_aCING","R_GM_aCING")], na.rm = TRUE))

HC_CC <- filter(CC_Data, Group == "HC")%>% select(CC_2SD) %>% unlist %>% as.numeric()
TDP_CC <- filter(CC_Data, Group == "TDP")%>% select(CC_2SD)%>% unlist %>% as.numeric()
Tau_CC <- filter(CC_Data, Group == "Tau")%>% select(CC_2SD)%>% unlist %>% as.numeric()

HC_Cing <- filter(Cing_Data, Group == "HC")%>% select(Cing_2SD)%>% unlist %>% as.numeric() 
TDP_Cing <- filter(Cing_Data, Group == "TDP")%>% select(Cing_2SD)%>% unlist %>% as.numeric()
Tau_Cing <- filter(Cing_Data, Group == "Tau")%>% select(Cing_2SD)%>% unlist %>% as.numeric()
#Sample Size----

CC_Data %>% group_by(Group) %>% filter(Group =="HC")%>% count()
CC_Data %>% group_by(Group) %>% filter(Group =="Tau")%>% count()
CC_Data %>% group_by(Group) %>% filter(Group =="TDP")%>% count()

Cing_Data %>% group_by(Group) %>% filter(Group=="HC")%>% count()
Cing_Data %>% group_by(Group) %>% filter(Group=="Tau")%>% count()
Cing_Data %>% group_by(Group) %>% filter(Group=="TDP")%>% count()

#Distribution of Data----
qqnorm(HC_CC, ylab = "Control Corpus Callosum -2SD MBP Threshold: Sample Quantiles")
qqline(HC_CC)

qqnorm(HC_Cing, ylab = "Control Cingulum -2SD MBP Threshold: Sample Quantiles")
qqline(HC_Cing)

qqnorm(TDP_CC, ylab = "TDP Corpus Callosum -2SD MBP Threshold: Sample Quantiles")
qqline(TDP_CC)

qqnorm(TDP_Cing, ylab = "TDP Cingulum -2SD MBP Threshold: Sample Quantiles")
qqline(TDP_Cing)

qqnorm(Tau_Cing, ylab = "Tau Cingulum -2SD MBP Threshold: Sample Quantiles")
qqline(Tau_Cing)

qqnorm(Tau_CC, ylab = "Tau Corpus Callosum -2SD MBP Threshold: Sample Quantiles")
qqline(Tau_CC)

qqnorm(Cing_Data$Cing_2SD, ylab = "Cingulum MBP Threshold:Sample Quantiles")
qqline(Cing_Data$Cing_2SD)

CC_Data <- transform(CC_Data, CC_2SD = as.numeric(as.character(CC_2SD)))

qqnorm(CC_Data$CC_2SD, ylab = "Corpus Callosum MBP Threshold:Sample Quantiles")
qqline(CC_Data$CC_2SD)

#install.packages("ggplot2")
library(ggplot2)

histCC <- ggplot(CC_Data, aes(x=CC_2SD, fill="Group")) + geom_histogram()

print(histCC + labs(x = "Percent ROI Under the Threshold")+ ggtitle("Corpus Callosum"))

ks.test(CC_Data$CC_2SD, "pnorm")

histCing <- ggplot(Cing_Data, aes(x=Cing_2SD)) + geom_histogram()

print(histCing + labs(x = "Percent ROI Under the Threshold")+ ggtitle("Cingulum"))


#Boxplots----

install.packages("patchwork", repos = "https://cloud.r-project.org")

 ggplot(data = CC_Data) +
  geom_boxplot(mapping = aes(x = Group, y = CC_2SD))+ 
  labs(x= "Pathology", y= "Percent ROI Under MBP Threshold", title = "Corpus Callosum")

 ggplot(data = Cing_Data) +
   geom_boxplot(mapping = aes(x = Group, y = Cing_2SD))+ 
   labs(x= "Pathology", y= "Percent ROI Under MBP Threshold", title = "Cingulum")
 

ggplot(data = Cing_Data)+
  geom_boxplot(mapping = aes(x = NPDx1, y= Cing_2SD, fill=Tau2TDP1))+
  labs(x = "Disease", y= "Percent ROI Under MBP Threshold", title = "Cingulum" )+ 
  theme(axis.text.x = element_text(angle = 45))

ggplot(data = CC_Data)+
  geom_boxplot(mapping = aes(x = NPDx1, y= CC_2SD, fill=Tau2TDP1))+
  labs(x = "Disease", y= "Percent ROI Under MBP Threshold", title = "Corpus Callosum" )

CC_Data %>% ggplot(aes(x=Group, y=CC_2SD, fill=NPDx1)) +
  geom_boxplot() +
  scale_fill_discrete(labels=c('ALS', 'ALS-dementia','Corticobasal degeneration','FTD-Parkinson-linked Chr17','FTLD-TDP','Picks Disease', 'PSP','Tauopathy unclassifiable','NA'))+
  labs(x = "Disease", y= "Percent ROI Under MBP Threshold", title = "Corpus Callosum" )

Cing_Data %>% ggplot(aes(x=Group, y=Cing_2SD, fill=NPDx1)) +
  geom_boxplot() +
  scale_fill_discrete(labels=c('ALS', 'ALS-dementia','Corticobasal degeneration','FTD-Parkinson-linked Chr17','FTLD-TDP','Picks Disease', 'PSP','Tauopathy unclassifiable','NA'))+
  labs(x = "Disease", y= "Percent ROI Under MBP Threshold", title = "Cingulum" )


ggplot(CC_Data, aes(x=CC_2SD, y=WM_aCing)) + geom_point()+
  labs(x="Percent ROI Below Threshold", y="WM aCing Pathology Rating")

ggplot(Cing_Data, aes(x=Cing_2SD, y=WM_aCing)) + geom_point()+
  labs(x="Percent ROI Below Threshold", y="WM aCing Pathology Rating")

#Statistical Tests----
#kruskal Wallis 

kruskal.test(CC_2SD ~ Group, data = CC_Data)

kruskal.test(Cing_2SD ~ Group, data = Cing_Data)

CC_anova <- aov(CC_2SD ~ Group, data = CC_Data)
summary(CC_anova)

Cing_anova <- aov(Cing_2SD ~ Group, data = Cing_Data)
summary(Cing_anova)

kruskal.test(CC_2SD ~ NPDx1, data = CC_Data)

kruskal.test(Cing_2SD ~ NPDx1, data = Cing_Data)

#Filter Data base dino Ordinal Rating----
#upload ordinal ratings
ordinal <- read_excel("Quant Neuro_Fall 2022/MBP Ordinal Rating.xlsx")
View(ordinal)

ordinal <- rename(ordinal, CC_ord = "CC")
ordinal <- rename(ordinal, Cing_ord = "Cing")

CC_Data2 <- left_join(CC_Data, ordinal, by="AutopsyID","Block")
View(CC_Data2)

Cing_Data2 <- left_join(Cing_Data, ordinal, by="AutopsyID", "Block")
View(Cing_Data2)

CingData3 <- filter(Cing_Data2, Cing_ord !="3")
View(CingData3)

CCData3 <- filter(CC_Data2, CC_ord !="3")
View(CCData3)


ggplot(data = CCData3) +
  geom_boxplot(mapping = aes(x = Group, y = CC_2SD))+ 
  labs(x= "Pathology", y= "Percent ROI Under MBP Threshold", title = "Corpus Callosum")

ggplot(data = CingData3) +
  geom_boxplot(mapping = aes(x = Group, y = Cing_2SD))+ 
  labs(x= "Pathology", y= "Percent ROI Under MBP Threshold", title = "Cingulum")

#new filtered distribution
filter_histCC <- ggplot(CCData3, aes(x=CC_2SD, fill="Group")) + geom_histogram()

print(filter_histCC + labs(x = "Percent ROI Under the Threshold")+ ggtitle("Corpus Callosum"))

qqnorm(CCData3$CC_2SD, ylab = "CC MBP Threshold:Sample Quantiles")
qqline(CCData3$CC_2SD)

filter_histCing <- ggplot(CingData3, aes(x=Cing_2SD, fill="Group")) + geom_histogram()

print(filter_histCing + labs(x = "Percent ROI Under the Threshold")+ ggtitle("Cingulum"))

qqnorm(CingData3$Cing_2SD, ylab = "Cingulum MBP Threshold:Sample Quantiles")
qqline(CingData3$Cing_2SD)

CCData3 %>% ggplot(aes(x=Group, y=CC_2SD, fill=NPDx1)) +
  geom_boxplot() +
  scale_fill_discrete(labels=c('ALS', 'ALS-dementia','Corticobasal degeneration','FTD-Parkinson-linked Chr17','FTLD-TDP','Picks Disease', 'PSP','Tauopathy unclassifiable','NA'))+
  labs(x = "Disease", y= "Percent ROI Under MBP Threshold", title = "Corpus Callosum" )

CingData3 %>% ggplot(aes(x=Group, y=Cing_2SD, fill=NPDx1)) +
  geom_boxplot() +
  scale_fill_discrete(labels=c('ALS', 'ALS-dementia','Corticobasal degeneration','FTD-Parkinson-linked Chr17','FTLD-TDP','Picks Disease', 'PSP','Tauopathy unclassifiable','NA'))+
  labs(x = "Disease", y= "Percent ROI Under MBP Threshold", title = "Cingulum" )

kruskal.test(Cing_2SD ~ Group, data = CingData3)

kruskal.test(CC_2SD ~ Group, data = CCData3)

kruskal.test(Cing_2SD ~ NPDx1, data = CingData3)

kruskal.test(CC_2SD ~ NPDx1, data = CCData3)

CCData3 <- rename(CCData3, Picks = "Pick's Disease")


#Mann Whitney for CC NPDx1
Picks <- CCData3 %>% group_by(NPDx1)%>% filter(NPDx1 == "Pick's disease")
HC_CC <- CCData3 %>% group_by(Group)%>% filter(Group == "HC")
FTLD <- CCData3 %>% group_by(NPDx1)%>% filter(NPDx1 == "Frontotemporal lobar degeneration with TDP inclusions (Also known as FTLD-TDP)")

wilcox.test(Picks$CC_2SD, HC_CC$CC_2SD)

wilcox.test(Picks$CC_2SD, FTLD$CC_2SD)

wilcox.test(HC_CC$CC_2SD, FTLD$CC_2SD)
