###
# Variance Component Estimation for CLMM
# Mitchell Feldmann
###

##########################################################################################
############################# category -- Cumulative Logit Mixed Models ##################
##########################################################################################
#write.csv(data,"2018_Salinas_Fruit_Morph_KMA.csv")
rm(list=ls())
library(ordinal)

data <- read.csv("~/Box/Desktop/Reading/Strawberry_Research/1_UnsupervisedQuantization/Zenodo/Quantitative_Strawberry_Features.csv")

data$modK2 <- as.factor(data$modK2) # 1 2 
data$modK3 <- as.factor(data$modK3) # 1 3 2
data$modK4 <- as.factor(data$modK4) # 1 2 3 4
data$modK5 <- as.factor(data$modK5) # 4 5 1 3 2
data$modK6 <- as.factor(data$modK6) # 5 1 6 3 4 2
data$modK7 <- as.factor(data$modK7) # 7 3 6 1 2 4 5
data$modK8 <- as.factor(data$modK8) # 6 2 1 5 3 8 4 7
data$modK9 <- as.factor(data$modK9) # 5 3 2 1 8 7 9 4 6
data$modK10 <- as.factor(data$modK10) # 9 2 1 3 8 10 7 5 6 4

#PPKC modified clusters
data$modK2 <- factor(data$modK2,levels=c("1","2"))
data$modK3 <- factor(data$modK3,levels=c("1","3","2"))
data$modK4 <- factor(data$modK4,levels=c("1","2","3","4"))
data$modK5 <- factor(data$modK5,levels=c("2","3","1","5","4"))
data$modK6 <- factor(data$modK6,levels=c("5","1","6","3","4","2"))
data$modK7 <- factor(data$modK7,levels=c("7","3","6","1","2","4","5"))
data$modK8 <- factor(data$modK8,levels=c("6","2","1","5","3","8","4","7"))
data$modK9 <- factor(data$modK9,levels=c("5","3","2","1","8","7","9","4","6"))
data$modK10 <- factor(data$modK10,levels=c("9","2","1","3","8","10","7","5","6","4"))


# Modified K Means Discription
data$EU <- interaction(as.factor(data$harvest),as.factor(data$Block),data$Progeny,drop=T)

h2 <- c()

clm2 <- clmm((modK2) ~  factor(harvest)+ factor(Block) +(1|Progeny) + (1|EU),data=data)
h2[1] <- VarCorr.clmm(clm2)$Progeny[[1]] / (VarCorr.clmm(clm2)$Progeny[[1]] + (VarCorr.clmm(clm2)$EU[[1]])); plot(density(clm2$u))
clm3 <- clmm((modK3) ~  factor(harvest)+ factor(Block) +(1|Progeny) + (1|EU),data=data)
h2[2] <- VarCorr.clmm(clm3)$Progeny[[1]] / (VarCorr.clmm(clm3)$Progeny[[1]] + (VarCorr.clmm(clm3)$EU[[1]]));  plot(density(clm3$u))
clm4 <- clmm((modK4) ~  factor(harvest)+ factor(Block) +(1|Progeny) + (1|EU),data=data)
h2[3] <- VarCorr.clmm(clm4)$Progeny[[1]] / (VarCorr.clmm(clm4)$Progeny[[1]] + (VarCorr.clmm(clm4)$EU[[1]])); plot(density(clm4$u))
clm5 <- clmm((modK5) ~  factor(harvest)+ factor(Block) +(1|Progeny) + (1|EU),data=data)
h2[4] <- VarCorr.clmm(clm5)$Progeny[[1]] / (VarCorr.clmm(clm5)$Progeny[[1]] + (VarCorr.clmm(clm5)$EU[[1]])); plot(density(clm5$u))
clm6 <- clmm((modK6) ~  factor(harvest)+ factor(Block) +(1|Progeny) + (1|EU),data=data)
h2[5] <- VarCorr.clmm(clm6)$Progeny[[1]] / (VarCorr.clmm(clm6)$Progeny[[1]] + (VarCorr.clmm(clm6)$EU[[1]])); plot(density(clm6$u))
clm7 <- clmm((modK7) ~  factor(harvest)+ factor(Block) +(1|Progeny) + (1|EU),data=data)
h2[6] <- VarCorr.clmm(clm7)$Progeny[[1]] / (VarCorr.clmm(clm7)$Progeny[[1]] + (VarCorr.clmm(clm7)$EU[[1]])); plot(density(clm7$u))
clm8 <- clmm((modK8) ~  factor(harvest)+ factor(Block) +(1|Progeny) + (1|EU),data=data)
h2[7] <- VarCorr.clmm(clm8)$Progeny[[1]] / (VarCorr.clmm(clm8)$Progeny[[1]] + (VarCorr.clmm(clm8)$EU[[1]])); plot(density(clm8$u))
clm9 <- clmm((modK9) ~  factor(harvest)+ factor(Block) +(1|Progeny) + (1|EU),data=data)
h2[8] <- VarCorr.clmm(clm9)$Progeny[[1]] / (VarCorr.clmm(clm9)$Progeny[[1]] + (VarCorr.clmm(clm9)$EU[[1]])); plot(density(clm9$u))
clm10 <- clmm((modK10) ~  factor(harvest)+ factor(Block) +(1|Progeny) + (1|EU),data=data)
h2[9] <- VarCorr.clmm(clm10)$Progeny[[1]] / (VarCorr.clmm(clm10)$Progeny[[1]] + (VarCorr.clmm(clm10)$EU[[1]])); plot(density(clm10$u))