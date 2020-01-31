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
data <- read.csv("~/Box/Desktop/Reading/Strawberry_Research/Heterosis/Salinas/Data/2018_Salinas_Fruit_Morph_KMA.csv")
data$K2 <- as.factor(data$K2) # 1 2 
data$K3 <- as.factor(data$K3) # 1 3 2
data$K4 <- as.factor(data$K4) # 1 3 2 4
data$K5 <- as.factor(data$K5) # 5 1 3 4 2
data$K6 <- as.factor(data$K6) # 1 4 2 5 6 3 
data$K7 <- as.factor(data$K7) # 4 1 6 5 3 7 2
data$K8 <- as.factor(data$K8) # 5 1 8 2 4 7 3 6
data$K9 <- as.factor(data$K9) # 1 4 7 2 6 8 3 9 5
data$K10 <- as.factor(data$K10) #3 1 2 4 5 10 7 8 6 9

#PPKC modified clusters
data$K2 <- factor(data$K2,levels= c(1, 2 ))
data$K3 <- factor(data$K3,levels= c(1, 3, 2))
data$K4 <- factor(data$K4,levels= c(1, 3, 2, 4))
data$K5 <- factor(data$K5,levels= c(5, 1, 3, 4, 2))
data$K6 <- factor(data$K6,levels= c(1, 4, 2,5, 6, 3 ))
data$K7 <- factor(data$K7,levels= c(4, 1, 6, 5, 3, 7, 2))
data$K8 <- factor(data$K8,levels= c(5, 1, 8, 2, 4, 7, 3, 6))
data$K9 <- factor(data$K9,levels= c(1, 4, 7, 2, 6, 8, 3, 9, 5))
data$K10 <- factor(data$K10,levels= c(3, 1, 2, 4, 5, 10, 7, 8, 6, 9))

pdf("K_dist.pdf",width=10,height=12)
par(mfrow=c(5,2))
hist(as.numeric(as.character(data$K2)),col="gray",xlab="Cluster ID", main="Clusters K=2",xlim=c(1,10),ylim=c(0,3600),breaks=2)
hist(as.numeric(as.character(data$K3)),col="gray",xlab="Cluster ID", main="Clusters K=3",xlim=c(1,10),ylim=c(0,3600),breaks=5)
hist(as.numeric(as.character(data$K4)),col="gray",xlab="Cluster ID", main="Clusters K=4",xlim=c(1,10),ylim=c(0,3600),breaks=5)
hist(as.numeric(as.character(data$K5)),col="gray",xlab="Cluster ID", main="Clusters K=5",xlim=c(1,10),ylim=c(0,3600),breaks=10)
hist(as.numeric(as.character(data$K6)),col="gray",xlab="Cluster ID", main="Clusters K=6",xlim=c(1,10),ylim=c(0,3600),breaks=10)
hist(as.numeric(as.character(data$K7)),col="gray",xlab="Cluster ID", main="Clusters K=7",xlim=c(1,10),ylim=c(0,3600),breaks=10)
hist(as.numeric(as.character(data$K8)),col="gray",xlab="Cluster ID", main="Clusters K=8",xlim=c(1,10),ylim=c(0,3600),breaks=10)
hist(as.numeric(as.character(data$K9)),col="gray",xlab="Cluster ID", main="Clusters K=9",xlim=c(1,10),ylim=c(0,3600),breaks=15)
hist(as.numeric(as.character(data$K10)),col="gray",xlab="Cluster ID", main="Clusters K=10",xlim=c(1,10),ylim=c(0,3600),breaks=15)
dev.off()

# Modified K Means Discription
data$EU <- interaction(as.factor(data$harvest),as.factor(data$Block),data$Progeny,drop=T)

h2 <- c()

clm2 <- clmm((K2) ~  factor(harvest)+ factor(Block) +(1|Progeny) + (1|EU),data=data)
h2[1] <- VarCorr.clmm(clm2)$Progeny[[1]] / (VarCorr.clmm(clm2)$Progeny[[1]] + (VarCorr.clmm(clm2)$EU[[1]])); plot(density(clm2$u))
clm3 <- clmm((K3) ~  factor(harvest)+ factor(Block) +(1|Progeny) + (1|EU),data=data)
h2[2] <- VarCorr.clmm(clm3)$Progeny[[1]] / (VarCorr.clmm(clm3)$Progeny[[1]] + (VarCorr.clmm(clm3)$EU[[1]]));  plot(density(clm3$u))
clm4 <- clmm((K4) ~  factor(harvest)+ factor(Block) +(1|Progeny) + (1|EU),data=data)
h2[3] <- VarCorr.clmm(clm4)$Progeny[[1]] / (VarCorr.clmm(clm4)$Progeny[[1]] + (VarCorr.clmm(clm4)$EU[[1]])); plot(density(clm4$u))
clm5 <- clmm((K5) ~  factor(harvest)+ factor(Block) +(1|Progeny) + (1|EU),data=data)
h2[4] <- VarCorr.clmm(clm5)$Progeny[[1]] / (VarCorr.clmm(clm5)$Progeny[[1]] + (VarCorr.clmm(clm5)$EU[[1]])); plot(density(clm5$u))
clm6 <- clmm((K6) ~  factor(harvest)+ factor(Block) +(1|Progeny) + (1|EU),data=data)
h2[5] <- VarCorr.clmm(clm6)$Progeny[[1]] / (VarCorr.clmm(clm6)$Progeny[[1]] + (VarCorr.clmm(clm6)$EU[[1]])); plot(density(clm6$u))
clm7 <- clmm((K7) ~  factor(harvest)+ factor(Block) +(1|Progeny) + (1|EU),data=data)
h2[6] <- VarCorr.clmm(clm7)$Progeny[[1]] / (VarCorr.clmm(clm7)$Progeny[[1]] + (VarCorr.clmm(clm7)$EU[[1]])); plot(density(clm7$u))
clm8 <- clmm((K8) ~  factor(harvest)+ factor(Block) +(1|Progeny) + (1|EU),data=data)
h2[7] <- VarCorr.clmm(clm8)$Progeny[[1]] / (VarCorr.clmm(clm8)$Progeny[[1]] + (VarCorr.clmm(clm8)$EU[[1]])); plot(density(clm8$u))
clm9 <- clmm((K9) ~  factor(harvest)+ factor(Block) +(1|Progeny) + (1|EU),data=data)
h2[8] <- VarCorr.clmm(clm9)$Progeny[[1]] / (VarCorr.clmm(clm9)$Progeny[[1]] + (VarCorr.clmm(clm9)$EU[[1]])); plot(density(clm9$u))
clm10 <- clmm((K10) ~  factor(harvest)+ factor(Block) +(1|Progeny) + (1|EU),data=data)
h2[9] <- VarCorr.clmm(clm10)$Progeny[[1]] / (VarCorr.clmm(clm10)$Progeny[[1]] + (VarCorr.clmm(clm10)$EU[[1]])); plot(density(clm10$u))

h2