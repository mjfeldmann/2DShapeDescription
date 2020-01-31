###
# Variable Selection Using Random Forests and Classification using SVM and LDA
# Mitchell Feldmann
###

##########################################################################
# Random Forest Classifier for Variable Selection
##########################################################################
r
rm(list=ls()); 
library(VSURF); library(parallel); library(e1071); library(MASS); library(cvTools)

data <- read.csv("~/Box/Desktop/Reading/Strawberry_Research/Heterosis/Salinas/Data/2018_Salinas_Fruit_Morph.csv")
colnames(data)
data <- data[,c(27,30,32,34,35,36,41:43,50,51,  # ImageJ Phenotypes
                72:91,                       # Fourier PCs
                92:98,                       # SEM latent traits
                399:418,                     # EigenFruit
                419:428,                     # Biomass
                429:437)]                    # Categories
str(data)
data$K2 = as.factor(data$K2)
data$K3 = as.factor(data$K3)
data$K4 = as.factor(data$K4)
data$K5 = as.factor(data$K5)
data$K6 = as.factor(data$K6)
data$K7 = as.factor(data$K7)
data$K8 = as.factor(data$K8)
data$K9 = as.factor(data$K9)
data$K10 = as.factor(data$K10)

n = detectCores()
k2_rf <- VSURF(x = data[,c(1:68)],y = data[,69],
               ncores = n,parallel=T,
               clusterType = "PSOCK")
save(list="k2_rf",file="k2_rf.RData")

k3_rf <- VSURF(x = data[,c(1:68)],y = data[,70],
               ncores = n,parallel=T,
               clusterType = "PSOCK")
save(list="k3_rf",file="k3_rf.RData")

k4_rf <- VSURF(x = data[,c(1:68)],y = data[,71],
               ncores = n,parallel=T,
               clusterType = "PSOCK")
save(list="k4_rf",file="k4_rf.RData")

k5_rf <- VSURF(x = data[,c(1:68)],y = data[,72],
               ncores = n,parallel=T,
               clusterType = "PSOCK")
save(list="k5_rf",file="k5_rf.RData")

k6_rf <- VSURF(x = data[,c(1:68)],y = data[,73],
               ncores = n,parallel=T,
               clusterType = "PSOCK")
save(list="k6_rf",file="k6_rf.RData")

k7_rf <- VSURF(x = data[,c(1:68)],y = data[,74],
               ncores = n,parallel=T,
               clusterType = "PSOCK")
save(list="k7_rf",file="k7_rf.RData")

k8_rf <- VSURF(x = data[,c(1:68)],y = data[,75],
               ncores = n,parallel=T,
               clusterType = "PSOCK")
save(list="k8_rf",file="k8_rf.RData")

k9_rf <- VSURF(x = data[,c(1:68)],y = data[,76],
               ncores = n,parallel=T,
               clusterType = "PSOCK")
save(list="k9_rf",file="k9_rf.RData")

k10_rf <- VSURF(x = data[,c(1:68)],y = data[,77],
                ncores = n,parallel=T,
                clusterType = "PSOCK")
save(list="k10_rf",file="k10_rf.RData")

