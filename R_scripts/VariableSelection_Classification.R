###
# Variable Selection Using Random Forests and Classification using SVM and LDA
# Mitchell Feldmann
###

##########################################################################
# Random Forest Classifier for Variable Selection
##########################################################################
rm(list=ls()); library(VSURF); library(parallel)

data <- read.csv("~/Box/Desktop/Reading/Strawberry_Research/1_UnsupervisedQuantization/Zenodo/Quantitative_Strawberry_Features.csv")
data <- data[,c(27,30,32,34,35,36,41:43,50,51,        # ImageJ Phenotypes
                72:91,                       # Fourier PCs
                92:97,                       # SEM latent traits
                398:417,                     # EigenFruit
                418:427,                     # Biomass
                437:445)]                    # Categories
colnames(dat)
data$modK2 = as.factor(data$modK2)
data$modK3 = as.factor(data$modK3)
data$modK4 = as.factor(data$modK4)
data$modK5 = as.factor(data$modK5)
data$modK6 = as.factor(data$modK6)
data$modK7 = as.factor(data$modK7)
data$modK8 = as.factor(data$modK8)
data$modK9 = as.factor(data$modK9)
data$modK10 = as.factor(data$modK10)

n = detectCores()

k2_rf <- VSURF(x = data[,c(1:67)],y = data[,68],
               ncores = n, parallel=T, 
               clusterType = "PSOCK")
save(list="k2_rf",file="C:/Users/18058/Box/k2_rf.RData")

k3_rf <- VSURF(x = data[,c(1:67)],y = data[,69],
               ncores = n,parallel=T, 
               clusterType = "PSOCK")
save(list="k3_rf",file="C:/Users/18058/Box/k3_rf.RData")

k4_rf <- VSURF(x = data[,c(1:67)],y = data[,70],
               ncores = n,parallel=T,
               clusterType = "PSOCK")
save(list="k4_rf",file="C:/Users/18058/Box/k4_rf.RData")

k5_rf <- VSURF(x = data[,c(1:67)],y = data[,71],
               ncores = n,parallel=T,
               clusterType = "PSOCK")
save(list="k5_rf",file="C:/Users/18058/Box/k5_rf.RData")

k6_rf <- VSURF(x = data[,c(1:67)],y = data[,72],
               ncores = n,parallel=T,
               clusterType = "PSOCK")
save(list="k6_rf",file="C:/Users/18058/Box/k6_rf.RData")

k7_rf <- VSURF(x = data[,c(1:67)],y = data[,73],
               ncores = n,parallel=T, 
               clusterType = "PSOCK")
save(list="k7_rf",file="C:/Users/18058/Box/k7_rf.RData")

k8_rf <- VSURF(x = data[,c(1:67)],y = data[,74],
               ncores = n,parallel=T,
               clusterType = "PSOCK")
save(list="k8_rf",file="C:/Users/18058/Box/k8_rf.RData")

k9_rf <- VSURF(x = data[,c(1:67)],y = data[,75],
               ncores = n,parallel=T, 
               clusterType = "PSOCK")
save(list="k9_rf",file="C:/Users/18058/Box/k9_rf.RData")

k10_rf <- VSURF(x = data[,c(1:67)],y = data[,76],
                ncores = n,parallel=T, 
                clusterType = "PSOCK")
save(list="k10_rf",file="C:/Users/18058/Box/k10_rf.RData")

##########################################################################################
############################## classification -- SVM, LDA ################################
##########################################################################################
rm(list=ls()); 
library(VSURF); library(parallel); library(e1071); library(MASS); library(cvTools)
data <- read.csv("~/Box/Desktop/Reading/Strawberry_Research/1_UnsupervisedQuantization/Zenodo/Quantitative_Strawberry_Features.csv")
data <- data[,c(27,30,32,34,35,36,41:43,50,51,        # ImageJ Phenotypes
                72:91,                       # Fourier PCs
                92:97,                       # SEM latent traits
                398:417,                     # EigenFruit
                418:427,                     # Biomass
                437:445)]    
colnames(data)

load("~/Box/Desktop/Reading/Strawberry_Research/1_UnsupervisedQuantization/VSURF/k2_rf.RData")
load("~/Box/Desktop/Reading/Strawberry_Research/1_UnsupervisedQuantization/VSURF/k3_rf.RData")
load("~/Box/Desktop/Reading/Strawberry_Research/1_UnsupervisedQuantization/VSURF/k4_rf.RData")
load("~/Box/Desktop/Reading/Strawberry_Research/1_UnsupervisedQuantization/VSURF/k5_rf.RData")
load("~/Box/Desktop/Reading/Strawberry_Research/1_UnsupervisedQuantization/VSURF/k6_rf.RData")
load("~/Box/Desktop/Reading/Strawberry_Research/1_UnsupervisedQuantization/VSURF/k7_rf.RData")
load("~/Box/Desktop/Reading/Strawberry_Research/1_UnsupervisedQuantization/VSURF/k8_rf.RData")
load("~/Box/Desktop/Reading/Strawberry_Research/1_UnsupervisedQuantization/VSURF/k9_rf.RData")
load("~/Box/Desktop/Reading/Strawberry_Research/1_UnsupervisedQuantization/VSURF/k10_rf.RData")

name <- c( names(data[,k2_rf$varselect.pred]),
           names(data[,k3_rf$varselect.pred]),
           names(data[,k4_rf$varselect.pred]),
           names(data[,k5_rf$varselect.pred]),
           names(data[,k6_rf$varselect.pred]),
           names(data[,k7_rf$varselect.pred]),
           names(data[,k8_rf$varselect.pred]),
           names(data[,k9_rf$varselect.pred]),
           names(data[,k10_rf$varselect.pred]))
(table(name))

name <- names(which(table(name) >= 3))

dat <- data[,which(colnames(data) %in% name)]

colnames(dat)

#10 fold CV
kfold = 10
lda.acc <- data.frame(iter=c(1:10),k2=NA,k3=NA,k4=NA,k5=NA,k6=NA,k7=NA,k8=NA,k9=NA,k10=NA)
svm.acc <- data.frame(iter=c(1:10),k2=NA,k3=NA,k4=NA,k5=NA,k6=NA,k7=NA,k8=NA,k9=NA,k10=NA)
lda.rec <- data.frame(iter=c(1:10),k2=NA,k3=NA,k4=NA,k5=NA,k6=NA,k7=NA,k8=NA,k9=NA,k10=NA)
lda.prec <- data.frame(iter=c(1:10),k2=NA,k3=NA,k4=NA,k5=NA,k6=NA,k7=NA,k8=NA,k9=NA,k10=NA)
lda.F1 <- data.frame(iter=c(1:10),k2=NA,k3=NA,k4=NA,k5=NA,k6=NA,k7=NA,k8=NA,k9=NA,k10=NA)
lda.FP <- data.frame(iter=c(1:10),k2=NA,k3=NA,k4=NA,k5=NA,k6=NA,k7=NA,k8=NA,k9=NA,k10=NA)
svm.rec <- data.frame(iter=c(1:10),k2=NA,k3=NA,k4=NA,k5=NA,k6=NA,k7=NA,k8=NA,k9=NA,k10=NA)
svm.prec <- data.frame(iter=c(1:10),k2=NA,k3=NA,k4=NA,k5=NA,k6=NA,k7=NA,k8=NA,k9=NA,k10=NA)
svm.F1 <- data.frame(iter=c(1:10),k2=NA,k3=NA,k4=NA,k5=NA,k6=NA,k7=NA,k8=NA,k9=NA,k10=NA)
svm.FP <- data.frame(iter=c(1:10),k2=NA,k3=NA,k4=NA,k5=NA,k6=NA,k7=NA,k8=NA,k9=NA,k10=NA)
lda.acc.tot <- data.frame()
svm.acc.tot <- data.frame()
lda.rec.tot <-  data.frame()
svm.rec.tot <-  data.frame()
lda.prec.tot <-  data.frame()
svm.prec.tot <-  data.frame()
lda.FP.tot <-  data.frame()
svm.FP.tot <-  data.frame()
lda.F1.tot <-  data.frame()
svm.F1.tot <-  data.frame()
?cvFolds
for(i in 1:10){
  fold <- cvFolds(nrow(dat),K=kfold,type="random")
  for(k in 1:kfold){
    print(paste0(i,":",k))
    vp=fold$subsets[which(fold$which == k)]
    #validation population includes everything not in training population
    tp=setdiff(1:nrow(dat),vp)
    
    train = dat[tp,]; test = dat[vp,]
    
    k2.train <- factor(data$modK2[tp]); k2.test <- factor(data$modK2[vp])
    k3.train <- factor(data$modK3[tp]); k3.test <- factor(data$modK3[vp])
    k4.train <- factor(data$modK4[tp]); k4.test <- factor(data$modK4[vp])
    k5.train <- factor(data$modK5[tp]); k5.test <- factor(data$modK5[vp])
    k6.train <- factor(data$modK6[tp]); k6.test <- factor(data$modK6[vp])
    k7.train <- factor(data$modK7[tp]); k7.test <- factor(data$modK7[vp])
    k8.train <- factor(data$modK8[tp]); k8.test <- factor(data$modK8[vp])
    k9.train <- factor(data$modK9[tp]); k9.test <- factor(data$modK9[vp])
    k10.train <- factor(data$modK10[tp]); k10.test <- factor(data$modK10[vp])
    
    #Full Test vs Train LDA
    k2.lda <- lda(k2.train ~ ., train); k2.lda.pred <- predict(k2.lda, test)$class
    k3.lda <- lda(k3.train ~ ., train); k3.lda.pred <- predict(k3.lda, test)$class
    k4.lda <- lda(k4.train ~ ., train); k4.lda.pred <- predict(k4.lda, test)$class
    k5.lda <- lda(k5.train ~ ., train); k5.lda.pred <- predict(k5.lda, test)$class
    k6.lda <- lda(k6.train ~ ., train); k6.lda.pred <- predict(k6.lda, test)$class
    k7.lda <- lda(k7.train ~ ., train); k7.lda.pred <- predict(k7.lda, test)$class
    k8.lda <- lda(k8.train ~ ., train); k8.lda.pred <- predict(k8.lda, test)$class
    k9.lda <- lda(k9.train ~ ., train); k9.lda.pred <- predict(k9.lda, test)$class
    k10.lda <- lda(k10.train ~ ., train); k10.lda.pred <- predict(k10.lda, test)$class
    
    #Full Test vs Train SVM
    k2.svm <- svm(k2.train ~ ., train); k2.svm.pred <- predict(k2.svm, test)
    k3.svm <- svm(k3.train ~ ., train); k3.svm.pred <- predict(k3.svm, test)
    k4.svm <- svm(k4.train ~ ., train); k4.svm.pred <- predict(k4.svm, test)
    k5.svm <- svm(k5.train ~ ., train); k5.svm.pred <- predict(k5.svm, test)
    k6.svm <- svm(k6.train ~ ., train); k6.svm.pred <- predict(k6.svm, test)
    k7.svm <- svm(k7.train ~ ., train); k7.svm.pred <- predict(k7.svm, test)
    k8.svm <- svm(k8.train ~ ., train); k8.svm.pred <- predict(k8.svm, test)
    k9.svm <- svm(k9.train ~ ., train); k9.svm.pred <- predict(k9.svm, test)
    k10.svm <- svm(k10.train ~ ., train); k10.svm.pred <- predict(k10.svm, test)
    
    
    ###### Linear Discriminants
    x <- table(k2.lda.pred,k2.test); lda.acc$k2[k] <- 1-((sum(x) - sum(diag(x))) / sum(x))
    lda.rec$k2[k] <- mean(c(x[1,1] / sum(x[,1]), 
                            x[2,2] / sum(x[,2])
    ))
    lda.prec$k2[k] <- mean(c(x[1,1] / sum(x[1,]), 
                             x[2,2] / sum(x[2,])
    ))
    lda.FP$k2[k] <- mean(c((sum(x[1,]) - x[1,1]) / (sum(x) - sum(x[,1])), 
                           (sum(x[2,]) - x[2,2]) / (sum(x) - sum(x[,2]))
    ))
    lda.F1$k2[k] = 2*((lda.rec$k2[k] * lda.prec$k2[k]) / (lda.rec$k2[k] + lda.prec$k2[k]))
    x <- table(k3.lda.pred,k3.test); lda.acc$k3[k] <- 1-((sum(x) - sum(diag(x))) / sum(x))
    lda.rec$k3[k] <- mean(c(x[1,1] / sum(x[,1]), 
                            x[2,2] / sum(x[,2]),
                            x[3,3] / sum(x[,3])
    ))
    lda.prec$k3[k] <- mean(c(x[1,1] / sum(x[1,]), 
                             x[2,2] / sum(x[2,]),
                             x[3,3] / sum(x[3,])
    ))
    lda.FP$k3[k] <- mean(c((sum(x[1,]) - x[1,1]) / (sum(x) - sum(x[,1])), 
                           (sum(x[2,]) - x[2,2]) / (sum(x) - sum(x[,2])),
                           (sum(x[3,]) - x[3,3]) / (sum(x) - sum(x[,3]))
    ))
    lda.F1$k3[k] = 2*((lda.rec$k3[k] * lda.prec$k3[k]) / (lda.rec$k3[k] + lda.prec$k3[k]))
    x <- table(k4.lda.pred,k4.test); lda.acc$k4[k] <-  1-((sum(x) - sum(diag(x))) / sum(x))
    lda.rec$k4[k] <- mean(c(x[1,1] / sum(x[,1]), 
                            x[2,2] / sum(x[,2]),
                            x[3,3] / sum(x[,3]),
                            x[4,4] / sum(x[,4])
    ))
    lda.prec$k4[k] <- mean(c(x[1,1] / sum(x[1,]), 
                             x[2,2] / sum(x[2,]),
                             x[3,3] / sum(x[3,]),
                             x[4,4] / sum(x[4,])
    ))
    lda.FP$k4[k] <- mean(c((sum(x[1,]) - x[1,1]) / (sum(x) - sum(x[,1])), 
                           (sum(x[2,]) - x[2,2]) / (sum(x) - sum(x[,2])),
                           (sum(x[3,]) - x[3,3]) / (sum(x) - sum(x[,3])),
                           (sum(x[4,]) - x[4,4]) / (sum(x) - sum(x[,4]))
    ))
    lda.F1$k4[k] = 2*((lda.rec$k4[k] * lda.prec$k4[k]) / (lda.rec$k4[k] + lda.prec$k4[k]))
    x <- table(k5.lda.pred,k5.test); lda.acc$k5[k] <-  1-((sum(x) - sum(diag(x))) / sum(x))
    lda.rec$k5[k] <- mean(c(x[1,1] / sum(x[,1]), 
                            x[2,2] / sum(x[,2]),
                            x[3,3] / sum(x[,3]),
                            x[4,4] / sum(x[,4]),
                            x[5,5] / sum(x[,5])
    ))
    lda.prec$k5[k] <- mean(c(x[1,1] / sum(x[1,]), 
                             x[2,2] / sum(x[2,]),
                             x[3,3] / sum(x[3,]),
                             x[4,4] / sum(x[4,]),
                             x[5,5] / sum(x[5,])
    ))
    lda.FP$k5[k] <- mean(c((sum(x[1,]) - x[1,1]) / (sum(x) - sum(x[,1])), 
                           (sum(x[2,]) - x[2,2]) / (sum(x) - sum(x[,2])),
                           (sum(x[3,]) - x[3,3]) / (sum(x) - sum(x[,3])),
                           (sum(x[4,]) - x[4,4]) / (sum(x) - sum(x[,4])),
                           (sum(x[5,]) - x[5,5]) / (sum(x) - sum(x[,5]))
    ))
    lda.F1$k5[k] = 2*((lda.rec$k5[k] * lda.prec$k5[k]) / (lda.rec$k5[k] + lda.prec$k5[k]))
    x <- table(k6.lda.pred,k6.test); lda.acc$k6[k] <-  1-((sum(x) - sum(diag(x))) / sum(x))
    lda.rec$k6[k] <- mean(c(x[1,1] / sum(x[,1]), 
                            x[2,2] / sum(x[,2]),
                            x[3,3] / sum(x[,3]),
                            x[4,4] / sum(x[,4]),
                            x[5,5] / sum(x[,5]),
                            x[6,6] / sum(x[,6])
    ))
    lda.prec$k6[k] <- mean(c(x[1,1] / sum(x[1,]), 
                             x[2,2] / sum(x[2,]),
                             x[3,3] / sum(x[3,]),
                             x[4,4] / sum(x[4,]),
                             x[5,5] / sum(x[5,]),
                             x[6,6] / sum(x[6,])
    ))
    lda.FP$k6[k] <- mean(c((sum(x[1,]) - x[1,1]) / (sum(x) - sum(x[,1])), 
                           (sum(x[2,]) - x[2,2]) / (sum(x) - sum(x[,2])),
                           (sum(x[3,]) - x[3,3]) / (sum(x) - sum(x[,3])),
                           (sum(x[4,]) - x[4,4]) / (sum(x) - sum(x[,4])),
                           (sum(x[5,]) - x[5,5]) / (sum(x) - sum(x[,5])),
                           (sum(x[6,]) - x[6,6]) / (sum(x) - sum(x[,6]))
    ))
    lda.F1$k6[k] = 2*((lda.rec$k6[k] * lda.prec$k6[k]) / (lda.rec$k6[k] + lda.prec$k6[k]))
    x <- table(k7.lda.pred,k7.test); lda.acc$k7[k] <-  1-((sum(x) - sum(diag(x))) / sum(x))
    lda.rec$k7[k] <- mean(c(x[1,1] / sum(x[,1]), 
                            x[2,2] / sum(x[,2]),
                            x[3,3] / sum(x[,3]),
                            x[4,4] / sum(x[,4]),
                            x[5,5] / sum(x[,5]),
                            x[6,6] / sum(x[,6]),
                            x[7,7] / sum(x[,7])
    ))
    lda.prec$k7[k] <- mean(c(x[1,1] / sum(x[1,]), 
                             x[2,2] / sum(x[2,]),
                             x[3,3] / sum(x[3,]),
                             x[4,4] / sum(x[4,]),
                             x[5,5] / sum(x[5,]),
                             x[6,6] / sum(x[6,]),
                             x[7,7] / sum(x[7,])
    ))
    lda.FP$k7[k] <- mean(c((sum(x[1,]) - x[1,1]) / (sum(x) - sum(x[,1])), 
                           (sum(x[2,]) - x[2,2]) / (sum(x) - sum(x[,2])),
                           (sum(x[3,]) - x[3,3]) / (sum(x) - sum(x[,3])),
                           (sum(x[4,]) - x[4,4]) / (sum(x) - sum(x[,4])),
                           (sum(x[5,]) - x[5,5]) / (sum(x) - sum(x[,5])),
                           (sum(x[6,]) - x[6,6]) / (sum(x) - sum(x[,6])),
                           (sum(x[7,]) - x[7,7]) / (sum(x) - sum(x[,7]))
    ))
    lda.F1$k7[k] = 2*((lda.rec$k7[k] * lda.prec$k7[k]) / (lda.rec$k7[k] + lda.prec$k7[k]))
    x <- table(k8.lda.pred,k8.test); lda.acc$k8[k] <-  1-((sum(x) - sum(diag(x))) / sum(x))
    lda.rec$k8[k] <- mean(c(x[1,1] / sum(x[,1]), 
                            x[2,2] / sum(x[,2]),
                            x[3,3] / sum(x[,3]),
                            x[4,4] / sum(x[,4]),
                            x[5,5] / sum(x[,5]),
                            x[6,6] / sum(x[,6]),
                            x[7,7] / sum(x[,7]),
                            x[8,8] / sum(x[,8])
    ))
    lda.prec$k8[k] <- mean(c(x[1,1] / sum(x[1,]), 
                             x[2,2] / sum(x[2,]),
                             x[3,3] / sum(x[3,]),
                             x[4,4] / sum(x[4,]),
                             x[5,5] / sum(x[5,]),
                             x[6,6] / sum(x[6,]),
                             x[7,7] / sum(x[7,]),
                             x[8,8] / sum(x[8,])
    ))
    lda.FP$k8[k] <- mean(c((sum(x[1,]) - x[1,1]) / (sum(x) - sum(x[,1])), 
                           (sum(x[2,]) - x[2,2]) / (sum(x) - sum(x[,2])),
                           (sum(x[3,]) - x[3,3]) / (sum(x) - sum(x[,3])),
                           (sum(x[4,]) - x[4,4]) / (sum(x) - sum(x[,4])),
                           (sum(x[5,]) - x[5,5]) / (sum(x) - sum(x[,5])),
                           (sum(x[6,]) - x[6,6]) / (sum(x) - sum(x[,6])),
                           (sum(x[7,]) - x[7,7]) / (sum(x) - sum(x[,7])),
                           (sum(x[8,]) - x[8,8]) / (sum(x) - sum(x[,8]))
    ))
    lda.F1$k8[k] = 2*((lda.rec$k8[k] * lda.prec$k8[k]) / (lda.rec$k8[k] + lda.prec$k8[k]))
    x <- table(k9.lda.pred,k9.test); lda.acc$k9[k] <-  1-((sum(x) - sum(diag(x))) / sum(x))
    lda.rec$k9[k] <- mean(c(x[1,1] / sum(x[,1]), 
                            x[2,2] / sum(x[,2]),
                            x[3,3] / sum(x[,3]),
                            x[4,4] / sum(x[,4]),
                            x[5,5] / sum(x[,5]),
                            x[6,6] / sum(x[,6]),
                            x[7,7] / sum(x[,7]),
                            x[8,8] / sum(x[,8]),
                            x[9,9] / sum(x[,9])
    ))
    lda.prec$k9[k] <- mean(c(x[1,1] / sum(x[1,]), 
                             x[2,2] / sum(x[2,]),
                             x[3,3] / sum(x[3,]),
                             x[4,4] / sum(x[4,]),
                             x[5,5] / sum(x[5,]),
                             x[6,6] / sum(x[6,]),
                             x[7,7] / sum(x[7,]),
                             x[8,8] / sum(x[8,]),
                             x[9,9] / sum(x[9,])
    ))
    lda.FP$k9[k] <- mean(c((sum(x[1,]) - x[1,1]) / (sum(x) - sum(x[,1])), 
                           (sum(x[2,]) - x[2,2]) / (sum(x) - sum(x[,2])),
                           (sum(x[3,]) - x[3,3]) / (sum(x) - sum(x[,3])),
                           (sum(x[4,]) - x[4,4]) / (sum(x) - sum(x[,4])),
                           (sum(x[5,]) - x[5,5]) / (sum(x) - sum(x[,5])),
                           (sum(x[6,]) - x[6,6]) / (sum(x) - sum(x[,6])),
                           (sum(x[7,]) - x[7,7]) / (sum(x) - sum(x[,7])),
                           (sum(x[8,]) - x[8,8]) / (sum(x) - sum(x[,8])),
                           (sum(x[9,]) - x[9,9]) / (sum(x) - sum(x[,9]))
    ))
    lda.F1$k9[k] = 2*((lda.rec$k9[k] * lda.prec$k9[k]) / (lda.rec$k9[k] + lda.prec$k9[k]))
    x <- table(k10.lda.pred,k10.test); lda.acc$k10[k] <-  1-((sum(x) - sum(diag(x))) / sum(x))
    lda.rec$k10[k] <- mean(c(x[1,1] / sum(x[,1]), 
                             x[2,2] / sum(x[,2]),
                             x[3,3] / sum(x[,3]),
                             x[4,4] / sum(x[,4]),
                             x[5,5] / sum(x[,5]),
                             x[6,6] / sum(x[,6]),
                             x[7,7] / sum(x[,7]),
                             x[8,8] / sum(x[,8]),
                             x[9,9] / sum(x[,9]),
                             x[10,10] / sum(x[,10])
    ))
    lda.prec$k10[k] <- mean(c(x[1,1] / sum(x[1,]), 
                              x[2,2] / sum(x[2,]),
                              x[3,3] / sum(x[3,]),
                              x[4,4] / sum(x[4,]),
                              x[5,5] / sum(x[5,]),
                              x[6,6] / sum(x[6,]),
                              x[7,7] / sum(x[7,]),
                              x[8,8] / sum(x[8,]),
                              x[9,9] / sum(x[9,]),
                              x[10,10] / sum(x[10,])
    ))
    lda.FP$k10[k] <- mean(c((sum(x[1,]) - x[1,1]) / (sum(x) - sum(x[,1])), 
                            (sum(x[2,]) - x[2,2]) / (sum(x) - sum(x[,2])),
                            (sum(x[3,]) - x[3,3]) / (sum(x) - sum(x[,3])),
                            (sum(x[4,]) - x[4,4]) / (sum(x) - sum(x[,4])),
                            (sum(x[5,]) - x[5,5]) / (sum(x) - sum(x[,5])),
                            (sum(x[6,]) - x[6,6]) / (sum(x) - sum(x[,6])),
                            (sum(x[7,]) - x[7,7]) / (sum(x) - sum(x[,7])),
                            (sum(x[8,]) - x[8,8]) / (sum(x) - sum(x[,8])),
                            (sum(x[9,]) - x[9,9]) / (sum(x) - sum(x[,9])),
                            (sum(x[10,]) - x[10,10]) / (sum(x) - sum(x[,10]))
    ))
    lda.F1$k10[k] = 2*((lda.rec$k10[k] * lda.prec$k10[k]) / (lda.rec$k10[k] + lda.prec$k10[k]))
    
    ###### SUPPORT VECTORS
    x <- table(k2.svm.pred,k2.test); svm.acc$k2[k] <- 1-((sum(x) - sum(diag(x))) / sum(x))
    svm.rec$k2[k] <- mean(c(x[1,1] / sum(x[,1]), 
                            x[2,2] / sum(x[,2])
    ))
    svm.prec$k2[k] <- mean(c(x[1,1] / sum(x[1,]), 
                             x[2,2] / sum(x[2,])
    ))
    svm.FP$k2[k] <- mean(c((sum(x[1,]) - x[1,1]) / (sum(x) - sum(x[,1])), 
                           (sum(x[2,]) - x[2,2]) / (sum(x) - sum(x[,2]))
    ))
    svm.F1$k2[k] = 2*((svm.rec$k2[k] * svm.prec$k2[k]) / (svm.rec$k2[k] + svm.prec$k2[k]))
    x <- table(k3.svm.pred,k3.test); svm.acc$k3[k] <- 1-((sum(x) - sum(diag(x))) / sum(x))
    svm.rec$k3[k] <- mean(c(x[1,1] / sum(x[,1]), 
                            x[2,2] / sum(x[,2]),
                            x[3,3] / sum(x[,3])
    ))
    svm.prec$k3[k] <- mean(c(x[1,1] / sum(x[1,]), 
                             x[2,2] / sum(x[2,]),
                             x[3,3] / sum(x[3,])
    ))
    svm.FP$k3[k] <- mean(c((sum(x[1,]) - x[1,1]) / (sum(x) - sum(x[,1])), 
                           (sum(x[2,]) - x[2,2]) / (sum(x) - sum(x[,2])),
                           (sum(x[3,]) - x[3,3]) / (sum(x) - sum(x[,3]))
    ))
    svm.F1$k3[k] = 2*((svm.rec$k3[k] * svm.prec$k3[k]) / (svm.rec$k3[k] + svm.prec$k3[k]))
    x <- table(k4.svm.pred,k4.test); svm.acc$k4[k] <-  1-((sum(x) - sum(diag(x))) / sum(x))
    svm.rec$k4[k] <- mean(c(x[1,1] / sum(x[,1]), 
                            x[2,2] / sum(x[,2]),
                            x[3,3] / sum(x[,3]),
                            x[4,4] / sum(x[,4])
    ))
    svm.prec$k4[k] <- mean(c(x[1,1] / sum(x[1,]), 
                             x[2,2] / sum(x[2,]),
                             x[3,3] / sum(x[3,]),
                             x[4,4] / sum(x[4,])
    ))
    svm.FP$k4[k] <- mean(c((sum(x[1,]) - x[1,1]) / (sum(x) - sum(x[,1])), 
                           (sum(x[2,]) - x[2,2]) / (sum(x) - sum(x[,2])),
                           (sum(x[3,]) - x[3,3]) / (sum(x) - sum(x[,3])),
                           (sum(x[4,]) - x[4,4]) / (sum(x) - sum(x[,4]))
    ))
    svm.F1$k4[k] = 2*((svm.rec$k4[k] * svm.prec$k4[k]) / (svm.rec$k4[k] + svm.prec$k4[k]))
    x <- table(k5.svm.pred,k5.test); svm.acc$k5[k] <-  1-((sum(x) - sum(diag(x))) / sum(x))
    svm.rec$k5[k] <- mean(c(x[1,1] / sum(x[,1]), 
                            x[2,2] / sum(x[,2]),
                            x[3,3] / sum(x[,3]),
                            x[4,4] / sum(x[,4]),
                            x[5,5] / sum(x[,5])
    ))
    svm.prec$k5[k] <- mean(c(x[1,1] / sum(x[1,]), 
                             x[2,2] / sum(x[2,]),
                             x[3,3] / sum(x[3,]),
                             x[4,4] / sum(x[4,]),
                             x[5,5] / sum(x[5,])
    ))
    svm.FP$k5[k] <- mean(c((sum(x[1,]) - x[1,1]) / (sum(x) - sum(x[,1])), 
                           (sum(x[2,]) - x[2,2]) / (sum(x) - sum(x[,2])),
                           (sum(x[3,]) - x[3,3]) / (sum(x) - sum(x[,3])),
                           (sum(x[4,]) - x[4,4]) / (sum(x) - sum(x[,4])),
                           (sum(x[5,]) - x[5,5]) / (sum(x) - sum(x[,5]))
    ))
    svm.F1$k5[k] = 2*((svm.rec$k5[k] * svm.prec$k5[k]) / (svm.rec$k5[k] + svm.prec$k5[k]))
    x <- table(k6.svm.pred,k6.test); svm.acc$k6[k] <-  1-((sum(x) - sum(diag(x))) / sum(x))
    svm.rec$k6[k] <- mean(c(x[1,1] / sum(x[,1]), 
                            x[2,2] / sum(x[,2]),
                            x[3,3] / sum(x[,3]),
                            x[4,4] / sum(x[,4]),
                            x[5,5] / sum(x[,5]),
                            x[6,6] / sum(x[,6])
    ))
    svm.prec$k6[k] <- mean(c(x[1,1] / sum(x[1,]), 
                             x[2,2] / sum(x[2,]),
                             x[3,3] / sum(x[3,]),
                             x[4,4] / sum(x[4,]),
                             x[5,5] / sum(x[5,]),
                             x[6,6] / sum(x[6,])
    ))
    svm.FP$k6[k] <- mean(c((sum(x[1,]) - x[1,1]) / (sum(x) - sum(x[,1])), 
                           (sum(x[2,]) - x[2,2]) / (sum(x) - sum(x[,2])),
                           (sum(x[3,]) - x[3,3]) / (sum(x) - sum(x[,3])),
                           (sum(x[4,]) - x[4,4]) / (sum(x) - sum(x[,4])),
                           (sum(x[5,]) - x[5,5]) / (sum(x) - sum(x[,5])),
                           (sum(x[6,]) - x[6,6]) / (sum(x) - sum(x[,6]))
    ))
    svm.F1$k6[k] = 2*((svm.rec$k6[k] * svm.prec$k6[k]) / (svm.rec$k6[k] + svm.prec$k6[k]))
    x <- table(k7.svm.pred,k7.test); svm.acc$k7[k] <-  1-((sum(x) - sum(diag(x))) / sum(x))
    svm.rec$k7[k] <- mean(c(x[1,1] / sum(x[,1]), 
                            x[2,2] / sum(x[,2]),
                            x[3,3] / sum(x[,3]),
                            x[4,4] / sum(x[,4]),
                            x[5,5] / sum(x[,5]),
                            x[6,6] / sum(x[,6]),
                            x[7,7] / sum(x[,7])
    ))
    svm.prec$k7[k] <- mean(c(x[1,1] / sum(x[1,]), 
                             x[2,2] / sum(x[2,]),
                             x[3,3] / sum(x[3,]),
                             x[4,4] / sum(x[4,]),
                             x[5,5] / sum(x[5,]),
                             x[6,6] / sum(x[6,]),
                             x[7,7] / sum(x[7,])
    ))
    svm.FP$k7[k] <- mean(c((sum(x[1,]) - x[1,1]) / (sum(x) - sum(x[,1])), 
                           (sum(x[2,]) - x[2,2]) / (sum(x) - sum(x[,2])),
                           (sum(x[3,]) - x[3,3]) / (sum(x) - sum(x[,3])),
                           (sum(x[4,]) - x[4,4]) / (sum(x) - sum(x[,4])),
                           (sum(x[5,]) - x[5,5]) / (sum(x) - sum(x[,5])),
                           (sum(x[6,]) - x[6,6]) / (sum(x) - sum(x[,6])),
                           (sum(x[7,]) - x[7,7]) / (sum(x) - sum(x[,7]))
    ))
    svm.F1$k7[k] = 2*((svm.rec$k7[k] * svm.prec$k7[k]) / (svm.rec$k7[k] + svm.prec$k7[k]))
    x <- table(k8.svm.pred,k8.test); svm.acc$k8[k] <-  1-((sum(x) - sum(diag(x))) / sum(x))
    svm.rec$k8[k] <- mean(c(x[1,1] / sum(x[,1]), 
                            x[2,2] / sum(x[,2]),
                            x[3,3] / sum(x[,3]),
                            x[4,4] / sum(x[,4]),
                            x[5,5] / sum(x[,5]),
                            x[6,6] / sum(x[,6]),
                            x[7,7] / sum(x[,7]),
                            x[8,8] / sum(x[,8])
    ))
    svm.prec$k8[k] <- mean(c(x[1,1] / sum(x[1,]), 
                             x[2,2] / sum(x[2,]),
                             x[3,3] / sum(x[3,]),
                             x[4,4] / sum(x[4,]),
                             x[5,5] / sum(x[5,]),
                             x[6,6] / sum(x[6,]),
                             x[7,7] / sum(x[7,]),
                             x[8,8] / sum(x[8,])
    ))
    svm.FP$k8[k] <- mean(c((sum(x[1,]) - x[1,1]) / (sum(x) - sum(x[,1])), 
                           (sum(x[2,]) - x[2,2]) / (sum(x) - sum(x[,2])),
                           (sum(x[3,]) - x[3,3]) / (sum(x) - sum(x[,3])),
                           (sum(x[4,]) - x[4,4]) / (sum(x) - sum(x[,4])),
                           (sum(x[5,]) - x[5,5]) / (sum(x) - sum(x[,5])),
                           (sum(x[6,]) - x[6,6]) / (sum(x) - sum(x[,6])),
                           (sum(x[7,]) - x[7,7]) / (sum(x) - sum(x[,7])),
                           (sum(x[8,]) - x[8,8]) / (sum(x) - sum(x[,8]))
    ))
    svm.F1$k8[k] = 2*((svm.rec$k8[k] * svm.prec$k8[k]) / (svm.rec$k8[k] + svm.prec$k8[k]))
    x <- table(k9.svm.pred,k9.test); svm.acc$k9[k] <-  1-((sum(x) - sum(diag(x))) / sum(x))
    svm.rec$k9[k] <- mean(c(x[1,1] / sum(x[,1]), 
                            x[2,2] / sum(x[,2]),
                            x[3,3] / sum(x[,3]),
                            x[4,4] / sum(x[,4]),
                            x[5,5] / sum(x[,5]),
                            x[6,6] / sum(x[,6]),
                            x[7,7] / sum(x[,7]),
                            x[8,8] / sum(x[,8]),
                            x[9,9] / sum(x[,9])
    ))
    svm.prec$k9[k] <- mean(c(x[1,1] / sum(x[1,]), 
                             x[2,2] / sum(x[2,]),
                             x[3,3] / sum(x[3,]),
                             x[4,4] / sum(x[4,]),
                             x[5,5] / sum(x[5,]),
                             x[6,6] / sum(x[6,]),
                             x[7,7] / sum(x[7,]),
                             x[8,8] / sum(x[8,]),
                             x[9,9] / sum(x[9,])
    ))
    svm.FP$k9[k] <- mean(c((sum(x[1,]) - x[1,1]) / (sum(x) - sum(x[,1])), 
                           (sum(x[2,]) - x[2,2]) / (sum(x) - sum(x[,2])),
                           (sum(x[3,]) - x[3,3]) / (sum(x) - sum(x[,3])),
                           (sum(x[4,]) - x[4,4]) / (sum(x) - sum(x[,4])),
                           (sum(x[5,]) - x[5,5]) / (sum(x) - sum(x[,5])),
                           (sum(x[6,]) - x[6,6]) / (sum(x) - sum(x[,6])),
                           (sum(x[7,]) - x[7,7]) / (sum(x) - sum(x[,7])),
                           (sum(x[8,]) - x[8,8]) / (sum(x) - sum(x[,8])),
                           (sum(x[9,]) - x[9,9]) / (sum(x) - sum(x[,9]))
    ))
    svm.F1$k9[k] = 2*((svm.rec$k9[k] * svm.prec$k9[k]) / (svm.rec$k9[k] + svm.prec$k9[k]))
    x <- table(k10.svm.pred,k10.test); svm.acc$k10[k] <-  1-((sum(x) - sum(diag(x))) / sum(x))
    svm.rec$k10[k] <- mean(c(x[1,1] / sum(x[,1]), 
                             x[2,2] / sum(x[,2]),
                             x[3,3] / sum(x[,3]),
                             x[4,4] / sum(x[,4]),
                             x[5,5] / sum(x[,5]),
                             x[6,6] / sum(x[,6]),
                             x[7,7] / sum(x[,7]),
                             x[8,8] / sum(x[,8]),
                             x[9,9] / sum(x[,9]),
                             x[10,10] / sum(x[,10])
    ))
    svm.prec$k10[k] <- mean(c(x[1,1] / sum(x[1,]), 
                              x[2,2] / sum(x[2,]),
                              x[3,3] / sum(x[3,]),
                              x[4,4] / sum(x[4,]),
                              x[5,5] / sum(x[5,]),
                              x[6,6] / sum(x[6,]),
                              x[7,7] / sum(x[7,]),
                              x[8,8] / sum(x[8,]),
                              x[9,9] / sum(x[9,]),
                              x[10,10] / sum(x[10,])
    ))
    svm.FP$k10[k] <- mean(c((sum(x[1,]) - x[1,1]) / (sum(x) - sum(x[,1])), 
                            (sum(x[2,]) - x[2,2]) / (sum(x) - sum(x[,2])),
                            (sum(x[3,]) - x[3,3]) / (sum(x) - sum(x[,3])),
                            (sum(x[4,]) - x[4,4]) / (sum(x) - sum(x[,4])),
                            (sum(x[5,]) - x[5,5]) / (sum(x) - sum(x[,5])),
                            (sum(x[6,]) - x[6,6]) / (sum(x) - sum(x[,6])),
                            (sum(x[7,]) - x[7,7]) / (sum(x) - sum(x[,7])),
                            (sum(x[8,]) - x[8,8]) / (sum(x) - sum(x[,8])),
                            (sum(x[9,]) - x[9,9]) / (sum(x) - sum(x[,9])),
                            (sum(x[10,]) - x[10,10]) / (sum(x) - sum(x[,10]))
    ))
    svm.F1$k10[k] = 2*((svm.rec$k10[k] * svm.prec$k10[k]) / (svm.rec$k10[k] + svm.prec$k10[k]))
    
  }
  lda.acc.tot <- rbind(lda.acc.tot,lda.acc)
  svm.acc.tot <- rbind(svm.acc.tot,svm.acc)
  lda.rec.tot <- rbind(lda.rec.tot,lda.rec)
  svm.rec.tot <- rbind(svm.rec.tot,svm.rec)
  lda.prec.tot <- rbind(lda.prec.tot,lda.prec)
  svm.prec.tot <- rbind(svm.prec.tot,svm.prec)
  lda.FP.tot <- rbind(lda.FP.tot,lda.FP)
  svm.FP.tot <- rbind(svm.FP.tot,svm.FP)
  lda.F1.tot <- rbind(lda.F1.tot,lda.F1)
  svm.F1.tot <- rbind(svm.F1.tot,svm.F1)
}


t(rbind(colMeans(svm.acc.tot),
        colMeans(lda.acc.tot),
        
        colMeans(svm.prec.tot),
        colMeans(lda.prec.tot),
        
        colMeans(svm.rec.tot),
        colMeans(lda.rec.tot),
        
        colMeans(svm.FP.tot),
        colMeans(lda.FP.tot),
        
        colMeans(svm.F1.tot),
        colMeans(lda.F1.tot)))
