rm(list=ls())
#### Hold out 20%
#1. Kmeans on 80% (but only to k=5)
  #. Assign new clusters based on using only 80% of full data.
  #. PPKC to compare order, does it hold up to permutations?
  #. Assign test set to nearest cluster.
#2. EigenFruit 
#3. Biomass Profiles
#4. Geometric features do not change.
#5. SVM & LDA

# project new data onto the PCA space
#scale(newdata, center = pca$center, scale = pca$scale) %*% pca$rotation 
#cl_predict(party, Cassini$x[-ind, ])


##########################################################################################
################################## (0) Set.Seed ##########################################
##########################################################################################
#set.seed(12345); train = sort(sample(c(1:6874),5500,replace = F))
#set.seed(1234); train = sort(sample(c(1:6874),3437,replace = F))
#set.seed(123); train = sort(sample(c(1:6874),1374,replace = F))
#train = sort(sample(c(1:6874),500,replace = F))

##########################################################################################
################################## (1) Setup #############################################
##########################################################################################
library(imager)
library(cluster)
library(clue)
library(keras)
library(imager)
library(lattice)
library(factoextra)
library(cluster)
library(mclust)
library(tidyverse)
library(e1071); library(MASS); library(cvTools)
library(ggplot2); library(gridExtra)

pics <- list.files(path = "/Volumes/Elements/Desktop/Reading/Strawberry_Research/Heterosis/Salinas/Data/Images/ReSized",
                   full.names = TRUE)

load_and_resize = function(path){image = load.image(path);resize(im = image,size_x = 100, size_y = 100)}
#datt = read.csv("~/Box/Desktop/Reading/Strawberry_Research/Heterosis/Salinas/Data/2018_Salinas_Fruit_Morph.csv")

# list of 4D tensors
list_of_images = lapply(pics, load_and_resize) 

# list of 2D tensors
list_of_images.mat <- lapply(list_of_images, as.matrix) 
levelplot(list_of_images.mat[[26]])

image_matrix = do.call('cbind', lapply(list_of_images.mat, as.numeric))
dim(image_matrix)
image_matrix = t(image_matrix)
dim(image_matrix)


wss.t = c()
for(i in 1:10){
  print(paste0("Iteration ",i))
  k.max <- 10
  data <- image_matrix[sort(sample(c(1:6874),3437,replace = F)),]
  wss <- sapply(1:k.max, 
                function(k){ print(paste0("K = ",k));kmeans(data, k)$tot.withinss})
  wss.t =  c(wss.t,wss)
}
k = (c(rep(1:10,10)))
sub = as.factor(c(rep(1,10),rep(2,10),rep(3,10),rep(4,10),rep(5,10),rep(6,10),rep(7,10),rep(8,10),rep(9,10),rep(10,10)))
sub2 = (c(rep(1,10),rep(11,10),rep(21,10),rep(31,10),rep(41,10),rep(51,10),rep(61,10),rep(71,10),rep(81,10),rep(91,10)))

n = 3437
rsq = ((wss.t * (n-1)) / (wss.t[sub2] * (n - k)))
aic = wss.t + 2 * 10000 * k
bic = wss.t + log(3437) * 10000 * k

fitmet = data.frame(k=as.factor(k), sub=sub,sbu2=sub2, wss.t=wss.t, rsq=rsq, aic=aic, bic=bic)

wss.p = ggplot(data=fitmet, mapping=aes(x=k, y = wss.t/(max(wss.t)), group=sub)) + geom_point() + geom_line() + theme_bw() + xlab(label = "K") + ylab(label="Standardized Within Cluster Sum of Squares") + geom_vline(xintercept=4,lty=2,col="gray")
rsq.p = ggplot(data=fitmet, mapping=aes(x=k, y = (rsq/(max(rsq))), group=sub)) + geom_point() + geom_line() + theme_bw() + xlab(label = "K") + ylab(label="1 - Standardized R Squared")+ geom_vline(xintercept=4,lty=2,col="gray")
aic.p = ggplot(data=fitmet, mapping=aes(x=k, y = aic/(max(aic)), group=sub)) + geom_point() + geom_line() + theme_bw() + xlab(label = "K") + ylab(label="Standardized AIC")+ geom_vline(xintercept=4,lty=2,col="gray")
bic.p = ggplot(data=fitmet, mapping=aes(x=k, y = bic/(max(bic)), group=sub)) + geom_point() + geom_line() + theme_bw() + xlab(label = "K") + ylab(label="Standardized BIC")+ geom_vline(xintercept=4,lty=2,col="gray")

pdf("S3_optKclust.pdf", width=6.6, height=6.6, compress=F, pointsize = 10)
grid.arrange(wss.p, rsq.p, aic.p, bic.p)
dev.off()
###################################################################################################
###################################################################################################
###################################################################################################
met15_all=list()
met5_all=list()
met2_all=list()
size = c(5500,3437,1374)

for(j in 1:length(size)){
  metrix_15 = list()
  metrix_5 = list()
  metrix_2 = list()
  for(i in 1:10){
    print(paste0("Sample size: ",size[j],"; ", i))
    train.im = sort(sample(c(1:6874),size[j],replace = F))
    
    ##########################################################################################
    ################################## (1) Kmeans ############################################
    ##########################################################################################
    print(paste0("Start K-means; ", size[j],"; ",i))
    print("k2")
    fit2 = kmeans(image_matrix[train.im,], centers = 2)
    print("k3")
    fit3 = kmeans(image_matrix[train.im,], centers = 3)
    print("k4")
    fit4 = kmeans(image_matrix[train.im,], centers = 4)
    print("k5")
    fit5 = kmeans(image_matrix[train.im,], centers = 5)
    print("k6")
    fit6 = kmeans(image_matrix[train.im,], centers = 6)
    print("k7")
    fit7 = kmeans(image_matrix[train.im,], centers = 7)
    print("k8")
    fit8 = kmeans(image_matrix[train.im,], centers = 8)
    print("k9")
    fit9 = kmeans(image_matrix[train.im,], centers = 9)
    print("k10")
    fit10 = kmeans(image_matrix[train.im,], centers = 10)
    
    #test clusters
    print("Start Clustering test set")
    test.k2 = cl_predict(fit2, image_matrix[-train.im, ])
    test.k3 = cl_predict(fit3, image_matrix[-train.im, ])
    test.k4 = cl_predict(fit4, image_matrix[-train.im, ])
    test.k5 = cl_predict(fit5, image_matrix[-train.im, ])
    test.k6 = cl_predict(fit6, image_matrix[-train.im, ])
    test.k7 = cl_predict(fit7, image_matrix[-train.im, ])
    test.k8 = cl_predict(fit8, image_matrix[-train.im, ])
    test.k9 = cl_predict(fit9, image_matrix[-train.im, ])
    test.k10 = cl_predict(fit10, image_matrix[-train.im, ])
    print("Finish CLustering test set")
    print(paste0("Finish K-means; ",size[j],"; ", i))
    ##########################################################################################
    ################################## (2) EigenFruit ########################################
    ##########################################################################################
    
    print(paste0("Start PCA; ", size[j],"; ",i))
    pca = prcomp(image_matrix[train.im, ])
    print("Start Projecting Test Set")
    pca_dat_rot = scale(image_matrix[-train.im,], center = pca$center, scale = pca$scale) %*% pca$rotation 
    print("Finish Projecting Test Set")
    print(paste0("Finish PCA; ", size[j],"; ",i))
    
    ##########################################################################################
    ################################## (3) Biomass Profile ###################################
    ##########################################################################################
    v.biomass <- matrix(rep(0,100*length(list_of_images)),ncol=100)
    h.biomass <- matrix(rep(0,100*length(list_of_images)),ncol=100)
    print(paste0("Start BMP; ", size[j],"; ",i))
    for(k in 1:length(list_of_images)){
      v.biomass[k,] <- 100 - rowSums(list_of_images[[k]])
      h.biomass[k,] <- 100 - colSums(list_of_images[[k]])
    }
    
    vpca <- prcomp(v.biomass[train.im,])
    hpca <- prcomp(h.biomass[train.im,])
    print("Start Projecting Test Set")
    vpca_rot = scale(v.biomass[-train.im,], center = vpca$center, scale = vpca$scale) %*% vpca$rotation 
    hpca_rot = scale(h.biomass[-train.im,], center = hpca$center, scale = hpca$scale) %*% hpca$rotation 
    print("Finish Projecting Test Set")
    print(paste0("Finish BMP; ",size[j],"; ",i))
    
    ##########################################################################################
    ################################## (4) Data ##############################################
    ##########################################################################################
    print(paste0("Start Train/Test Build; ",size[j],"; ",i))
    #general info
    train.data = KM.dat[train.im,c(1:13)]; test.data = KM.dat[-train.im,c(1:13)]
    
    #clusters
    train.data$k2 = fit2$cluster; test.data$k2 = test.k2
    train.data$k3 = fit3$cluster; test.data$k3 = test.k3
    train.data$k4 = fit4$cluster; test.data$k4 = test.k4
    train.data$k5 = fit5$cluster; test.data$k5 = test.k5
    train.data$k6 = fit6$cluster; test.data$k6 = test.k6
    train.data$k7 = fit7$cluster; test.data$k7 = test.k7
    train.data$k8 = fit8$cluster; test.data$k8 = test.k8
    train.data$k9 = fit9$cluster; test.data$k9 = test.k9
    train.data$k10 = fit10$cluster; test.data$k10 = test.k10
    
    #eigenfruit
    train.data$EFPC1 = pca$x[,1]; test.data$EFPC1  = pca_dat_rot[,1]
    train.data$EFPC2 = pca$x[,2]; test.data$EFPC2  = pca_dat_rot[,2]
    train.data$EFPC3 = pca$x[,3]; test.data$EFPC3  = pca_dat_rot[,3]
    train.data$EFPC4 = pca$x[,4]; test.data$EFPC4  = pca_dat_rot[,4]
    train.data$EFPC5 = pca$x[,5]; test.data$EFPC5  = pca_dat_rot[,5]
    train.data$EFPC6 = pca$x[,6]; test.data$EFPC6  = pca_dat_rot[,6]
    train.data$EFPC7 = pca$x[,7]; test.data$EFPC7  = pca_dat_rot[,7]
    
    #biomass
    train.data$BVPC1 = vpca$x[,1]; test.data$BVPC1 = vpca_rot[,1]
    train.data$BVPC2 = vpca$x[,2]; test.data$BVPC2 = vpca_rot[,2]
    train.data$BHPC1 = hpca$x[,1]; test.data$BHPC1 = hpca_rot[,1]
    train.data$BHPC2 = hpca$x[,2]; test.data$BHPC2 = hpca_rot[,2]
    train.data$BHPC3 = hpca$x[,3]; test.data$BHPC3 = hpca_rot[,3]
    
    #geom
    train.data$BAR = datt$BAR[train.im]; test.data$BAR = datt$BAR[-train.im] 
    train.data$Kurt = datt$Kurt[train.im]; test.data$Kurt = datt$Kurt[-train.im] 
    train.data$SI = datt$SI[train.im]; test.data$SI = datt$SI[-train.im] 
    print(paste0("Finish Build; ",size[j],"; ",i))
    
    ##########################################################################################
    ################################## (5) Test Classification 15 Var ########################
    ##########################################################################################
    print(paste0("Start CV 15 Variables; ",size[j],"; ",i))
    lda.acc <- data.frame(k2=NA,k3=NA,k4=NA,k5=NA,k6=NA,k7=NA,k8=NA,k9=NA,k10=NA)
    svm.acc <- data.frame(k2=NA,k3=NA,k4=NA,k5=NA,k6=NA,k7=NA,k8=NA,k9=NA,k10=NA)
    lda.rec <- data.frame(k2=NA,k3=NA,k4=NA,k5=NA,k6=NA,k7=NA,k8=NA,k9=NA,k10=NA)
    lda.prec <- data.frame(k2=NA,k3=NA,k4=NA,k5=NA,k6=NA,k7=NA,k8=NA,k9=NA,k10=NA)
    lda.FP <- data.frame(k2=NA,k3=NA,k4=NA,k5=NA,k6=NA,k7=NA,k8=NA,k9=NA,k10=NA)
    svm.rec <- data.frame(k2=NA,k3=NA,k4=NA,k5=NA,k6=NA,k7=NA,k8=NA,k9=NA,k10=NA)
    svm.prec <- data.frame(k2=NA,k3=NA,k4=NA,k5=NA,k6=NA,k7=NA,k8=NA,k9=NA,k10=NA)
    svm.FP <- data.frame(k2=NA,k3=NA,k4=NA,k5=NA,k6=NA,k7=NA,k8=NA,k9=NA,k10=NA)
    
    #c(23:37)
    #c(23,24,30,32,35)
    #c(23,30)
    
    train = train.data[,c(23:37)]; test = test.data[,c(23:37)]
    
    k2.train <- factor(train.data$k2); k2.test <- factor(test.data$k2)
    k3.train <- factor(train.data$k3); k3.test <- factor(test.data$k3)
    k4.train <- factor(train.data$k4); k4.test <- factor(test.data$k4)
    k5.train <- factor(train.data$k5); k5.test <- factor(test.data$k5)
    k6.train <- factor(train.data$k6); k6.test <- factor(test.data$k6)
    k7.train <- factor(train.data$k7); k7.test <- factor(test.data$k7)
    k8.train <- factor(train.data$k8); k8.test <- factor(test.data$k8)
    k9.train <- factor(train.data$k9); k9.test <- factor(test.data$k9)
    k10.train <- factor(train.data$k10); k10.test <- factor(test.data$k10)
    
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
    x <- table(k2.lda.pred,k2.test); lda.acc$k2 <- 1-((sum(x) - sum(diag(x))) / sum(x))
    lda.rec$k2 <- mean(c(x[1,1] / sum(x[,1]), 
                         x[2,2] / sum(x[,2])
    ))
    lda.prec$k2 <- mean(c(x[1,1] / sum(x[1,]), 
                          x[2,2] / sum(x[2,])
    ))
    lda.FP$k2 <- mean(c((sum(x[1,]) - x[1,1]) / (sum(x) - sum(x[,1])), 
                        (sum(x[2,]) - x[2,2]) / (sum(x) - sum(x[,2]))
    ))
    x <- table(k3.lda.pred,k3.test); lda.acc$k3 <- 1-((sum(x) - sum(diag(x))) / sum(x))
    lda.rec$k3 <- mean(c(x[1,1] / sum(x[,1]), 
                         x[2,2] / sum(x[,2]),
                         x[3,3] / sum(x[,3])
    ))
    lda.prec$k3 <- mean(c(x[1,1] / sum(x[1,]), 
                          x[2,2] / sum(x[2,]),
                          x[3,3] / sum(x[3,])
    ))
    lda.FP$k3 <- mean(c((sum(x[1,]) - x[1,1]) / (sum(x) - sum(x[,1])), 
                        (sum(x[2,]) - x[2,2]) / (sum(x) - sum(x[,2])),
                        (sum(x[3,]) - x[3,3]) / (sum(x) - sum(x[,3]))
    ))
    x <- table(k4.lda.pred,k4.test); lda.acc$k4 <-  1-((sum(x) - sum(diag(x))) / sum(x))
    lda.rec$k4 <- mean(c(x[1,1] / sum(x[,1]), 
                         x[2,2] / sum(x[,2]),
                         x[3,3] / sum(x[,3]),
                         x[4,4] / sum(x[,4])
    ))
    lda.prec$k4 <- mean(c(x[1,1] / sum(x[1,]), 
                          x[2,2] / sum(x[2,]),
                          x[3,3] / sum(x[3,]),
                          x[4,4] / sum(x[4,])
    ))
    lda.FP$k4 <- mean(c((sum(x[1,]) - x[1,1]) / (sum(x) - sum(x[,1])), 
                        (sum(x[2,]) - x[2,2]) / (sum(x) - sum(x[,2])),
                        (sum(x[3,]) - x[3,3]) / (sum(x) - sum(x[,3])),
                        (sum(x[4,]) - x[4,4]) / (sum(x) - sum(x[,4]))
    ))
    x <- table(k5.lda.pred,k5.test); lda.acc$k5 <-  1-((sum(x) - sum(diag(x))) / sum(x))
    lda.rec$k5 <- mean(c(x[1,1] / sum(x[,1]), 
                         x[2,2] / sum(x[,2]),
                         x[3,3] / sum(x[,3]),
                         x[4,4] / sum(x[,4]),
                         x[5,5] / sum(x[,5])
    ))
    lda.prec$k5 <- mean(c(x[1,1] / sum(x[1,]), 
                          x[2,2] / sum(x[2,]),
                          x[3,3] / sum(x[3,]),
                          x[4,4] / sum(x[4,]),
                          x[5,5] / sum(x[5,])
    ))
    lda.FP$k5 <- mean(c((sum(x[1,]) - x[1,1]) / (sum(x) - sum(x[,1])), 
                        (sum(x[2,]) - x[2,2]) / (sum(x) - sum(x[,2])),
                        (sum(x[3,]) - x[3,3]) / (sum(x) - sum(x[,3])),
                        (sum(x[4,]) - x[4,4]) / (sum(x) - sum(x[,4])),
                        (sum(x[5,]) - x[5,5]) / (sum(x) - sum(x[,5]))
    ))
    x <- table(k6.lda.pred,k6.test); lda.acc$k6 <-  1-((sum(x) - sum(diag(x))) / sum(x))
    lda.rec$k6 <- mean(c(x[1,1] / sum(x[,1]), 
                         x[2,2] / sum(x[,2]),
                         x[3,3] / sum(x[,3]),
                         x[4,4] / sum(x[,4]),
                         x[5,5] / sum(x[,5]),
                         x[6,6] / sum(x[,6])
    ))
    lda.prec$k6 <- mean(c(x[1,1] / sum(x[1,]), 
                          x[2,2] / sum(x[2,]),
                          x[3,3] / sum(x[3,]),
                          x[4,4] / sum(x[4,]),
                          x[5,5] / sum(x[5,]),
                          x[6,6] / sum(x[6,])
    ))
    lda.FP$k6 <- mean(c((sum(x[1,]) - x[1,1]) / (sum(x) - sum(x[,1])), 
                        (sum(x[2,]) - x[2,2]) / (sum(x) - sum(x[,2])),
                        (sum(x[3,]) - x[3,3]) / (sum(x) - sum(x[,3])),
                        (sum(x[4,]) - x[4,4]) / (sum(x) - sum(x[,4])),
                        (sum(x[5,]) - x[5,5]) / (sum(x) - sum(x[,5])),
                        (sum(x[6,]) - x[6,6]) / (sum(x) - sum(x[,6]))
    ))
    x <- table(k7.lda.pred,k7.test); lda.acc$k7 <-  1-((sum(x) - sum(diag(x))) / sum(x))
    lda.rec$k7 <- mean(c(x[1,1] / sum(x[,1]), 
                         x[2,2] / sum(x[,2]),
                         x[3,3] / sum(x[,3]),
                         x[4,4] / sum(x[,4]),
                         x[5,5] / sum(x[,5]),
                         x[6,6] / sum(x[,6]),
                         x[7,7] / sum(x[,7])
    ))
    lda.prec$k7 <- mean(c(x[1,1] / sum(x[1,]), 
                          x[2,2] / sum(x[2,]),
                          x[3,3] / sum(x[3,]),
                          x[4,4] / sum(x[4,]),
                          x[5,5] / sum(x[5,]),
                          x[6,6] / sum(x[6,]),
                          x[7,7] / sum(x[7,])
    ))
    lda.FP$k7 <- mean(c((sum(x[1,]) - x[1,1]) / (sum(x) - sum(x[,1])), 
                        (sum(x[2,]) - x[2,2]) / (sum(x) - sum(x[,2])),
                        (sum(x[3,]) - x[3,3]) / (sum(x) - sum(x[,3])),
                        (sum(x[4,]) - x[4,4]) / (sum(x) - sum(x[,4])),
                        (sum(x[5,]) - x[5,5]) / (sum(x) - sum(x[,5])),
                        (sum(x[6,]) - x[6,6]) / (sum(x) - sum(x[,6])),
                        (sum(x[7,]) - x[7,7]) / (sum(x) - sum(x[,7]))
    ))
    x <- table(k8.lda.pred,k8.test); lda.acc$k8 <-  1-((sum(x) - sum(diag(x))) / sum(x))
    lda.rec$k8 <- mean(c(x[1,1] / sum(x[,1]), 
                         x[2,2] / sum(x[,2]),
                         x[3,3] / sum(x[,3]),
                         x[4,4] / sum(x[,4]),
                         x[5,5] / sum(x[,5]),
                         x[6,6] / sum(x[,6]),
                         x[7,7] / sum(x[,7]),
                         x[8,8] / sum(x[,8])
    ))
    lda.prec$k8 <- mean(c(x[1,1] / sum(x[1,]), 
                          x[2,2] / sum(x[2,]),
                          x[3,3] / sum(x[3,]),
                          x[4,4] / sum(x[4,]),
                          x[5,5] / sum(x[5,]),
                          x[6,6] / sum(x[6,]),
                          x[7,7] / sum(x[7,]),
                          x[8,8] / sum(x[8,])
    ))
    lda.FP$k8 <- mean(c((sum(x[1,]) - x[1,1]) / (sum(x) - sum(x[,1])), 
                        (sum(x[2,]) - x[2,2]) / (sum(x) - sum(x[,2])),
                        (sum(x[3,]) - x[3,3]) / (sum(x) - sum(x[,3])),
                        (sum(x[4,]) - x[4,4]) / (sum(x) - sum(x[,4])),
                        (sum(x[5,]) - x[5,5]) / (sum(x) - sum(x[,5])),
                        (sum(x[6,]) - x[6,6]) / (sum(x) - sum(x[,6])),
                        (sum(x[7,]) - x[7,7]) / (sum(x) - sum(x[,7])),
                        (sum(x[8,]) - x[8,8]) / (sum(x) - sum(x[,8]))
    ))
    x <- table(k9.lda.pred,k9.test); lda.acc$k9 <-  1-((sum(x) - sum(diag(x))) / sum(x))
    lda.rec$k9 <- mean(c(x[1,1] / sum(x[,1]), 
                         x[2,2] / sum(x[,2]),
                         x[3,3] / sum(x[,3]),
                         x[4,4] / sum(x[,4]),
                         x[5,5] / sum(x[,5]),
                         x[6,6] / sum(x[,6]),
                         x[7,7] / sum(x[,7]),
                         x[8,8] / sum(x[,8]),
                         x[9,9] / sum(x[,9])
    ))
    lda.prec$k9 <- mean(c(x[1,1] / sum(x[1,]), 
                          x[2,2] / sum(x[2,]),
                          x[3,3] / sum(x[3,]),
                          x[4,4] / sum(x[4,]),
                          x[5,5] / sum(x[5,]),
                          x[6,6] / sum(x[6,]),
                          x[7,7] / sum(x[7,]),
                          x[8,8] / sum(x[8,]),
                          x[9,9] / sum(x[9,])
    ))
    lda.FP$k9 <- mean(c((sum(x[1,]) - x[1,1]) / (sum(x) - sum(x[,1])), 
                        (sum(x[2,]) - x[2,2]) / (sum(x) - sum(x[,2])),
                        (sum(x[3,]) - x[3,3]) / (sum(x) - sum(x[,3])),
                        (sum(x[4,]) - x[4,4]) / (sum(x) - sum(x[,4])),
                        (sum(x[5,]) - x[5,5]) / (sum(x) - sum(x[,5])),
                        (sum(x[6,]) - x[6,6]) / (sum(x) - sum(x[,6])),
                        (sum(x[7,]) - x[7,7]) / (sum(x) - sum(x[,7])),
                        (sum(x[8,]) - x[8,8]) / (sum(x) - sum(x[,8])),
                        (sum(x[9,]) - x[9,9]) / (sum(x) - sum(x[,9]))
    ))
    x <- table(k10.lda.pred,k10.test); lda.acc$k10 <-  1-((sum(x) - sum(diag(x))) / sum(x))
    lda.rec$k10 <- mean(c(x[1,1] / sum(x[,1]), 
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
    lda.prec$k10 <- mean(c(x[1,1] / sum(x[1,]), 
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
    lda.FP$k10 <- mean(c((sum(x[1,]) - x[1,1]) / (sum(x) - sum(x[,1])), 
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
    
    ###### SUPPORT VECTORS
    x <- table(k2.svm.pred,k2.test); svm.acc$k2 <- 1-((sum(x) - sum(diag(x))) / sum(x))
    svm.rec$k2 <- mean(c(x[1,1] / sum(x[,1]), 
                         x[2,2] / sum(x[,2])
    ))
    svm.prec$k2 <- mean(c(x[1,1] / sum(x[1,]), 
                          x[2,2] / sum(x[2,])
    ))
    svm.FP$k2 <- mean(c((sum(x[1,]) - x[1,1]) / (sum(x) - sum(x[,1])), 
                        (sum(x[2,]) - x[2,2]) / (sum(x) - sum(x[,2]))
    ))
    x <- table(k3.svm.pred,k3.test); svm.acc$k3 <- 1-((sum(x) - sum(diag(x))) / sum(x))
    svm.rec$k3 <- mean(c(x[1,1] / sum(x[,1]), 
                         x[2,2] / sum(x[,2]),
                         x[3,3] / sum(x[,3])
    ))
    svm.prec$k3 <- mean(c(x[1,1] / sum(x[1,]), 
                          x[2,2] / sum(x[2,]),
                          x[3,3] / sum(x[3,])
    ))
    svm.FP$k3 <- mean(c((sum(x[1,]) - x[1,1]) / (sum(x) - sum(x[,1])), 
                        (sum(x[2,]) - x[2,2]) / (sum(x) - sum(x[,2])),
                        (sum(x[3,]) - x[3,3]) / (sum(x) - sum(x[,3]))
    ))
    x <- table(k4.svm.pred,k4.test); svm.acc$k4 <-  1-((sum(x) - sum(diag(x))) / sum(x))
    svm.rec$k4 <- mean(c(x[1,1] / sum(x[,1]), 
                         x[2,2] / sum(x[,2]),
                         x[3,3] / sum(x[,3]),
                         x[4,4] / sum(x[,4])
    ))
    svm.prec$k4 <- mean(c(x[1,1] / sum(x[1,]), 
                          x[2,2] / sum(x[2,]),
                          x[3,3] / sum(x[3,]),
                          x[4,4] / sum(x[4,])
    ))
    svm.FP$k4 <- mean(c((sum(x[1,]) - x[1,1]) / (sum(x) - sum(x[,1])), 
                        (sum(x[2,]) - x[2,2]) / (sum(x) - sum(x[,2])),
                        (sum(x[3,]) - x[3,3]) / (sum(x) - sum(x[,3])),
                        (sum(x[4,]) - x[4,4]) / (sum(x) - sum(x[,4]))
    ))
    x <- table(k5.svm.pred,k5.test); svm.acc$k5 <-  1-((sum(x) - sum(diag(x))) / sum(x))
    svm.rec$k5 <- mean(c(x[1,1] / sum(x[,1]), 
                         x[2,2] / sum(x[,2]),
                         x[3,3] / sum(x[,3]),
                         x[4,4] / sum(x[,4]),
                         x[5,5] / sum(x[,5])
    ))
    svm.prec$k5 <- mean(c(x[1,1] / sum(x[1,]), 
                          x[2,2] / sum(x[2,]),
                          x[3,3] / sum(x[3,]),
                          x[4,4] / sum(x[4,]),
                          x[5,5] / sum(x[5,])
    ))
    svm.FP$k5 <- mean(c((sum(x[1,]) - x[1,1]) / (sum(x) - sum(x[,1])), 
                        (sum(x[2,]) - x[2,2]) / (sum(x) - sum(x[,2])),
                        (sum(x[3,]) - x[3,3]) / (sum(x) - sum(x[,3])),
                        (sum(x[4,]) - x[4,4]) / (sum(x) - sum(x[,4])),
                        (sum(x[5,]) - x[5,5]) / (sum(x) - sum(x[,5]))
    ))
    x <- table(k6.svm.pred,k6.test); svm.acc$k6 <-  1-((sum(x) - sum(diag(x))) / sum(x))
    svm.rec$k6 <- mean(c(x[1,1] / sum(x[,1]), 
                         x[2,2] / sum(x[,2]),
                         x[3,3] / sum(x[,3]),
                         x[4,4] / sum(x[,4]),
                         x[5,5] / sum(x[,5]),
                         x[6,6] / sum(x[,6])
    ))
    svm.prec$k6 <- mean(c(x[1,1] / sum(x[1,]), 
                          x[2,2] / sum(x[2,]),
                          x[3,3] / sum(x[3,]),
                          x[4,4] / sum(x[4,]),
                          x[5,5] / sum(x[5,]),
                          x[6,6] / sum(x[6,])
    ))
    svm.FP$k6 <- mean(c((sum(x[1,]) - x[1,1]) / (sum(x) - sum(x[,1])), 
                        (sum(x[2,]) - x[2,2]) / (sum(x) - sum(x[,2])),
                        (sum(x[3,]) - x[3,3]) / (sum(x) - sum(x[,3])),
                        (sum(x[4,]) - x[4,4]) / (sum(x) - sum(x[,4])),
                        (sum(x[5,]) - x[5,5]) / (sum(x) - sum(x[,5])),
                        (sum(x[6,]) - x[6,6]) / (sum(x) - sum(x[,6]))
    ))
    x <- table(k7.svm.pred,k7.test); svm.acc$k7 <-  1-((sum(x) - sum(diag(x))) / sum(x))
    svm.rec$k7 <- mean(c(x[1,1] / sum(x[,1]), 
                         x[2,2] / sum(x[,2]),
                         x[3,3] / sum(x[,3]),
                         x[4,4] / sum(x[,4]),
                         x[5,5] / sum(x[,5]),
                         x[6,6] / sum(x[,6]),
                         x[7,7] / sum(x[,7])
    ))
    svm.prec$k7 <- mean(c(x[1,1] / sum(x[1,]), 
                          x[2,2] / sum(x[2,]),
                          x[3,3] / sum(x[3,]),
                          x[4,4] / sum(x[4,]),
                          x[5,5] / sum(x[5,]),
                          x[6,6] / sum(x[6,]),
                          x[7,7] / sum(x[7,])
    ))
    svm.FP$k7 <- mean(c((sum(x[1,]) - x[1,1]) / (sum(x) - sum(x[,1])), 
                        (sum(x[2,]) - x[2,2]) / (sum(x) - sum(x[,2])),
                        (sum(x[3,]) - x[3,3]) / (sum(x) - sum(x[,3])),
                        (sum(x[4,]) - x[4,4]) / (sum(x) - sum(x[,4])),
                        (sum(x[5,]) - x[5,5]) / (sum(x) - sum(x[,5])),
                        (sum(x[6,]) - x[6,6]) / (sum(x) - sum(x[,6])),
                        (sum(x[7,]) - x[7,7]) / (sum(x) - sum(x[,7]))
    ))
    x <- table(k8.svm.pred,k8.test); svm.acc$k8 <-  1-((sum(x) - sum(diag(x))) / sum(x))
    svm.rec$k8 <- mean(c(x[1,1] / sum(x[,1]), 
                         x[2,2] / sum(x[,2]),
                         x[3,3] / sum(x[,3]),
                         x[4,4] / sum(x[,4]),
                         x[5,5] / sum(x[,5]),
                         x[6,6] / sum(x[,6]),
                         x[7,7] / sum(x[,7]),
                         x[8,8] / sum(x[,8])
    ))
    svm.prec$k8 <- mean(c(x[1,1] / sum(x[1,]), 
                          x[2,2] / sum(x[2,]),
                          x[3,3] / sum(x[3,]),
                          x[4,4] / sum(x[4,]),
                          x[5,5] / sum(x[5,]),
                          x[6,6] / sum(x[6,]),
                          x[7,7] / sum(x[7,]),
                          x[8,8] / sum(x[8,])
    ))
    svm.FP$k8 <- mean(c((sum(x[1,]) - x[1,1]) / (sum(x) - sum(x[,1])), 
                        (sum(x[2,]) - x[2,2]) / (sum(x) - sum(x[,2])),
                        (sum(x[3,]) - x[3,3]) / (sum(x) - sum(x[,3])),
                        (sum(x[4,]) - x[4,4]) / (sum(x) - sum(x[,4])),
                        (sum(x[5,]) - x[5,5]) / (sum(x) - sum(x[,5])),
                        (sum(x[6,]) - x[6,6]) / (sum(x) - sum(x[,6])),
                        (sum(x[7,]) - x[7,7]) / (sum(x) - sum(x[,7])),
                        (sum(x[8,]) - x[8,8]) / (sum(x) - sum(x[,8]))
    ))
    x <- table(k9.svm.pred,k9.test); svm.acc$k9 <-  1-((sum(x) - sum(diag(x))) / sum(x))
    svm.rec$k9 <- mean(c(x[1,1] / sum(x[,1]), 
                         x[2,2] / sum(x[,2]),
                         x[3,3] / sum(x[,3]),
                         x[4,4] / sum(x[,4]),
                         x[5,5] / sum(x[,5]),
                         x[6,6] / sum(x[,6]),
                         x[7,7] / sum(x[,7]),
                         x[8,8] / sum(x[,8]),
                         x[9,9] / sum(x[,9])
    ))
    svm.prec$k9 <- mean(c(x[1,1] / sum(x[1,]), 
                          x[2,2] / sum(x[2,]),
                          x[3,3] / sum(x[3,]),
                          x[4,4] / sum(x[4,]),
                          x[5,5] / sum(x[5,]),
                          x[6,6] / sum(x[6,]),
                          x[7,7] / sum(x[7,]),
                          x[8,8] / sum(x[8,]),
                          x[9,9] / sum(x[9,])
    ))
    svm.FP$k9 <- mean(c((sum(x[1,]) - x[1,1]) / (sum(x) - sum(x[,1])), 
                        (sum(x[2,]) - x[2,2]) / (sum(x) - sum(x[,2])),
                        (sum(x[3,]) - x[3,3]) / (sum(x) - sum(x[,3])),
                        (sum(x[4,]) - x[4,4]) / (sum(x) - sum(x[,4])),
                        (sum(x[5,]) - x[5,5]) / (sum(x) - sum(x[,5])),
                        (sum(x[6,]) - x[6,6]) / (sum(x) - sum(x[,6])),
                        (sum(x[7,]) - x[7,7]) / (sum(x) - sum(x[,7])),
                        (sum(x[8,]) - x[8,8]) / (sum(x) - sum(x[,8])),
                        (sum(x[9,]) - x[9,9]) / (sum(x) - sum(x[,9]))
    ))
    x <- table(k10.svm.pred,k10.test); svm.acc$k10 <-  1-((sum(x) - sum(diag(x))) / sum(x))
    svm.rec$k10 <- mean(c(x[1,1] / sum(x[,1]), 
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
    svm.prec$k10 <- mean(c(x[1,1] / sum(x[1,]), 
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
    svm.FP$k10 <- mean(c((sum(x[1,]) - x[1,1]) / (sum(x) - sum(x[,1])), 
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
    
    
    metrix_15[[i]] =matrix(t(rbind(round(svm.acc,digits=2),
                                   round(lda.acc,digits=2),
                                   round(svm.prec,digits=2),
                                   round(lda.prec,digits=2),
                                   round(svm.rec,digits=2),
                                   round(lda.rec,digits=2),
                                   round(svm.FP,digits=2),
                                   round(lda.FP,digits=2))),nrow=9,ncol=8)
    print(paste0("Finish CV 15 Variables; ",size[j],"; ",i))
    
    ##########################################################################################
    ################################## (5) Test Classification 5 Var ########################
    ##########################################################################################
    print(paste0("Start CV 5 Variables; ",size[j],"; ",i))
    lda.acc <- data.frame(k2=NA,k3=NA,k4=NA,k5=NA,k6=NA,k7=NA,k8=NA,k9=NA,k10=NA)
    svm.acc <- data.frame(k2=NA,k3=NA,k4=NA,k5=NA,k6=NA,k7=NA,k8=NA,k9=NA,k10=NA)
    lda.rec <- data.frame(k2=NA,k3=NA,k4=NA,k5=NA,k6=NA,k7=NA,k8=NA,k9=NA,k10=NA)
    lda.prec <- data.frame(k2=NA,k3=NA,k4=NA,k5=NA,k6=NA,k7=NA,k8=NA,k9=NA,k10=NA)
    lda.FP <- data.frame(k2=NA,k3=NA,k4=NA,k5=NA,k6=NA,k7=NA,k8=NA,k9=NA,k10=NA)
    svm.rec <- data.frame(k2=NA,k3=NA,k4=NA,k5=NA,k6=NA,k7=NA,k8=NA,k9=NA,k10=NA)
    svm.prec <- data.frame(k2=NA,k3=NA,k4=NA,k5=NA,k6=NA,k7=NA,k8=NA,k9=NA,k10=NA)
    svm.FP <- data.frame(k2=NA,k3=NA,k4=NA,k5=NA,k6=NA,k7=NA,k8=NA,k9=NA,k10=NA)
    
    #c(23:37)
    #c(23,24,30,32,35)
    #c(23,30)
    
    train = train.data[,c(23,24,30,32,35)]; test = test.data[,c(23,24,30,32,35)]
    
    k2.train <- factor(train.data$k2); k2.test <- factor(test.data$k2)
    k3.train <- factor(train.data$k3); k3.test <- factor(test.data$k3)
    k4.train <- factor(train.data$k4); k4.test <- factor(test.data$k4)
    k5.train <- factor(train.data$k5); k5.test <- factor(test.data$k5)
    k6.train <- factor(train.data$k6); k6.test <- factor(test.data$k6)
    k7.train <- factor(train.data$k7); k7.test <- factor(test.data$k7)
    k8.train <- factor(train.data$k8); k8.test <- factor(test.data$k8)
    k9.train <- factor(train.data$k9); k9.test <- factor(test.data$k9)
    k10.train <- factor(train.data$k10); k10.test <- factor(test.data$k10)
    
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
    x <- table(k2.lda.pred,k2.test); lda.acc$k2 <- 1-((sum(x) - sum(diag(x))) / sum(x))
    lda.rec$k2 <- mean(c(x[1,1] / sum(x[,1]), 
                         x[2,2] / sum(x[,2])
    ))
    lda.prec$k2 <- mean(c(x[1,1] / sum(x[1,]), 
                          x[2,2] / sum(x[2,])
    ))
    lda.FP$k2 <- mean(c((sum(x[1,]) - x[1,1]) / (sum(x) - sum(x[,1])), 
                        (sum(x[2,]) - x[2,2]) / (sum(x) - sum(x[,2]))
    ))
    x <- table(k3.lda.pred,k3.test); lda.acc$k3 <- 1-((sum(x) - sum(diag(x))) / sum(x))
    lda.rec$k3 <- mean(c(x[1,1] / sum(x[,1]), 
                         x[2,2] / sum(x[,2]),
                         x[3,3] / sum(x[,3])
    ))
    lda.prec$k3 <- mean(c(x[1,1] / sum(x[1,]), 
                          x[2,2] / sum(x[2,]),
                          x[3,3] / sum(x[3,])
    ))
    lda.FP$k3 <- mean(c((sum(x[1,]) - x[1,1]) / (sum(x) - sum(x[,1])), 
                        (sum(x[2,]) - x[2,2]) / (sum(x) - sum(x[,2])),
                        (sum(x[3,]) - x[3,3]) / (sum(x) - sum(x[,3]))
    ))
    x <- table(k4.lda.pred,k4.test); lda.acc$k4 <-  1-((sum(x) - sum(diag(x))) / sum(x))
    lda.rec$k4 <- mean(c(x[1,1] / sum(x[,1]), 
                         x[2,2] / sum(x[,2]),
                         x[3,3] / sum(x[,3]),
                         x[4,4] / sum(x[,4])
    ))
    lda.prec$k4 <- mean(c(x[1,1] / sum(x[1,]), 
                          x[2,2] / sum(x[2,]),
                          x[3,3] / sum(x[3,]),
                          x[4,4] / sum(x[4,])
    ))
    lda.FP$k4 <- mean(c((sum(x[1,]) - x[1,1]) / (sum(x) - sum(x[,1])), 
                        (sum(x[2,]) - x[2,2]) / (sum(x) - sum(x[,2])),
                        (sum(x[3,]) - x[3,3]) / (sum(x) - sum(x[,3])),
                        (sum(x[4,]) - x[4,4]) / (sum(x) - sum(x[,4]))
    ))
    x <- table(k5.lda.pred,k5.test); lda.acc$k5 <-  1-((sum(x) - sum(diag(x))) / sum(x))
    lda.rec$k5 <- mean(c(x[1,1] / sum(x[,1]), 
                         x[2,2] / sum(x[,2]),
                         x[3,3] / sum(x[,3]),
                         x[4,4] / sum(x[,4]),
                         x[5,5] / sum(x[,5])
    ))
    lda.prec$k5 <- mean(c(x[1,1] / sum(x[1,]), 
                          x[2,2] / sum(x[2,]),
                          x[3,3] / sum(x[3,]),
                          x[4,4] / sum(x[4,]),
                          x[5,5] / sum(x[5,])
    ))
    lda.FP$k5 <- mean(c((sum(x[1,]) - x[1,1]) / (sum(x) - sum(x[,1])), 
                        (sum(x[2,]) - x[2,2]) / (sum(x) - sum(x[,2])),
                        (sum(x[3,]) - x[3,3]) / (sum(x) - sum(x[,3])),
                        (sum(x[4,]) - x[4,4]) / (sum(x) - sum(x[,4])),
                        (sum(x[5,]) - x[5,5]) / (sum(x) - sum(x[,5]))
    ))
    x <- table(k6.lda.pred,k6.test); lda.acc$k6 <-  1-((sum(x) - sum(diag(x))) / sum(x))
    lda.rec$k6 <- mean(c(x[1,1] / sum(x[,1]), 
                         x[2,2] / sum(x[,2]),
                         x[3,3] / sum(x[,3]),
                         x[4,4] / sum(x[,4]),
                         x[5,5] / sum(x[,5]),
                         x[6,6] / sum(x[,6])
    ))
    lda.prec$k6 <- mean(c(x[1,1] / sum(x[1,]), 
                          x[2,2] / sum(x[2,]),
                          x[3,3] / sum(x[3,]),
                          x[4,4] / sum(x[4,]),
                          x[5,5] / sum(x[5,]),
                          x[6,6] / sum(x[6,])
    ))
    lda.FP$k6 <- mean(c((sum(x[1,]) - x[1,1]) / (sum(x) - sum(x[,1])), 
                        (sum(x[2,]) - x[2,2]) / (sum(x) - sum(x[,2])),
                        (sum(x[3,]) - x[3,3]) / (sum(x) - sum(x[,3])),
                        (sum(x[4,]) - x[4,4]) / (sum(x) - sum(x[,4])),
                        (sum(x[5,]) - x[5,5]) / (sum(x) - sum(x[,5])),
                        (sum(x[6,]) - x[6,6]) / (sum(x) - sum(x[,6]))
    ))
    x <- table(k7.lda.pred,k7.test); lda.acc$k7 <-  1-((sum(x) - sum(diag(x))) / sum(x))
    lda.rec$k7 <- mean(c(x[1,1] / sum(x[,1]), 
                         x[2,2] / sum(x[,2]),
                         x[3,3] / sum(x[,3]),
                         x[4,4] / sum(x[,4]),
                         x[5,5] / sum(x[,5]),
                         x[6,6] / sum(x[,6]),
                         x[7,7] / sum(x[,7])
    ))
    lda.prec$k7 <- mean(c(x[1,1] / sum(x[1,]), 
                          x[2,2] / sum(x[2,]),
                          x[3,3] / sum(x[3,]),
                          x[4,4] / sum(x[4,]),
                          x[5,5] / sum(x[5,]),
                          x[6,6] / sum(x[6,]),
                          x[7,7] / sum(x[7,])
    ))
    lda.FP$k7 <- mean(c((sum(x[1,]) - x[1,1]) / (sum(x) - sum(x[,1])), 
                        (sum(x[2,]) - x[2,2]) / (sum(x) - sum(x[,2])),
                        (sum(x[3,]) - x[3,3]) / (sum(x) - sum(x[,3])),
                        (sum(x[4,]) - x[4,4]) / (sum(x) - sum(x[,4])),
                        (sum(x[5,]) - x[5,5]) / (sum(x) - sum(x[,5])),
                        (sum(x[6,]) - x[6,6]) / (sum(x) - sum(x[,6])),
                        (sum(x[7,]) - x[7,7]) / (sum(x) - sum(x[,7]))
    ))
    x <- table(k8.lda.pred,k8.test); lda.acc$k8 <-  1-((sum(x) - sum(diag(x))) / sum(x))
    lda.rec$k8 <- mean(c(x[1,1] / sum(x[,1]), 
                         x[2,2] / sum(x[,2]),
                         x[3,3] / sum(x[,3]),
                         x[4,4] / sum(x[,4]),
                         x[5,5] / sum(x[,5]),
                         x[6,6] / sum(x[,6]),
                         x[7,7] / sum(x[,7]),
                         x[8,8] / sum(x[,8])
    ))
    lda.prec$k8 <- mean(c(x[1,1] / sum(x[1,]), 
                          x[2,2] / sum(x[2,]),
                          x[3,3] / sum(x[3,]),
                          x[4,4] / sum(x[4,]),
                          x[5,5] / sum(x[5,]),
                          x[6,6] / sum(x[6,]),
                          x[7,7] / sum(x[7,]),
                          x[8,8] / sum(x[8,])
    ))
    lda.FP$k8 <- mean(c((sum(x[1,]) - x[1,1]) / (sum(x) - sum(x[,1])), 
                        (sum(x[2,]) - x[2,2]) / (sum(x) - sum(x[,2])),
                        (sum(x[3,]) - x[3,3]) / (sum(x) - sum(x[,3])),
                        (sum(x[4,]) - x[4,4]) / (sum(x) - sum(x[,4])),
                        (sum(x[5,]) - x[5,5]) / (sum(x) - sum(x[,5])),
                        (sum(x[6,]) - x[6,6]) / (sum(x) - sum(x[,6])),
                        (sum(x[7,]) - x[7,7]) / (sum(x) - sum(x[,7])),
                        (sum(x[8,]) - x[8,8]) / (sum(x) - sum(x[,8]))
    ))
    x <- table(k9.lda.pred,k9.test); lda.acc$k9 <-  1-((sum(x) - sum(diag(x))) / sum(x))
    lda.rec$k9 <- mean(c(x[1,1] / sum(x[,1]), 
                         x[2,2] / sum(x[,2]),
                         x[3,3] / sum(x[,3]),
                         x[4,4] / sum(x[,4]),
                         x[5,5] / sum(x[,5]),
                         x[6,6] / sum(x[,6]),
                         x[7,7] / sum(x[,7]),
                         x[8,8] / sum(x[,8]),
                         x[9,9] / sum(x[,9])
    ))
    lda.prec$k9 <- mean(c(x[1,1] / sum(x[1,]), 
                          x[2,2] / sum(x[2,]),
                          x[3,3] / sum(x[3,]),
                          x[4,4] / sum(x[4,]),
                          x[5,5] / sum(x[5,]),
                          x[6,6] / sum(x[6,]),
                          x[7,7] / sum(x[7,]),
                          x[8,8] / sum(x[8,]),
                          x[9,9] / sum(x[9,])
    ))
    lda.FP$k9 <- mean(c((sum(x[1,]) - x[1,1]) / (sum(x) - sum(x[,1])), 
                        (sum(x[2,]) - x[2,2]) / (sum(x) - sum(x[,2])),
                        (sum(x[3,]) - x[3,3]) / (sum(x) - sum(x[,3])),
                        (sum(x[4,]) - x[4,4]) / (sum(x) - sum(x[,4])),
                        (sum(x[5,]) - x[5,5]) / (sum(x) - sum(x[,5])),
                        (sum(x[6,]) - x[6,6]) / (sum(x) - sum(x[,6])),
                        (sum(x[7,]) - x[7,7]) / (sum(x) - sum(x[,7])),
                        (sum(x[8,]) - x[8,8]) / (sum(x) - sum(x[,8])),
                        (sum(x[9,]) - x[9,9]) / (sum(x) - sum(x[,9]))
    ))
    x <- table(k10.lda.pred,k10.test); lda.acc$k10 <-  1-((sum(x) - sum(diag(x))) / sum(x))
    lda.rec$k10 <- mean(c(x[1,1] / sum(x[,1]), 
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
    lda.prec$k10 <- mean(c(x[1,1] / sum(x[1,]), 
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
    lda.FP$k10 <- mean(c((sum(x[1,]) - x[1,1]) / (sum(x) - sum(x[,1])), 
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
    
    ###### SUPPORT VECTORS
    x <- table(k2.svm.pred,k2.test); svm.acc$k2 <- 1-((sum(x) - sum(diag(x))) / sum(x))
    svm.rec$k2 <- mean(c(x[1,1] / sum(x[,1]), 
                         x[2,2] / sum(x[,2])
    ))
    svm.prec$k2 <- mean(c(x[1,1] / sum(x[1,]), 
                          x[2,2] / sum(x[2,])
    ))
    svm.FP$k2 <- mean(c((sum(x[1,]) - x[1,1]) / (sum(x) - sum(x[,1])), 
                        (sum(x[2,]) - x[2,2]) / (sum(x) - sum(x[,2]))
    ))
    x <- table(k3.svm.pred,k3.test); svm.acc$k3 <- 1-((sum(x) - sum(diag(x))) / sum(x))
    svm.rec$k3 <- mean(c(x[1,1] / sum(x[,1]), 
                         x[2,2] / sum(x[,2]),
                         x[3,3] / sum(x[,3])
    ))
    svm.prec$k3 <- mean(c(x[1,1] / sum(x[1,]), 
                          x[2,2] / sum(x[2,]),
                          x[3,3] / sum(x[3,])
    ))
    svm.FP$k3 <- mean(c((sum(x[1,]) - x[1,1]) / (sum(x) - sum(x[,1])), 
                        (sum(x[2,]) - x[2,2]) / (sum(x) - sum(x[,2])),
                        (sum(x[3,]) - x[3,3]) / (sum(x) - sum(x[,3]))
    ))
    x <- table(k4.svm.pred,k4.test); svm.acc$k4 <-  1-((sum(x) - sum(diag(x))) / sum(x))
    svm.rec$k4 <- mean(c(x[1,1] / sum(x[,1]), 
                         x[2,2] / sum(x[,2]),
                         x[3,3] / sum(x[,3]),
                         x[4,4] / sum(x[,4])
    ))
    svm.prec$k4 <- mean(c(x[1,1] / sum(x[1,]), 
                          x[2,2] / sum(x[2,]),
                          x[3,3] / sum(x[3,]),
                          x[4,4] / sum(x[4,])
    ))
    svm.FP$k4 <- mean(c((sum(x[1,]) - x[1,1]) / (sum(x) - sum(x[,1])), 
                        (sum(x[2,]) - x[2,2]) / (sum(x) - sum(x[,2])),
                        (sum(x[3,]) - x[3,3]) / (sum(x) - sum(x[,3])),
                        (sum(x[4,]) - x[4,4]) / (sum(x) - sum(x[,4]))
    ))
    x <- table(k5.svm.pred,k5.test); svm.acc$k5 <-  1-((sum(x) - sum(diag(x))) / sum(x))
    svm.rec$k5 <- mean(c(x[1,1] / sum(x[,1]), 
                         x[2,2] / sum(x[,2]),
                         x[3,3] / sum(x[,3]),
                         x[4,4] / sum(x[,4]),
                         x[5,5] / sum(x[,5])
    ))
    svm.prec$k5 <- mean(c(x[1,1] / sum(x[1,]), 
                          x[2,2] / sum(x[2,]),
                          x[3,3] / sum(x[3,]),
                          x[4,4] / sum(x[4,]),
                          x[5,5] / sum(x[5,])
    ))
    svm.FP$k5 <- mean(c((sum(x[1,]) - x[1,1]) / (sum(x) - sum(x[,1])), 
                        (sum(x[2,]) - x[2,2]) / (sum(x) - sum(x[,2])),
                        (sum(x[3,]) - x[3,3]) / (sum(x) - sum(x[,3])),
                        (sum(x[4,]) - x[4,4]) / (sum(x) - sum(x[,4])),
                        (sum(x[5,]) - x[5,5]) / (sum(x) - sum(x[,5]))
    ))
    x <- table(k6.svm.pred,k6.test); svm.acc$k6 <-  1-((sum(x) - sum(diag(x))) / sum(x))
    svm.rec$k6 <- mean(c(x[1,1] / sum(x[,1]), 
                         x[2,2] / sum(x[,2]),
                         x[3,3] / sum(x[,3]),
                         x[4,4] / sum(x[,4]),
                         x[5,5] / sum(x[,5]),
                         x[6,6] / sum(x[,6])
    ))
    svm.prec$k6 <- mean(c(x[1,1] / sum(x[1,]), 
                          x[2,2] / sum(x[2,]),
                          x[3,3] / sum(x[3,]),
                          x[4,4] / sum(x[4,]),
                          x[5,5] / sum(x[5,]),
                          x[6,6] / sum(x[6,])
    ))
    svm.FP$k6 <- mean(c((sum(x[1,]) - x[1,1]) / (sum(x) - sum(x[,1])), 
                        (sum(x[2,]) - x[2,2]) / (sum(x) - sum(x[,2])),
                        (sum(x[3,]) - x[3,3]) / (sum(x) - sum(x[,3])),
                        (sum(x[4,]) - x[4,4]) / (sum(x) - sum(x[,4])),
                        (sum(x[5,]) - x[5,5]) / (sum(x) - sum(x[,5])),
                        (sum(x[6,]) - x[6,6]) / (sum(x) - sum(x[,6]))
    ))
    x <- table(k7.svm.pred,k7.test); svm.acc$k7 <-  1-((sum(x) - sum(diag(x))) / sum(x))
    svm.rec$k7 <- mean(c(x[1,1] / sum(x[,1]), 
                         x[2,2] / sum(x[,2]),
                         x[3,3] / sum(x[,3]),
                         x[4,4] / sum(x[,4]),
                         x[5,5] / sum(x[,5]),
                         x[6,6] / sum(x[,6]),
                         x[7,7] / sum(x[,7])
    ))
    svm.prec$k7 <- mean(c(x[1,1] / sum(x[1,]), 
                          x[2,2] / sum(x[2,]),
                          x[3,3] / sum(x[3,]),
                          x[4,4] / sum(x[4,]),
                          x[5,5] / sum(x[5,]),
                          x[6,6] / sum(x[6,]),
                          x[7,7] / sum(x[7,])
    ))
    svm.FP$k7 <- mean(c((sum(x[1,]) - x[1,1]) / (sum(x) - sum(x[,1])), 
                        (sum(x[2,]) - x[2,2]) / (sum(x) - sum(x[,2])),
                        (sum(x[3,]) - x[3,3]) / (sum(x) - sum(x[,3])),
                        (sum(x[4,]) - x[4,4]) / (sum(x) - sum(x[,4])),
                        (sum(x[5,]) - x[5,5]) / (sum(x) - sum(x[,5])),
                        (sum(x[6,]) - x[6,6]) / (sum(x) - sum(x[,6])),
                        (sum(x[7,]) - x[7,7]) / (sum(x) - sum(x[,7]))
    ))
    x <- table(k8.svm.pred,k8.test); svm.acc$k8 <-  1-((sum(x) - sum(diag(x))) / sum(x))
    svm.rec$k8 <- mean(c(x[1,1] / sum(x[,1]), 
                         x[2,2] / sum(x[,2]),
                         x[3,3] / sum(x[,3]),
                         x[4,4] / sum(x[,4]),
                         x[5,5] / sum(x[,5]),
                         x[6,6] / sum(x[,6]),
                         x[7,7] / sum(x[,7]),
                         x[8,8] / sum(x[,8])
    ))
    svm.prec$k8 <- mean(c(x[1,1] / sum(x[1,]), 
                          x[2,2] / sum(x[2,]),
                          x[3,3] / sum(x[3,]),
                          x[4,4] / sum(x[4,]),
                          x[5,5] / sum(x[5,]),
                          x[6,6] / sum(x[6,]),
                          x[7,7] / sum(x[7,]),
                          x[8,8] / sum(x[8,])
    ))
    svm.FP$k8 <- mean(c((sum(x[1,]) - x[1,1]) / (sum(x) - sum(x[,1])), 
                        (sum(x[2,]) - x[2,2]) / (sum(x) - sum(x[,2])),
                        (sum(x[3,]) - x[3,3]) / (sum(x) - sum(x[,3])),
                        (sum(x[4,]) - x[4,4]) / (sum(x) - sum(x[,4])),
                        (sum(x[5,]) - x[5,5]) / (sum(x) - sum(x[,5])),
                        (sum(x[6,]) - x[6,6]) / (sum(x) - sum(x[,6])),
                        (sum(x[7,]) - x[7,7]) / (sum(x) - sum(x[,7])),
                        (sum(x[8,]) - x[8,8]) / (sum(x) - sum(x[,8]))
    ))
    x <- table(k9.svm.pred,k9.test); svm.acc$k9 <-  1-((sum(x) - sum(diag(x))) / sum(x))
    svm.rec$k9 <- mean(c(x[1,1] / sum(x[,1]), 
                         x[2,2] / sum(x[,2]),
                         x[3,3] / sum(x[,3]),
                         x[4,4] / sum(x[,4]),
                         x[5,5] / sum(x[,5]),
                         x[6,6] / sum(x[,6]),
                         x[7,7] / sum(x[,7]),
                         x[8,8] / sum(x[,8]),
                         x[9,9] / sum(x[,9])
    ))
    svm.prec$k9 <- mean(c(x[1,1] / sum(x[1,]), 
                          x[2,2] / sum(x[2,]),
                          x[3,3] / sum(x[3,]),
                          x[4,4] / sum(x[4,]),
                          x[5,5] / sum(x[5,]),
                          x[6,6] / sum(x[6,]),
                          x[7,7] / sum(x[7,]),
                          x[8,8] / sum(x[8,]),
                          x[9,9] / sum(x[9,])
    ))
    svm.FP$k9 <- mean(c((sum(x[1,]) - x[1,1]) / (sum(x) - sum(x[,1])), 
                        (sum(x[2,]) - x[2,2]) / (sum(x) - sum(x[,2])),
                        (sum(x[3,]) - x[3,3]) / (sum(x) - sum(x[,3])),
                        (sum(x[4,]) - x[4,4]) / (sum(x) - sum(x[,4])),
                        (sum(x[5,]) - x[5,5]) / (sum(x) - sum(x[,5])),
                        (sum(x[6,]) - x[6,6]) / (sum(x) - sum(x[,6])),
                        (sum(x[7,]) - x[7,7]) / (sum(x) - sum(x[,7])),
                        (sum(x[8,]) - x[8,8]) / (sum(x) - sum(x[,8])),
                        (sum(x[9,]) - x[9,9]) / (sum(x) - sum(x[,9]))
    ))
    x <- table(k10.svm.pred,k10.test); svm.acc$k10 <-  1-((sum(x) - sum(diag(x))) / sum(x))
    svm.rec$k10 <- mean(c(x[1,1] / sum(x[,1]), 
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
    svm.prec$k10 <- mean(c(x[1,1] / sum(x[1,]), 
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
    svm.FP$k10 <- mean(c((sum(x[1,]) - x[1,1]) / (sum(x) - sum(x[,1])), 
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
    
    
    metrix_5[[i]] =matrix(t(rbind(round(svm.acc,digits=2),
                                  round(lda.acc,digits=2),
                                  round(svm.prec,digits=2),
                                  round(lda.prec,digits=2),
                                  round(svm.rec,digits=2),
                                  round(lda.rec,digits=2),
                                  round(svm.FP,digits=2),
                                  round(lda.FP,digits=2))),nrow=9,ncol=8)
    print(paste0("Finish CV 5 Variables; ",size[j],"; ",i))
    
    ##########################################################################################
    ################################## (5) Test Classification 2 Var ########################
    ##########################################################################################
    print(paste0("Start CV 2 Variables; ",size[j],"; ",i))
    lda.acc <- data.frame(k2=NA,k3=NA,k4=NA,k5=NA,k6=NA,k7=NA,k8=NA,k9=NA,k10=NA)
    svm.acc <- data.frame(k2=NA,k3=NA,k4=NA,k5=NA,k6=NA,k7=NA,k8=NA,k9=NA,k10=NA)
    lda.rec <- data.frame(k2=NA,k3=NA,k4=NA,k5=NA,k6=NA,k7=NA,k8=NA,k9=NA,k10=NA)
    lda.prec <- data.frame(k2=NA,k3=NA,k4=NA,k5=NA,k6=NA,k7=NA,k8=NA,k9=NA,k10=NA)
    lda.FP <- data.frame(k2=NA,k3=NA,k4=NA,k5=NA,k6=NA,k7=NA,k8=NA,k9=NA,k10=NA)
    svm.rec <- data.frame(k2=NA,k3=NA,k4=NA,k5=NA,k6=NA,k7=NA,k8=NA,k9=NA,k10=NA)
    svm.prec <- data.frame(k2=NA,k3=NA,k4=NA,k5=NA,k6=NA,k7=NA,k8=NA,k9=NA,k10=NA)
    svm.FP <- data.frame(k2=NA,k3=NA,k4=NA,k5=NA,k6=NA,k7=NA,k8=NA,k9=NA,k10=NA)
    
    #c(23:37)
    #c(23,24,30,32,35)
    #c(23,30)
    
    train = train.data[,c(23,30)]; test = test.data[,c(23,30)]
    
    k2.train <- factor(train.data$k2); k2.test <- factor(test.data$k2)
    k3.train <- factor(train.data$k3); k3.test <- factor(test.data$k3)
    k4.train <- factor(train.data$k4); k4.test <- factor(test.data$k4)
    k5.train <- factor(train.data$k5); k5.test <- factor(test.data$k5)
    k6.train <- factor(train.data$k6); k6.test <- factor(test.data$k6)
    k7.train <- factor(train.data$k7); k7.test <- factor(test.data$k7)
    k8.train <- factor(train.data$k8); k8.test <- factor(test.data$k8)
    k9.train <- factor(train.data$k9); k9.test <- factor(test.data$k9)
    k10.train <- factor(train.data$k10); k10.test <- factor(test.data$k10)
    
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
    x <- table(k2.lda.pred,k2.test); lda.acc$k2 <- 1-((sum(x) - sum(diag(x))) / sum(x))
    lda.rec$k2 <- mean(c(x[1,1] / sum(x[,1]), 
                         x[2,2] / sum(x[,2])
    ))
    lda.prec$k2 <- mean(c(x[1,1] / sum(x[1,]), 
                          x[2,2] / sum(x[2,])
    ))
    lda.FP$k2 <- mean(c((sum(x[1,]) - x[1,1]) / (sum(x) - sum(x[,1])), 
                        (sum(x[2,]) - x[2,2]) / (sum(x) - sum(x[,2]))
    ))
    x <- table(k3.lda.pred,k3.test); lda.acc$k3 <- 1-((sum(x) - sum(diag(x))) / sum(x))
    lda.rec$k3 <- mean(c(x[1,1] / sum(x[,1]), 
                         x[2,2] / sum(x[,2]),
                         x[3,3] / sum(x[,3])
    ))
    lda.prec$k3 <- mean(c(x[1,1] / sum(x[1,]), 
                          x[2,2] / sum(x[2,]),
                          x[3,3] / sum(x[3,])
    ))
    lda.FP$k3 <- mean(c((sum(x[1,]) - x[1,1]) / (sum(x) - sum(x[,1])), 
                        (sum(x[2,]) - x[2,2]) / (sum(x) - sum(x[,2])),
                        (sum(x[3,]) - x[3,3]) / (sum(x) - sum(x[,3]))
    ))
    x <- table(k4.lda.pred,k4.test); lda.acc$k4 <-  1-((sum(x) - sum(diag(x))) / sum(x))
    lda.rec$k4 <- mean(c(x[1,1] / sum(x[,1]), 
                         x[2,2] / sum(x[,2]),
                         x[3,3] / sum(x[,3]),
                         x[4,4] / sum(x[,4])
    ))
    lda.prec$k4 <- mean(c(x[1,1] / sum(x[1,]), 
                          x[2,2] / sum(x[2,]),
                          x[3,3] / sum(x[3,]),
                          x[4,4] / sum(x[4,])
    ))
    lda.FP$k4 <- mean(c((sum(x[1,]) - x[1,1]) / (sum(x) - sum(x[,1])), 
                        (sum(x[2,]) - x[2,2]) / (sum(x) - sum(x[,2])),
                        (sum(x[3,]) - x[3,3]) / (sum(x) - sum(x[,3])),
                        (sum(x[4,]) - x[4,4]) / (sum(x) - sum(x[,4]))
    ))
    x <- table(k5.lda.pred,k5.test); lda.acc$k5 <-  1-((sum(x) - sum(diag(x))) / sum(x))
    lda.rec$k5 <- mean(c(x[1,1] / sum(x[,1]), 
                         x[2,2] / sum(x[,2]),
                         x[3,3] / sum(x[,3]),
                         x[4,4] / sum(x[,4]),
                         x[5,5] / sum(x[,5])
    ))
    lda.prec$k5 <- mean(c(x[1,1] / sum(x[1,]), 
                          x[2,2] / sum(x[2,]),
                          x[3,3] / sum(x[3,]),
                          x[4,4] / sum(x[4,]),
                          x[5,5] / sum(x[5,])
    ))
    lda.FP$k5 <- mean(c((sum(x[1,]) - x[1,1]) / (sum(x) - sum(x[,1])), 
                        (sum(x[2,]) - x[2,2]) / (sum(x) - sum(x[,2])),
                        (sum(x[3,]) - x[3,3]) / (sum(x) - sum(x[,3])),
                        (sum(x[4,]) - x[4,4]) / (sum(x) - sum(x[,4])),
                        (sum(x[5,]) - x[5,5]) / (sum(x) - sum(x[,5]))
    ))
    x <- table(k6.lda.pred,k6.test); lda.acc$k6 <-  1-((sum(x) - sum(diag(x))) / sum(x))
    lda.rec$k6 <- mean(c(x[1,1] / sum(x[,1]), 
                         x[2,2] / sum(x[,2]),
                         x[3,3] / sum(x[,3]),
                         x[4,4] / sum(x[,4]),
                         x[5,5] / sum(x[,5]),
                         x[6,6] / sum(x[,6])
    ))
    lda.prec$k6 <- mean(c(x[1,1] / sum(x[1,]), 
                          x[2,2] / sum(x[2,]),
                          x[3,3] / sum(x[3,]),
                          x[4,4] / sum(x[4,]),
                          x[5,5] / sum(x[5,]),
                          x[6,6] / sum(x[6,])
    ))
    lda.FP$k6 <- mean(c((sum(x[1,]) - x[1,1]) / (sum(x) - sum(x[,1])), 
                        (sum(x[2,]) - x[2,2]) / (sum(x) - sum(x[,2])),
                        (sum(x[3,]) - x[3,3]) / (sum(x) - sum(x[,3])),
                        (sum(x[4,]) - x[4,4]) / (sum(x) - sum(x[,4])),
                        (sum(x[5,]) - x[5,5]) / (sum(x) - sum(x[,5])),
                        (sum(x[6,]) - x[6,6]) / (sum(x) - sum(x[,6]))
    ))
    x <- table(k7.lda.pred,k7.test); lda.acc$k7 <-  1-((sum(x) - sum(diag(x))) / sum(x))
    lda.rec$k7 <- mean(c(x[1,1] / sum(x[,1]), 
                         x[2,2] / sum(x[,2]),
                         x[3,3] / sum(x[,3]),
                         x[4,4] / sum(x[,4]),
                         x[5,5] / sum(x[,5]),
                         x[6,6] / sum(x[,6]),
                         x[7,7] / sum(x[,7])
    ))
    lda.prec$k7 <- mean(c(x[1,1] / sum(x[1,]), 
                          x[2,2] / sum(x[2,]),
                          x[3,3] / sum(x[3,]),
                          x[4,4] / sum(x[4,]),
                          x[5,5] / sum(x[5,]),
                          x[6,6] / sum(x[6,]),
                          x[7,7] / sum(x[7,])
    ))
    lda.FP$k7 <- mean(c((sum(x[1,]) - x[1,1]) / (sum(x) - sum(x[,1])), 
                        (sum(x[2,]) - x[2,2]) / (sum(x) - sum(x[,2])),
                        (sum(x[3,]) - x[3,3]) / (sum(x) - sum(x[,3])),
                        (sum(x[4,]) - x[4,4]) / (sum(x) - sum(x[,4])),
                        (sum(x[5,]) - x[5,5]) / (sum(x) - sum(x[,5])),
                        (sum(x[6,]) - x[6,6]) / (sum(x) - sum(x[,6])),
                        (sum(x[7,]) - x[7,7]) / (sum(x) - sum(x[,7]))
    ))
    x <- table(k8.lda.pred,k8.test); lda.acc$k8 <-  1-((sum(x) - sum(diag(x))) / sum(x))
    lda.rec$k8 <- mean(c(x[1,1] / sum(x[,1]), 
                         x[2,2] / sum(x[,2]),
                         x[3,3] / sum(x[,3]),
                         x[4,4] / sum(x[,4]),
                         x[5,5] / sum(x[,5]),
                         x[6,6] / sum(x[,6]),
                         x[7,7] / sum(x[,7]),
                         x[8,8] / sum(x[,8])
    ))
    lda.prec$k8 <- mean(c(x[1,1] / sum(x[1,]), 
                          x[2,2] / sum(x[2,]),
                          x[3,3] / sum(x[3,]),
                          x[4,4] / sum(x[4,]),
                          x[5,5] / sum(x[5,]),
                          x[6,6] / sum(x[6,]),
                          x[7,7] / sum(x[7,]),
                          x[8,8] / sum(x[8,])
    ))
    lda.FP$k8 <- mean(c((sum(x[1,]) - x[1,1]) / (sum(x) - sum(x[,1])), 
                        (sum(x[2,]) - x[2,2]) / (sum(x) - sum(x[,2])),
                        (sum(x[3,]) - x[3,3]) / (sum(x) - sum(x[,3])),
                        (sum(x[4,]) - x[4,4]) / (sum(x) - sum(x[,4])),
                        (sum(x[5,]) - x[5,5]) / (sum(x) - sum(x[,5])),
                        (sum(x[6,]) - x[6,6]) / (sum(x) - sum(x[,6])),
                        (sum(x[7,]) - x[7,7]) / (sum(x) - sum(x[,7])),
                        (sum(x[8,]) - x[8,8]) / (sum(x) - sum(x[,8]))
    ))
    x <- table(k9.lda.pred,k9.test); lda.acc$k9 <-  1-((sum(x) - sum(diag(x))) / sum(x))
    lda.rec$k9 <- mean(c(x[1,1] / sum(x[,1]), 
                         x[2,2] / sum(x[,2]),
                         x[3,3] / sum(x[,3]),
                         x[4,4] / sum(x[,4]),
                         x[5,5] / sum(x[,5]),
                         x[6,6] / sum(x[,6]),
                         x[7,7] / sum(x[,7]),
                         x[8,8] / sum(x[,8]),
                         x[9,9] / sum(x[,9])
    ))
    lda.prec$k9 <- mean(c(x[1,1] / sum(x[1,]), 
                          x[2,2] / sum(x[2,]),
                          x[3,3] / sum(x[3,]),
                          x[4,4] / sum(x[4,]),
                          x[5,5] / sum(x[5,]),
                          x[6,6] / sum(x[6,]),
                          x[7,7] / sum(x[7,]),
                          x[8,8] / sum(x[8,]),
                          x[9,9] / sum(x[9,])
    ))
    lda.FP$k9 <- mean(c((sum(x[1,]) - x[1,1]) / (sum(x) - sum(x[,1])), 
                        (sum(x[2,]) - x[2,2]) / (sum(x) - sum(x[,2])),
                        (sum(x[3,]) - x[3,3]) / (sum(x) - sum(x[,3])),
                        (sum(x[4,]) - x[4,4]) / (sum(x) - sum(x[,4])),
                        (sum(x[5,]) - x[5,5]) / (sum(x) - sum(x[,5])),
                        (sum(x[6,]) - x[6,6]) / (sum(x) - sum(x[,6])),
                        (sum(x[7,]) - x[7,7]) / (sum(x) - sum(x[,7])),
                        (sum(x[8,]) - x[8,8]) / (sum(x) - sum(x[,8])),
                        (sum(x[9,]) - x[9,9]) / (sum(x) - sum(x[,9]))
    ))
    x <- table(k10.lda.pred,k10.test); lda.acc$k10 <-  1-((sum(x) - sum(diag(x))) / sum(x))
    lda.rec$k10 <- mean(c(x[1,1] / sum(x[,1]), 
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
    lda.prec$k10 <- mean(c(x[1,1] / sum(x[1,]), 
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
    lda.FP$k10 <- mean(c((sum(x[1,]) - x[1,1]) / (sum(x) - sum(x[,1])), 
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
    
    ###### SUPPORT VECTORS
    x <- table(k2.svm.pred,k2.test); svm.acc$k2 <- 1-((sum(x) - sum(diag(x))) / sum(x))
    svm.rec$k2 <- mean(c(x[1,1] / sum(x[,1]), 
                         x[2,2] / sum(x[,2])
    ))
    svm.prec$k2 <- mean(c(x[1,1] / sum(x[1,]), 
                          x[2,2] / sum(x[2,])
    ))
    svm.FP$k2 <- mean(c((sum(x[1,]) - x[1,1]) / (sum(x) - sum(x[,1])), 
                        (sum(x[2,]) - x[2,2]) / (sum(x) - sum(x[,2]))
    ))
    x <- table(k3.svm.pred,k3.test); svm.acc$k3 <- 1-((sum(x) - sum(diag(x))) / sum(x))
    svm.rec$k3 <- mean(c(x[1,1] / sum(x[,1]), 
                         x[2,2] / sum(x[,2]),
                         x[3,3] / sum(x[,3])
    ))
    svm.prec$k3 <- mean(c(x[1,1] / sum(x[1,]), 
                          x[2,2] / sum(x[2,]),
                          x[3,3] / sum(x[3,])
    ))
    svm.FP$k3 <- mean(c((sum(x[1,]) - x[1,1]) / (sum(x) - sum(x[,1])), 
                        (sum(x[2,]) - x[2,2]) / (sum(x) - sum(x[,2])),
                        (sum(x[3,]) - x[3,3]) / (sum(x) - sum(x[,3]))
    ))
    x <- table(k4.svm.pred,k4.test); svm.acc$k4 <-  1-((sum(x) - sum(diag(x))) / sum(x))
    svm.rec$k4 <- mean(c(x[1,1] / sum(x[,1]), 
                         x[2,2] / sum(x[,2]),
                         x[3,3] / sum(x[,3]),
                         x[4,4] / sum(x[,4])
    ))
    svm.prec$k4 <- mean(c(x[1,1] / sum(x[1,]), 
                          x[2,2] / sum(x[2,]),
                          x[3,3] / sum(x[3,]),
                          x[4,4] / sum(x[4,])
    ))
    svm.FP$k4 <- mean(c((sum(x[1,]) - x[1,1]) / (sum(x) - sum(x[,1])), 
                        (sum(x[2,]) - x[2,2]) / (sum(x) - sum(x[,2])),
                        (sum(x[3,]) - x[3,3]) / (sum(x) - sum(x[,3])),
                        (sum(x[4,]) - x[4,4]) / (sum(x) - sum(x[,4]))
    ))
    x <- table(k5.svm.pred,k5.test); svm.acc$k5 <-  1-((sum(x) - sum(diag(x))) / sum(x))
    svm.rec$k5 <- mean(c(x[1,1] / sum(x[,1]), 
                         x[2,2] / sum(x[,2]),
                         x[3,3] / sum(x[,3]),
                         x[4,4] / sum(x[,4]),
                         x[5,5] / sum(x[,5])
    ))
    svm.prec$k5 <- mean(c(x[1,1] / sum(x[1,]), 
                          x[2,2] / sum(x[2,]),
                          x[3,3] / sum(x[3,]),
                          x[4,4] / sum(x[4,]),
                          x[5,5] / sum(x[5,])
    ))
    svm.FP$k5 <- mean(c((sum(x[1,]) - x[1,1]) / (sum(x) - sum(x[,1])), 
                        (sum(x[2,]) - x[2,2]) / (sum(x) - sum(x[,2])),
                        (sum(x[3,]) - x[3,3]) / (sum(x) - sum(x[,3])),
                        (sum(x[4,]) - x[4,4]) / (sum(x) - sum(x[,4])),
                        (sum(x[5,]) - x[5,5]) / (sum(x) - sum(x[,5]))
    ))
    x <- table(k6.svm.pred,k6.test); svm.acc$k6 <-  1-((sum(x) - sum(diag(x))) / sum(x))
    svm.rec$k6 <- mean(c(x[1,1] / sum(x[,1]), 
                         x[2,2] / sum(x[,2]),
                         x[3,3] / sum(x[,3]),
                         x[4,4] / sum(x[,4]),
                         x[5,5] / sum(x[,5]),
                         x[6,6] / sum(x[,6])
    ))
    svm.prec$k6 <- mean(c(x[1,1] / sum(x[1,]), 
                          x[2,2] / sum(x[2,]),
                          x[3,3] / sum(x[3,]),
                          x[4,4] / sum(x[4,]),
                          x[5,5] / sum(x[5,]),
                          x[6,6] / sum(x[6,])
    ))
    svm.FP$k6 <- mean(c((sum(x[1,]) - x[1,1]) / (sum(x) - sum(x[,1])), 
                        (sum(x[2,]) - x[2,2]) / (sum(x) - sum(x[,2])),
                        (sum(x[3,]) - x[3,3]) / (sum(x) - sum(x[,3])),
                        (sum(x[4,]) - x[4,4]) / (sum(x) - sum(x[,4])),
                        (sum(x[5,]) - x[5,5]) / (sum(x) - sum(x[,5])),
                        (sum(x[6,]) - x[6,6]) / (sum(x) - sum(x[,6]))
    ))
    x <- table(k7.svm.pred,k7.test); svm.acc$k7 <-  1-((sum(x) - sum(diag(x))) / sum(x))
    svm.rec$k7 <- mean(c(x[1,1] / sum(x[,1]), 
                         x[2,2] / sum(x[,2]),
                         x[3,3] / sum(x[,3]),
                         x[4,4] / sum(x[,4]),
                         x[5,5] / sum(x[,5]),
                         x[6,6] / sum(x[,6]),
                         x[7,7] / sum(x[,7])
    ))
    svm.prec$k7 <- mean(c(x[1,1] / sum(x[1,]), 
                          x[2,2] / sum(x[2,]),
                          x[3,3] / sum(x[3,]),
                          x[4,4] / sum(x[4,]),
                          x[5,5] / sum(x[5,]),
                          x[6,6] / sum(x[6,]),
                          x[7,7] / sum(x[7,])
    ))
    svm.FP$k7 <- mean(c((sum(x[1,]) - x[1,1]) / (sum(x) - sum(x[,1])), 
                        (sum(x[2,]) - x[2,2]) / (sum(x) - sum(x[,2])),
                        (sum(x[3,]) - x[3,3]) / (sum(x) - sum(x[,3])),
                        (sum(x[4,]) - x[4,4]) / (sum(x) - sum(x[,4])),
                        (sum(x[5,]) - x[5,5]) / (sum(x) - sum(x[,5])),
                        (sum(x[6,]) - x[6,6]) / (sum(x) - sum(x[,6])),
                        (sum(x[7,]) - x[7,7]) / (sum(x) - sum(x[,7]))
    ))
    x <- table(k8.svm.pred,k8.test); svm.acc$k8 <-  1-((sum(x) - sum(diag(x))) / sum(x))
    svm.rec$k8 <- mean(c(x[1,1] / sum(x[,1]), 
                         x[2,2] / sum(x[,2]),
                         x[3,3] / sum(x[,3]),
                         x[4,4] / sum(x[,4]),
                         x[5,5] / sum(x[,5]),
                         x[6,6] / sum(x[,6]),
                         x[7,7] / sum(x[,7]),
                         x[8,8] / sum(x[,8])
    ))
    svm.prec$k8 <- mean(c(x[1,1] / sum(x[1,]), 
                          x[2,2] / sum(x[2,]),
                          x[3,3] / sum(x[3,]),
                          x[4,4] / sum(x[4,]),
                          x[5,5] / sum(x[5,]),
                          x[6,6] / sum(x[6,]),
                          x[7,7] / sum(x[7,]),
                          x[8,8] / sum(x[8,])
    ))
    svm.FP$k8 <- mean(c((sum(x[1,]) - x[1,1]) / (sum(x) - sum(x[,1])), 
                        (sum(x[2,]) - x[2,2]) / (sum(x) - sum(x[,2])),
                        (sum(x[3,]) - x[3,3]) / (sum(x) - sum(x[,3])),
                        (sum(x[4,]) - x[4,4]) / (sum(x) - sum(x[,4])),
                        (sum(x[5,]) - x[5,5]) / (sum(x) - sum(x[,5])),
                        (sum(x[6,]) - x[6,6]) / (sum(x) - sum(x[,6])),
                        (sum(x[7,]) - x[7,7]) / (sum(x) - sum(x[,7])),
                        (sum(x[8,]) - x[8,8]) / (sum(x) - sum(x[,8]))
    ))
    x <- table(k9.svm.pred,k9.test); svm.acc$k9 <-  1-((sum(x) - sum(diag(x))) / sum(x))
    svm.rec$k9 <- mean(c(x[1,1] / sum(x[,1]), 
                         x[2,2] / sum(x[,2]),
                         x[3,3] / sum(x[,3]),
                         x[4,4] / sum(x[,4]),
                         x[5,5] / sum(x[,5]),
                         x[6,6] / sum(x[,6]),
                         x[7,7] / sum(x[,7]),
                         x[8,8] / sum(x[,8]),
                         x[9,9] / sum(x[,9])
    ))
    svm.prec$k9 <- mean(c(x[1,1] / sum(x[1,]), 
                          x[2,2] / sum(x[2,]),
                          x[3,3] / sum(x[3,]),
                          x[4,4] / sum(x[4,]),
                          x[5,5] / sum(x[5,]),
                          x[6,6] / sum(x[6,]),
                          x[7,7] / sum(x[7,]),
                          x[8,8] / sum(x[8,]),
                          x[9,9] / sum(x[9,])
    ))
    svm.FP$k9 <- mean(c((sum(x[1,]) - x[1,1]) / (sum(x) - sum(x[,1])), 
                        (sum(x[2,]) - x[2,2]) / (sum(x) - sum(x[,2])),
                        (sum(x[3,]) - x[3,3]) / (sum(x) - sum(x[,3])),
                        (sum(x[4,]) - x[4,4]) / (sum(x) - sum(x[,4])),
                        (sum(x[5,]) - x[5,5]) / (sum(x) - sum(x[,5])),
                        (sum(x[6,]) - x[6,6]) / (sum(x) - sum(x[,6])),
                        (sum(x[7,]) - x[7,7]) / (sum(x) - sum(x[,7])),
                        (sum(x[8,]) - x[8,8]) / (sum(x) - sum(x[,8])),
                        (sum(x[9,]) - x[9,9]) / (sum(x) - sum(x[,9]))
    ))
    x <- table(k10.svm.pred,k10.test); svm.acc$k10 <-  1-((sum(x) - sum(diag(x))) / sum(x))
    svm.rec$k10 <- mean(c(x[1,1] / sum(x[,1]), 
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
    svm.prec$k10 <- mean(c(x[1,1] / sum(x[1,]), 
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
    svm.FP$k10 <- mean(c((sum(x[1,]) - x[1,1]) / (sum(x) - sum(x[,1])), 
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
    
    
    metrix_2[[i]] =matrix(t(rbind(round(svm.acc,digits=2),
                                  round(lda.acc,digits=2),
                                  round(svm.prec,digits=2),
                                  round(lda.prec,digits=2),
                                  round(svm.rec,digits=2),
                                  round(lda.rec,digits=2),
                                  round(svm.FP,digits=2),
                                  round(lda.FP,digits=2))),nrow=9,ncol=8)
    print(paste0("Finish CV 2 Variables; ",size[j],"; ",i))
  }
  met15_all = c(met15_all,metrix_15)
  met5_all = c(met5_all,metrix_5)
  met2_all = c(met2_all,metrix_2)
}

#save(list=c("datt","KM.dat","image_matrix","met15_all","met5_all","met2_all"),file="CV_list_all.Rdata")

################################################################################################

load("CV_list_all.Rdata")

# 15 Prediction Variables
met15_8020 = met15_all[1:10]
met15_5050 = met15_all[11:20]
met15_2080 = met15_all[21:30]

(met15_8020[[1]][] + met15_8020[[2]][] + met15_8020[[3]][] + met15_8020[[4]][] + met15_8020[[5]][] + 
    met15_8020[[6]][] + met15_8020[[7]][] + met15_8020[[8]][] + met15_8020[[9]][] + met15_8020[[10]][]) / 10

(met15_5050[[1]][] + met15_5050[[2]][] + met15_5050[[3]][] + met15_5050[[4]][] + met15_5050[[5]][] + 
    met15_5050[[6]][] + met15_5050[[7]][] + met15_5050[[8]][] + met15_5050[[9]][] + met15_5050[[10]][]) / 10

(met15_2080[[1]][] + met15_2080[[2]][] + met15_2080[[3]][] + met15_2080[[4]][] + met15_2080[[5]][] + 
    met15_2080[[6]][] + met15_2080[[7]][] + met15_2080[[8]][] + met15_2080[[9]][] + met15_2080[[10]][]) / 10

# 5 Prediction Variables
met5_8020 = met5_all[1:10]
met5_5050 = met5_all[11:20]
met5_2080 = met5_all[21:30]

(met5_8020[[1]][] + met5_8020[[2]][] + met5_8020[[3]][] + met5_8020[[4]][] + met5_8020[[5]][] + 
    met5_8020[[6]][] + met5_8020[[7]][] + met5_8020[[8]][] + met5_8020[[9]][] + met5_8020[[10]][]) / 10

(met5_5050[[1]][] + met5_5050[[2]][] + met5_5050[[3]][] + met5_5050[[4]][] + met5_5050[[5]][] + 
    met5_5050[[6]][] + met5_5050[[7]][] + met5_5050[[8]][] + met5_5050[[9]][] + met5_5050[[10]][]) / 10

(met5_2080[[1]][] + met5_2080[[2]][] + met5_2080[[3]][] + met5_2080[[4]][] + met5_2080[[5]][] + 
    met5_2080[[6]][] + met5_2080[[7]][] + met5_2080[[8]][] + met5_2080[[9]][] + met5_2080[[10]][]) / 10

# 2 Prediction Variables
met2_8020 = met2_all[1:10]
met2_5050 = met2_all[11:20]
met2_2080 = met2_all[21:30]

(met2_8020[[1]][] + met2_8020[[2]][] + met2_8020[[3]][] + met2_8020[[4]][] + met2_8020[[5]][] + 
    met2_8020[[6]][] + met2_8020[[7]][] + met2_8020[[8]][] + met2_8020[[9]][] + met2_8020[[10]][]) / 10

(met2_5050[[1]][] + met2_5050[[2]][] + met2_5050[[3]][] + met2_5050[[4]][] + met2_5050[[5]][] + 
    met2_5050[[6]][] + met2_5050[[7]][] + met2_5050[[8]][] + met2_5050[[9]][] + met2_5050[[10]][]) / 10

(met2_2080[[1]][] + met2_2080[[2]][] + met2_2080[[3]][] + met2_2080[[4]][] + met2_2080[[5]][] + 
    met2_2080[[6]][] + met2_2080[[7]][] + met2_2080[[8]][] + met2_2080[[9]][] + met2_2080[[10]][]) / 10
