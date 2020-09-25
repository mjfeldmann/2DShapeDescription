###
# Principal Component Analysis of each Pseudo Landmark.
# Mitchell Feldmann
###

##########################################################################################
############################### landmark -- Psuedo Landmark PCA ##########################
##########################################################################################
rm(list=ls())

data <- read.csv("~/Box/Desktop/Reading/Strawberry_Research/1_UnsupervisedQuantization/Zenodo/Quantitative_Strawberry_Features.csv")
pca.x=matrix(rep(NA,50*nrow(data)),ncol=50)
pca.y=matrix(rep(NA,50*nrow(data)),ncol=50)
var.x <- c()
var.y <- c()
theta <- c()
colnames(data)

for(i in 1:50){
  print(i)
  Re_Im <- data.frame(Re = data[,197+i],
                      Im = data[,247+i])
  pca_dat <- prcomp(Re_Im,center=TRUE, scale.=F)
  
  pca.x[,i] <- pca_dat$x[,1]
  pca.y[,i] <- pca_dat$x[,2]
  
  theta[i] = round(acos(pca_dat$rotation[1,1]) * (180/pi),digits=1)
  var.x[i] <- var(pca.x[,i])
  var.y[i] <- var(pca.y[,i])
}

pca <- cbind(pca.x,pca.y)
rownames(pca) <- data$label.z
write.csv(pca,"pca.csv",row.names = T)