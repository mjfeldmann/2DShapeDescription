###
# Ordering cluster using Principal Progresion of K Clusters
# Mitchell Feldmann
###

##########################################################################################
######################### category -- Principal Progression of K Clusters ################
##########################################################################################
rm(list=ls())
data <- read.csv("~/Box/Desktop/Reading/Strawberry_Research/1_UnsupervisedQuantization/Zenodo/Fruit_image_data.csv")

data$K2 <- as.factor(data$K2) # 1 2 
data$K3 <- as.factor(data$K3) # 1 3 2
data$K4 <- as.factor(data$K4) # 1 2 3 4
data$K5 <- as.factor(data$K5) # 4 5 1 3 2
data$K6 <- as.factor(data$K6) # 5 1 6 3 4 2
data$K7 <- as.factor(data$K7) # 7 3 6 1 2 4 5
data$K8 <- as.factor(data$K8) # 6 2 1 5 3 8 4 7
data$K9 <- as.factor(data$K9) # 5 3 2 1 8 7 9 4 6
data$K10 <- as.factor(data$K10) # 9 2 1 3 8 10 7 5 6 4

colnames(data)

dat = data[,c(428:436)] #columns with clusters.

PPKC=list()
for(i in 2:ncol(dat)){
  lev = levels(dat[,i])
  prev = c(1:(i-1))
  
  n = ((i)^2 + (i))/ 2 -1
  x = c()
  for(j in 1:length(lev)){
    for(k in 1:max(prev)){
      
      y = table(dat[which(dat[,i] == lev[j]),k]) / length(which(dat[,i] == lev[j]))
      x = c(x,y)
    } 
  }
  M = matrix(x,ncol=i+1, nrow=n)
  CM = as.data.frame(cov(M))
  PPKC[[i-1]] = sort(prcomp(CM)$rotation[,1],decreasing=F)
}
