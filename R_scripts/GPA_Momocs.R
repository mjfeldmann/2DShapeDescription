###
# Generalized procrustes analysis of pseudo landmarks with Momocs
# Mitchell Feldmann
###

##########################################################################################
############################### landmark -- Procrustes with Momocs #######################
##########################################################################################
rm(list=ls());library(readr);library(cowplot);library(Momocs)

lf <- list.files("~/Desktop/Reading/Strawberry_Research/Heterosis/Salinas/Data/Images/ReSized/", full.names=TRUE)
#lfn <- list.files("~/Desktop/Reading/Strawberry_Research/Heterosis/Salinas/Data/Images/ReSized/", full.names=F)

coo <- import_jpg(lf[-c(471,2880,2895,3283,3752,3940,3963,4362,5362,5367,5485,1686,2274,2689,3616,3860,4408,4763,4764,5089,5474,5817,5821,6284)])

coo.0 = import_jpg(lf[1686],threshold = 0.001)
coo.1 = import_jpg(lf[2274],threshold = 0.001)
coo.2 = import_jpg(lf[2689],threshold = 0.01)
coo.3 = import_jpg(lf[3616],threshold = 0.015)
coo.4 = import_jpg(lf[3860],threshold = 0.015)
coo.5 = import_jpg(lf[4408],threshold = 0.001)
coo.6 = import_jpg(lf[4763],threshold = 0.001)
coo.7 = import_jpg(lf[4764],threshold = 0.005)
coo.8 = import_jpg(lf[5089],threshold = 0.005)
coo.9 = import_jpg(lf[5474],threshold = 0.005)
coo.10 = import_jpg(lf[5817],threshold = 0.005)
coo.11 = import_jpg(lf[5821],threshold = 0.005)
coo.12 = import_jpg(lf[6284])
coo.13 = import_jpg(lf[471],threshold = 0.01)
coo.14 = import_jpg(lf[2880],threshold = 0.02)
coo.15 = import_jpg(lf[2895],threshold = 0.01)
coo.16 = import_jpg(lf[3283],threshold = 0.001)
coo.17 = import_jpg(lf[3752],threshold = 0.015)
coo.18 = import_jpg(lf[3940],threshold = 0.001)
coo.19 = import_jpg(lf[3963],threshold = 0.01)
coo.20 = import_jpg(lf[4362])
coo.21 = import_jpg(lf[5362],threshold = 0.01)
coo.22 = import_jpg(lf[5367],threshold = 0.01)
coo.23 = import_jpg(lf[5485],threshold = 0.001)

coo = append(coo,coo.0)
coo = append(coo,coo.1)
coo = append(coo,coo.2)
coo = append(coo,coo.3)
coo = append(coo,coo.4)
coo = append(coo,coo.5)
coo = append(coo,coo.6)
coo = append(coo,coo.7)
coo = append(coo,coo.8)
coo = append(coo,coo.9)
coo = append(coo,coo.10)
coo = append(coo,coo.11)
coo = append(coo,coo.12)
coo = append(coo,coo.13)
coo = append(coo,coo.14)
coo = append(coo,coo.15)
coo = append(coo,coo.16)
coo = append(coo,coo.17)
coo = append(coo,coo.18)
coo = append(coo,coo.19)
coo = append(coo,coo.20)
coo = append(coo,coo.21)
coo = append(coo,coo.22)
coo = append(coo,coo.23)

#l=c()
#for(i in 1:6874){
#  l[i] = dim(coo[[i]])[1]
#}
#plot(l)
#
#KM.dat <- read.csv("~/Box/Desktop/Reading/Strawberry_Research/Heterosis/Salinas/Data/2018_Salinas_Fruit_Morph_KMA.csv")
#k8 <- KM.dat[which(KM.dat$K8 == 6),]
#
#coo.rev = which(names(coo) %in% k8$label.z)
#
#for(i in coo.rev){
#  print(i)
#  coo[[i]][,1] = max(coo[[i]][,1]) - coo[[i]][,1]
#  coo[[i]] = apply(coo[[i]],2,rev)
#}

coo2 <- list()

for(i in 1:length(coo)){
  coo2[[i]] <- coo[[i]][round(seq(1,nrow(coo[[i]]),by=nrow(coo[[i]])/50),digits=0),] 
}

strw <- Ldk(coo2)

strw.pr <- fgProcrustes(strw)

dist.mat <- matrix(rep(0,50*length(strw.pr)),ncol=50,nrow = length(strw.pr))
disp.mat <- matrix(rep(0,50*length(strw.pr)),ncol=50,nrow = length(strw.pr))

for(i in 1:50){ #grab (x,y) of i pseudo land mark from all objects
  print(i)
  x=c()
  y=c()
  for(j in 1:length(strw.pr)){
    x[j] <- matrix(unlist(strw.pr[[1]][j]),ncol=2)[i,1]
    y[j] <- matrix(unlist(strw.pr[[1]][j]),ncol=2)[i,2]
  }
  mean.x <- mean(x)
  mean.y <- mean(y)
  
  dist.mat[,i] <- sqrt(((x - mean.x)**2) + ((y -mean.y)**2))
  disp.mat[,i] <- atan((y - mean.y)/(x -mean.x))
  
  for(k in 1:length(x)){
    ifelse((x[k]-mean.x > 0 && y[k]-mean.y > 0), disp.mat[k,i] <-  disp.mat[k,i],
           ifelse((x[k]-mean.x < 0 && y[k]-mean.y > 0), disp.mat[k,i] <-  pi + disp.mat[k,i],
                  ifelse((x[k]-mean.x < 0 && y[k]-mean.y < 0), disp.mat[k,i] <-  pi + disp.mat[k,i],
                         ifelse((x[k]-mean.x > 0 && y[k]-mean.y < 0), disp.mat[k,i] <-  2*pi + disp.mat[k,i], 
                                disp.mat[k,i] <-  disp.mat[k,i]))))
  }
}


x <- matrix(unlist(strw.pr[[1]][10]),ncol=2)[,1]
y <- matrix(unlist(strw.pr[[1]][10]),ncol=2)[,2]

rownames(dist.mat) <- names(coo)
rownames(disp.mat) <- names(coo)

real.mat <- dist.mat*cos(disp.mat)
img.mat <- dist.mat*sin(disp.mat)

rownames(real.mat) <- names(coo)
rownames(img.mat) <- names(coo)

write.csv(dist.mat,"procrustes.distance.csv",row.names = T)
write.csv(disp.mat,"procrustes.displacement.csv",row.names = T)
write.csv(real.mat,"procrustes.real.csv",row.names = T)
write.csv(img.mat,"procrustes.imaginary.csv",row.names = T)