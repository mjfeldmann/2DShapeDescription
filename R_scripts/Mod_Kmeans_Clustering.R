###
# Clustering Modified Images using K-means clustering
# Mitchell Feldmann
###

##########################################################################################
############################# category -- Modified K-means Clustering ####################
##########################################################################################
# After visually assessing the clusters from the K-Means algorithm, it was clear that some
# categories were artifactual, specifically the "Lobed" categories. I merged these.
rm(list=ls())
library(keras)
library(imager)
library(lattice)
library(factoextra)
library(cluster)
library(NbClust)
library(lme4)
library(nnet)
??resize
pics <- list.files(path = "~/Desktop/Reading/Strawberry_Research/Heterosis/Salinas/Data/Images/ReSized",full.names = TRUE)

load_and_resize = function(path){image = load.image(path);resize(im = image,size_x = 100, size_y = 100)}

KM.dat <-  read.csv("~/Box/Desktop/Reading/Strawberry_Research/1_UnsupervisedQuantization/Zenodo/Quantitative_Strawberry_Features.csv")
k8 <- KM.dat$K8
sort(pics)
list_of_images = lapply(pics, load_and_resize) 

list_of_images.mat <- lapply(list_of_images, as.matrix) 

for(i in which(k8==6)){
  print(i)
  list_of_images.mat[[i]] <- list_of_images.mat[[i]][100:1,]
}

image_matrix = do.call('cbind', lapply(list_of_images.mat, as.numeric))
dim(image_matrix)
image_matrix = t(image_matrix)

rm(list_of_images,list_of_images.mat,pics,k8,KM.dat)
#save(list = "image_matrix",file= "imageMat2.RData")
fit2 = kmeans(image_matrix, centers = 2)
fit3 = kmeans(image_matrix, centers = 3)
fit4 = kmeans(image_matrix, centers = 4)
fit5 = kmeans(image_matrix, centers = 5)
fit6 = kmeans(image_matrix, centers = 6)
fit7 = kmeans(image_matrix, centers = 7)
fit8 = kmeans(image_matrix, centers = 8)
fit9 = kmeans(image_matrix, centers = 9)
fit10 = kmeans(image_matrix, centers = 10)

data <- data.frame(name=pics,
                   k2=fit2$cluster,
                   k3=fit3$cluster,
                   k4=fit4$cluster,
                   k5=fit5$cluster,
                   k6=fit6$cluster,
                   k7=fit7$cluster,
                   k8=fit8$cluster,
                   k9=fit9$cluster,
                   k10=fit10$cluster)
