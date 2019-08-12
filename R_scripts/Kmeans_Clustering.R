###
# Clustering Images using K-means clustering
# Mitchell Feldmann
###

##########################################################################################
################################## category -- K-means Clustering  #######################
##########################################################################################
rm(list=ls())
library(keras)
library(imager)
library(lattice)
library(factoextra)
library(cluster)
library(NbClust)
library(lme4)

pics <- list.files(path = "~/Desktop/Reading/Strawberry_Research/Heterosis/Salinas/Data/Images/ReSized",full.names = TRUE)

load_and_resize = function(path){image = load.image(path);resize(im = image,size_x = 100, size_y = 100)}

list_of_images = lapply(pics, load_and_resize) 

#Harvest 1
image_matrix = do.call('cbind', lapply(list_of_images, as.numeric))
rm(list_of_images)
dim(image_matrix)
image_matrix = t(image_matrix)

fit2 = kmeans(image_matrix, centers = 2)
fit3 = kmeans(image_matrix, centers = 3)
fit4 = kmeans(image_matrix, centers = 4)
fit5 = kmeans(image_matrix, centers = 5)
fit6 = kmeans(image_matrix, centers = 6)
fit7 = kmeans(image_matrix, centers = 7)
fit8 = kmeans(image_matrix, centers = 8)
fit9 = kmeans(image_matrix, centers = 9)
fit10 = kmeans(image_matrix, centers = 10)

centroids2 = fit2$centers
centroids3 = fit3$centers
centroids4 = fit4$centers
centroids5 = fit5$centers
centroids6 = fit6$centers
centroids7 = fit7$centers
centroids8 = fit8$centers
centroids9 = fit9$centers
centroids10 = fit10$centers

data <- data.frame(name = pics,
                   K2=fit2$cluster,
                   K3=fit3$cluster,
                   K4=fit4$cluster,
                   K5=fit5$cluster,
                   K6=fit6$cluster,
                   K7=fit7$cluster,
                   K8=fit8$cluster,
                   K9=fit9$cluster,
                   K10=fit10$cluster)