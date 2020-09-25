###
# EigenFruit Analysis
# Mitchell Feldmann
###

##########################################################################################
########################### pixel -- EIGENFRUIT take from Eigenfaces #####################
##########################################################################################
# Sirovich and Kirby (1987)
# Turk and Pentland (1991)
rm(list=ls())
library(keras)
library(imager)
library(lattice)
library(gridExtra)
library(factoextra)
library(cluster)
library(NbClust)
library(lme4)

pics <- list.files(path = "~/Desktop/Reading/Strawberry_Research/Heterosis/Salinas/Data/Images/ReSized",full.names = TRUE)

load_and_resize = function(path){image = load.image(path);resize(im = image,size_x = 100, size_y = 100)}

list_of_images = lapply(pics, load_and_resize) 

list_of_images.mat <- lapply(list_of_images, as.matrix) 

image_matrix = do.call('cbind', lapply(list_of_images.mat, as.numeric))
rm(list_of_images)
dim(image_matrix)
image_matrix = t(image_matrix)
dim(image_matrix)

pca_dat = prcomp(image_matrix,scale=T)

pca.x <- pca_dat$x[,c(1:20)]
pca.x <- as.data.frame(pca.x)
pca.x$name <- pics
