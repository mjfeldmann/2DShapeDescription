###
# Biomass Profile PCs
# Mitchell Feldmann
###

##########################################################################################
############################### pixel -- Fruit Biomass Profile ###########################
##########################################################################################
rm(list=ls())
library(keras)
library(imager)
library(lattice)
library(factoextra)
library(cluster)
library(NbClust)


pics <- list.files(path = "~/Desktop/Reading/Strawberry_Research/Heterosis/Salinas/Data/Images/ReSized",full.names = TRUE)

load_and_resize = function(path){image = load.image(path);resize(im = image,size_x = 100, size_y = 100)}

list_of_images = lapply(pics, load_and_resize) 

list_of_images.mat <- lapply(list_of_images, as.matrix) 


v.biomass <- matrix(rep(0,100*length(list_of_images)),ncol=100)
h.biomass <- matrix(rep(0,100*length(list_of_images)),ncol=100)

for(i in 1:length(list_of_images)){
  print(i)
  v.biomass[i,] <- 100 - rowSums(list_of_images.mat[[i]])
  h.biomass[i,] <- 100 - colSums(list_of_images.mat[[i]])
}

v_pca_dat <- prcomp(v.biomass)
h_pca_dat <- prcomp(h.biomass)

pca.x <- cbind(v_pca_dat$x[,c(1:5)], h_pca_dat$x[,c(1:5)])
pca.x <- as.data.frame(pca.x)
pca.x$name = pics
