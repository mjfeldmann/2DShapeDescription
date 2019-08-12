###
# Crop Individual fruit and extend image borders
# Mitchell Feldmann
###

##########################################################################################
########################### process -- Cut Individual Fruit ##############################
##########################################################################################
library(jpeg)
data <-  read.csv("~/Box/Desktop/Reading/Strawberry_Research/1_UnsupervisedQuantization/Zenodo/Quantitative_Strawberry_Features.csv")
#C1: I crop each image in the two different harvest dates separately from one another because of the naming scheme for the image names, which are duplicated between the harvest dates.

#Harvest Date 1
data_h1 <- data[which(data$harvest==1),]
names1 <- data_h1$label

for(i in 1:length(names1)){
  print(i)
  #adds 10 white pixels around the border
  w = data_h1$Width[i]+20; h = data_h1$Height[i]+20; 
  x = data_h1$BX[i]-10; y = data_h1$BY[i]-10
  
  img <- readJPEG(paste0("~/Desktop/Reading/Strawberry_Research/Heterosis/Salinas/Data/Images/H1_mask/",names1[i]))
  
  img_1 <- round(img[c(y:(y+h)),c(x:(x+w))])
  
  writeJPEG(img_1,paste0("~/Desktop/Reading/Strawberry_Research/Heterosis/Salinas/Data/Images/H1_indiv/",data_h1$fruit[i],"_",names1[i]))
}

#Harvest Date 2
data_h2 <- data[which(data$harvest==2),]
names2 <- data_h2$label

for(i in 1:length(names2)){
  print(i)
  #adds 10 white pixels around the border
  w = data_h2$Width[i]+20; h = data_h2$Height[i]+20; 
  x = data_h2$BX[i]-10; y = data_h2$BY[i]-10
  
  img <- readJPEG(paste0("~/Desktop/Reading/Strawberry_Research/Heterosis/Salinas/Data/Images/H2_mask/",names2[i]))
  
  img_1 <- round(img[c(y:(y+h)),c(x:(x+w))])
  
  writeJPEG(img_1,paste0("~/Desktop/Reading/Strawberry_Research/Heterosis/Salinas/Data/Images/H2_indiv/",data_h2$fruit[i],"_",names2[i]))
}

##########################################################################################
########################## process -- Extend Image Borders ###############################
##########################################################################################
rm(list=ls());library(jpeg); library(magick); library(imager)
data <-  read.csv("~/Box/Desktop/Reading/Strawberry_Research/Heterosis/Salinas/Data/2018_Salinas_Fruit_Morph.csv")

data_h1 <- data[which(data$harvest==1),]; data_h2 <- data[which(data$harvest==2),]

for(i in 1:nrow(data_h1)){
  print(i)
  
  maj = max(data_h1$Width[i], data_h1$Height[i])
  dw = (maj - data_h1$Width[i]) / 2
  dh = (maj - data_h1$Height[i]) / 2
  
  img <- image_read(paste0("~/Desktop/Reading/Strawberry_Research/Heterosis/Salinas/Data/Images/H1_indiv/",data_h1$label.z[i],".jpg"))
  
  img_1 <- image_border(img, "#ffffff", paste0(dw,"x",dh))
  
  img_2 = image_resize(img_1, "1000x1000")
 
  image_write(img_2,paste0("~/Desktop/Reading/Strawberry_Research/Heterosis/Salinas/Data/Images/H1_sized/",data_h1$label.z[i],".jpg"),format = "jpeg")
}

for(i in 1:nrow(data_h2)){
  print(i)
  
  maj = max(data_h2$Width[i], data_h2$Height[i])
  dw = (maj - data_h2$Width[i]) / 2
  dh = (maj - data_h2$Height[i]) / 2
  
  img <- image_read(paste0("~/Desktop/Reading/Strawberry_Research/Heterosis/Salinas/Data/Images/H2_indiv/",data_h2$label.z[i],".jpg"))
  
  img_1 <- image_border(img, "#ffffff", paste0(dw,"x",dh))
  
  img_2 = image_resize(img_1, "1000x1000")
  
  image_write(img_2,paste0("~/Desktop/Reading/Strawberry_Research/Heterosis/Salinas/Data/Images/H2_sized/",data_h2$label.z[i],".jpg"),format = "jpeg")
}