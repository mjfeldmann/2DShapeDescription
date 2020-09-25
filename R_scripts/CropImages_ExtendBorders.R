###
# Crop Individual fruit and extend image borders
# Mitchell Feldmann
###

##########################################################################################
########################### process -- Cut Individual Fruit ##############################
##########################################################################################
library(jpeg)
data <-  read.csv("~/Box/Public/Desktop/Reading/Strawberry_Research/1_UnsupervisedQuantization/Zenodo/Quantitative_Strawberry_Features.csv")
#C1: I crop each image in the two different harvest dates separately from one another because of the naming scheme for the image names, which are duplicated between the harvest dates.

#Harvest Date 1
data_h1 <- data[which(data$harvest==1),]
names1 <- data_h1$label.y

for(i in 1:5){
  print(i)
  #adds 10 white pixels around the border
  w = data_h1$Width[i]+10; h = data_h1$Height[i]+10; 
  x = data_h1$BX[i]-5; y = data_h1$BY[i]-5
  
  img <- readJPEG(paste0("~/Box/Public/Desktop/Reading/Strawberry_Research/Heterosis/Salinas/Data/Images/H1_mask/",names1[i]))
  
  img_1 <- round(img[c(y:(y+h)),c(x:(x+w))])
  
  writeJPEG(img_1,paste0("~/Desktop/",data_h1$fruit[i],"_",names1[i]))
}

#Harvest Date 2
data_h2 <- data[which(data$harvest==2),]
names2 <- data_h2$label

for(i in 1:length(names2)){
  print(i)
  #adds 10 white pixels around the border
  w = data_h2$Width[i]+10; h = data_h2$Height[i]+10; 
  x = data_h2$BX[i]-50; y = data_h2$BY[i]-50
  
  img <- readJPEG(paste0("~/Desktop/Reading/Strawberry_Research/Heterosis/Salinas/Data/Images/H2_mask/",names2[i]))
  
  img_1 <- round(img[c(y:(y+h)),c(x:(x+w))])
  
  writeJPEG(img_1,paste0("~/Desktop/Reading/Strawberry_Research/Heterosis/Salinas/Data/Images/H2_indiv/",data_h2$fruit[i],"_",names2[i]))
}

##########################################################################################
########################## process -- Extend Image Borders ###############################
##########################################################################################
rm(list=ls());library(jpeg); library(magick); library(imager)
data <-  read.csv("~/Box/Public/Desktop/Reading/Strawberry_Research/Heterosis/Salinas/Data/2018_Salinas_Fruit_Morph.csv")

data_h1 <- data[which(data$harvest==1),]; data_h2 <- data[which(data$harvest==2),]

for(i in 1:10){
  print(i)
  maj = max(data_h1$Width[i], data_h1$Height[i])
  dw = (maj - data_h1$Width[i]) / 2
  dh = (maj - data_h1$Height[i]) / 2
  
  img <- image_read(paste0("~/Box/Public/Desktop/Reading/Strawberry_Research/Heterosis/Salinas/Data/Images/H1_indiv/",data_h1$label.z[i],".jpg"))
  
  img_0 <-  image_morphology(img, "OpenI", "Octagonal",iter=3)
  
  img_1 <- image_border(img_0, "#ffffff", paste0(dw,"x",dh))
  
  img_2 = image_resize(img_1, geometry = paste0(1000,"x",1000))
  
  img_3 =  image_border(img_2, "#ffffff", paste0((1000-image_info(img_2)[2])*0.5,"x",(1000-image_info(img_2)[3])*0.5))
  
 
  image_write(img_2,paste0("~/Desktop/",data_h1$label.z[i],".jpg"),format = "jpeg")
}
?image_border
for(i in 1:nrow(data_h2)){
  print(i)
  
  maj = max(data_h2$Width[i], data_h2$Height[i])
  dw = (maj - data_h2$Width[i]) / 2
  dh = (maj - data_h2$Height[i]) / 2
  
  img <- image_read(paste0("~/Desktop/Reading/Strawberry_Research/Heterosis/Salinas/Data/Images/H2_indiv/",data_h2$label.z[i],".jpg"))
  
  img_0 <-  image_morphology(img, "OpenI", "Octagonal",iter=3)
  
  img_1 <- image_border(img_0, "#ffffff", paste0(dw,"x",dh))
  
  img_2 = image_resize(img_1, geometry = paste0(1000,"x",1000))
  
  img_3 =  image_border(img_2, "#ffffff", paste0((1000-image_info(img_2)[2])*0.5,"x",(1000-image_info(img_2)[3])*0.5))
  

  
  image_write(img_2,paste0("~/Desktop/Reading/Strawberry_Research/Heterosis/Salinas/Data/Images/H2_sized/",data_h2$label.z[i],".jpg"),format = "jpeg")
}
