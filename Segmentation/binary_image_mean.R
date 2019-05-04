rm(list=ls())
### step 1
### average pixel values in binary images
###
library(magick)
library(jpeg)
library(OpenImageR)
library(readxl)

# Requires path to directory containing images segmented with "segmentator_LightMulti.siox" and "segmentator_NormalMulti.siox", respectively
names_mL = list.files("")
names_mN = list.files("")

for(i in 1:length(names_mL)){
  print(i)
  
  image_mL = readJPEG(paste0("",names_mL[i]))
  image_mN = readJPEG(paste0("",names_mN[i]))
  
  image_new <- round( (image_mL + image_mN) / 2)
  
  # Requires path to new directory to contain the joint images
  writeImage(image_new, paste0("",names_mN[i]))
}



