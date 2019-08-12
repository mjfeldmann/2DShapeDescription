###
# Structural Equation Modeling of PsuedoLandmark PCs using Lavaan
# Mitchell Feldmann
###

##########################################################################################
############################ landmark -- Structural Equation Modelling ###################
##########################################################################################
rm(list=ls())
data <- read.csv("~/Box/Desktop/Reading/Strawberry_Research/1_UnsupervisedQuantization/Zenodo/Quantitative_Strawberry_Features.csv")
name = data$label.z
library(tidyverse)
library(tidygraph)
library(ggraph)
library(lavaan)
library(semPlot)
library(semTools)
library(nonnest2)
colnames(data)
#data = as.data.frame(scale(data[,c(221,223,229,231,233,235,243,245,247,249,257,259,261,263,267,269)],center=T,scale=T))
dat = as.data.frame(scale(data[,c(298:347)],center=T,scale=T))

dat[,49] <- -1 * dat[,49]
dat[,48] <- -1 * dat[,48]
dat[,1] <- -1 * dat[,1]
dat[,2] <- -1 * dat[,2]
dat[,3] <- -1 * dat[,3]
dat[,4] <- -1 * dat[,4]
dat[,13] <- -1 * dat[,13]
dat[,14] <- -1 * dat[,14]
dat[,26] <- -1 * dat[,26]
dat[,27] <- -1 * dat[,27]
dat[,28] <- -1 * dat[,28]

SHP.model <- ' Tip    =~ PC1_1  + PC1_2  + PC1_3  + PC1_4  + PC1_47 + PC1_48 + PC1_49 + PC1_50
               Side_L =~ PC1_10 + PC1_11 + PC1_12 + PC1_13 + PC1_14 
               Neck   =~ PC1_23 + PC1_24 + PC1_25 + PC1_26 + PC1_27 + PC1_28 
               Side_R =~ PC1_37 + PC1_38 + PC1_39 + PC1_40 + PC1_41'
          
fit <- lavaan::sem(SHP.model, data = dat,std.lv=T,estimator = "DWLS")

summary(fit)
parameterestimates(fit)

table = predict(fit)

data$tip = table[,1]
data$side_L = table[,2]
data$neck = table[,3]
data$side_R = table[,4]

