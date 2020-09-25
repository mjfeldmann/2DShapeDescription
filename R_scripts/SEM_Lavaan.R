###
# Structural Equation Modeling of PsuedoLandmark PCs using Lavaan
# Mitchell Feldmann
###

##########################################################################################
############################ landmark -- Structural Equation Modelling ###################
##########################################################################################
rm(list=ls())
data <- read.csv("~/Box/Desktop/Reading/Strawberry_Research/Heterosis/Salinas/Data/2018_Salinas_Fruit_Morph_GPA.csv")
name = data$label.z
library(tidyverse)
library(tidygraph)
library(ggraph)
library(lavaan); library(nlsem); library(regsem)
library(semPlot)
library(semTools)
library(nonnest2)
colnames(dat)
#data = as.data.frame(scale(data[,c(221,223,229,231,233,235,243,245,247,249,257,259,261,263,267,269)],center=T,scale=T))
dat = as.data.frame(scale(data[,c(222:271)],center=T,scale=T))



dat[,11] <- -1 * dat[,11]
dat[,12] <- -1 * dat[,12]
dat[,13] <- -1 * dat[,13]
dat[,26] <- -1 * dat[,26]
dat[,24] <- -1 * dat[,24]
dat[,25] <- -1 * dat[,25]
dat[,42] <- -1 * dat[,42]
dat[,43] <- -1 * dat[,43]
dat[,38] <- -1 * dat[,38]
dat[,39] <- -1 * dat[,39]
dat[,40] <- -1 * dat[,40]
dat[,41] <- -1 * dat[,41]
dat[,48] <- -1 * dat[,48]


SHP.model <- ' Tip    =~ PC1_1  + PC1_2  + PC1_3  + PC1_4 + PC1_5 + PC1_48 + PC1_49 + PC1_50
               Side_L =~ PC1_15 + PC1_11 + PC1_12 + PC1_13 + PC1_14 
               Neck   =~  PC1_24 + PC1_25 + PC1_26 + PC1_27 + PC1_28 + PC1_29
               Side_R =~ PC1_38 + PC1_39 + PC1_40 + PC1_41 + PC1_42 + PC1_43
               Shape =~ Tip + Side_L + Neck + Side_R
               Tip ~~ 1*Tip
               Side_L ~~ 1*Side_L
               Side_R ~~ 1*Side_R
               Neck ~~ 1*Neck
               Shape ~~ 1*Shape
               '


fit <- lavaan::sem(SHP.model, data = dat,std.lv=T,estimator="DWLS")

inspect(fit, "sample")$cov
inspect(fit, "sigma")
heatmap(residuals(fit)$cov**2,symm = T,margins = c(8,8))
hist(residuals(fit)$cov**2)

parameterestimates(fit,standardized = T)
table = lavPredict(fit)

