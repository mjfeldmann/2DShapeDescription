###
# Elliptical Fourier Analysis with Momocs
# Mitchell Feldmann
###

##########################################################################################
############################# outline -- Fourier Disc. with Momocs #######################
##########################################################################################
rm(list=ls());library(readr);library(cowplot);library(Momocs)

lf <- list.files("~/Desktop/Reading/Strawberry_Research/Heterosis/Salinas/Data/Images/ReSized/", full.names=TRUE)
#lfn <- list.files("~/Desktop/Reading/Strawberry_Research/Heterosis/Salinas/Data/Images/ReSized/", full.names=F)

#Certain images seem to crash the import_jpg() function.
coo <- import_jpg(lf[-c(471,2880,2895,3283,3752,3940,3963,4362,5362,5367,5485,1686,2274,2689,3616,3860,4408,4763,4764,5089,5474,5817,5821,6284)])

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

strw <- Out(coo)

coo_oscillo(strw[1], "efourier")
Ptolemy(strw[1],nb.h = 5)
calibrate_harmonicpower_efourier(strw,nb.h = 5)
calibrate_deviations_efourier(strw,range = c(1:5),plot=T) 
calibrate_reconstructions_efourier(strw,range = c(1:5))

strw.f <- efourier(strw, nb.h=5,norm=F,start=F)

mean.shape <- MSHAPES(strw.f,nb.pts = 500)

hcontrib(strw.f,harm.r = 1:5,mp.r = c(0,1,2,5,10))

efd.x <- strw.f[]

strw.p <- PCA(strw.f)

PCA.x <- strw.p$x

table(rownames(PCA.x)==rownames(efd.x))

data <- cbind(efd.x,PCA.x)
rownames(data)
colnames(data)


