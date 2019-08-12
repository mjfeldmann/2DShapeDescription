2DShapeDescription
Code for performing a variety of 2D shape description using R

This code is specifically designed to work on binary images of strawberries where black pixels (value = 0) represent foreground (object) and white pixels (value = 1) are background (not object).

The bulk of this project coming soon.

1. Segmentation  
    >Lists the following: ImageJ Macro
    >Description: Batch segments all images within a directory using two different segmentators  
    >File name: BatchSIOX_multiFruit.ijm  
    >Operating system(s): Platform independent  
    >Programming language: ImageJ  

    We used imageJ to train two different SIOX segmentators using "BatchSIOX_multiFruit.ijm", referred to as "segmentator-NormalMulti.siox" and "segmentator-LightMulti.siox". 
     We did this to account for the fact that strawberries with very light color were not properly segmented using a segmentator trained on your average  red strawberries.
     Both generated images were averaged using "binary_image_mean.R" and pixel values = 0.5 were set to 0. 
     Images were then checked for defects and holes in the segments. Holes were closed in ImageJ by hand using the 'Wand (tracing) tool".

2. Cropping, Edge Addition , and Scaling
    >Lists the following: R code  
    >Description: Crops images based on positions of bounding rectangles and adds white pixels to the edges. Scales all images to 1000x1000 pixels.
    >File name: CropImages_ExtendBorders.R
    >Operating system(s): Platform independent  
    >Programming language: R  

    The bounding box of each object was extracted using ImageJ. This was done using the "brute force" method. 
     Each binary multi fruit image is first opened in imageJ and all of the fruit in the image were individually selected with the "Wand (tracing) tool" and all of the measurements that imageJ performed were recorded.
     As this step was done by hand, there is no code in this repository for this task. Since all of the fruit land in 1 or 3 positions in all of the images, given a consisten orientation, we could have written an imageJ macro, similar to the segmentation macro to do this.

3. K-means Clustering  
    >Lists the following: R code  
    >Description: Flattens scaled binary images and performs K-means clustering.
    >File name: Kmeans_Clustering.R 
    >Operating system(s): Platform independent  
    >Programming language: R

    Each image is first resized to 100x100px. The images are then flattened into 10,000 element vectors. 
     Kmeans clustering is performed on this vector across all images.

4. Modified K-means Clustering  
    >Lists the following: R code   
    >Description: Rotates and flattens scaled binary images and performs K-means clustering.
    >File name: Mod_Kmeans_Clustering.R 
    >Operating system(s): Platform independent  
    >Programming language: R

    Each image is first resized to 100x100px. The images in K8_C6 are rotated about their vertical axis.
     The images are then flattened into 10,000 element vectors. Kmeans clustering is performed on this vector across all images.

5. Principal Progression of K Clusters
    >Lists the following: R code   
    >Description: Takes the output from either Kmeans approach and returns a list containing the order of the factor levels.
    >File name: PPKC.R 
    >Operating system(s): Platform independent  
    >Programming language: R

    All that goes in is a data.frame where each row is an image and each column is a value of K.
     Each cell is teh cluster assigned to a given image for a specific value of K.

6. Cumulative Logit Mixed Models
    >Lists the following: R code 
    >Description: Estimates Heritability using a cumulative logit mixed model
    >File name: VarComp_CLMM.R
    >Operating system(s): Platform independent  
    >Programming language: R

    Used the 'ordinal::' R package to perform variance component estiamation using clmm().

7. Elliptical Fourier Analysis with Momocs
    >Lists the following: R code 
    >Description: Performs EFA using the Momocs package in R
    >File name: EFA_Momocs.R
    >Operating system(s): Platform independent  
    >Programming language: R

    I had to specify thresholds for several images that wouldn't load properly and would crash R.

8. Procrustes analysis with Momocs
    >Lists the following: R code 
    >Description: Performs GPA using the Momocs package in R
    >File name: GPA_Momocs.R
    >Operating system(s): Platform independent  
    >Programming language: R

    I had to specify thresholds for several images that wouldn't load properly and would crash R.
     This uses the same load sequence as EFA_Momocs.R

9. PCA of Psuedolandmarks
    >Lists the following: R code 
    >Description: Performs PCA using the 50 landmarks used in GPA
    >File name: Psuedolandmarks_PCA.R
    >Operating system(s): Platform independent  
    >Programming language: R

    The first principal axis is extracted from each of the 50 landmarks. 
     Those that are more variable than the median are kept for SEM.

10. Structured Equation Models of PsuedoLandmarks
    >Lists the following: R code 
    >Description: Builds and evaluates a structured equation model using the Psuedolandmarks PCs.
    >File name: SEM_Lavaan.R
    >Operating system(s): Platform independent  
    >Programming language: R

11. Biomass Profile PCA
    >Lists the following: R code 
    >Description: Calculated the biomass profile in the horizontal and vertical directions
    >File name: BiomassProfile_PCA.R
    >Operating system(s): Platform independent  
    >Programming language: R

    Binary images are scaled to 100x100. RowSums and ColSums are calculated for each of 100 rows and 100 columns. 
     PCA is performed on the 100 element vectors of row or column sums.

12. EigenFruit Analysis
    >Lists the following: R code 
    >Description: EigenFruit analysis is performed!
    >File name: EigenFruit_PCA.R
    >Operating system(s): Platform independent  
    >Programming language: R

    Binary images are scaled to 100x100 and flattened to 10,000 element vectors.
     PCA is performed on the 10,000 element vectors pixel values.

13. Variable Selection
    >Lists the following: R code 
    >Description: EigenFruit analysis is performed!
    >File name: VariablesSelection_Classification.R
    >Operating system(s): Platform independent  
    >Programming language: R

    VSURF package is used to train random forests for variable selection.

14. Classification and Cross Validation
    >Lists the following: R code 
    >Description: EigenFruit analysis is performed!
    >File name: Validation_EXP.R
    >Operating system(s): Platform independent  
    >Programming language: R

    A monumnetal for loop that performs multiple instances of cross validation using SVM and LDA of the 9 values of K.
     Accuracy, Precision, FPR, and Recall are all calculated within the loop.
