2DShapeDescription
Code for performing a variety of 2D shape description using R

This code is specifically designed to work on binary images of strawberries where black pixels (value = 0) represent foreground (object) and white pixels (value = 1) are background (not object).

The bulk of this project coming soon.

1. Segmentation
   > Lists the following: ImageJ Macro\\
   > Description: Batch segments all images within a directory using two different segmentators\\
   > File name: BatchSIOX_multiFruit.ijm\\
   > Operating system(s): Platform independent\\
   > Programming language: ImageJ\\
   > Other requirements: Two saved SIOX segmentators\\
   > License: MIT License\\

2. Cropping and Edge Addition
    >Lists the following: R code\\
    >Description: Crops images based on positions of bounding rectangles and adds white pixels to the edges\\
    >File name: img_crop.R\\
    >Operating system(s): Platform independent\\
    >Programming language: R\\
    >Other requirements: Not Applicable\\
    >License: MIT License\\

3. ROI Scale
    >Lists the following: ImageJ Macro\\
    >Description: Scales ROI (black) pixels so that the major axis become 1000px\\
    >File name: imgScale.ijm\\
    >Operating system(s): Platform independent\\
    >Programming language: imj macro\\
    >Other requirements: Two saved SIOX segmentators\\
    >License: MIT License\\