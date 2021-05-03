#Libraries

library(dplyr)
library(tidyverse)
library(stringr)
library(corrplot)  #for correlogram
library(e1071)  #for skewness(), kurtosis() and svm
library(randomForest) # Contains the randomForest() function

library(caret)
library(MASS)

library(splitstackshape)
library(pracma)
library(philentropy)
library(raster)
library(rgdal)
library(ForestTools)
library(lidR)
library(mapview)

library(gridExtra)  #for grid.arrange()
# library(rLiDAR)  #for CrownMetrics()  
library(RANN)

# intro/ understanding of data - Glight

# Variables created: did we do it correctly? all variables are created with the 4 original variables, there is an obvious linear dependency.

# Should we keep region? - No

# how do you address multicollinearity? 

# model_df, we have combined all observations summarized at tree level = 137.

# Data augmentation and SMOTE - it's common in certain practice to use these methods to create artificial data. Is that something people do here?

# Try Dead vs Alive only and compare with Alive, Green and Dead