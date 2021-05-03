###################################################################################
# READ in files ----

# Aliya
sfm_dir <- "F:/Users/aa0856a/ENVS_RS Dropbox/Projects/pix4d/201807XX_Denali_FHM_06/"

# Minh
sfm_dir <- "F:/mnguyen/ENVS_RS Dropbox/Projects/pix4d/201807XX_Denali_FHM_06/"

# Set working directory
setwd(sfm_dir); getwd()

# Read in csv and las files
raw_df <- read.csv("../../STV/code/R/AliyaMinh/data/raw_df.csv") %>% 
  mutate(region = as.factor(region),
         status = as.factor(status)) # add DF for preprocesisng/analyzing
id_df <- read.csv("../../STV/code/R/AliyaMinh/data/id_df.csv") %>% 
  mutate(region = as.factor(region),
         status = as.factor(status)) # add id_df
missing_df <- read.csv("../../STV/code/R/AliyaMinh/data/missing_df.csv") %>% 
  mutate(confirmed = as.factor(confirmed)) # add missing_df to see missing trees

# Minh: Do we need this ?
# Read in hand drawn crowns shapefile for cal/val
fhm_crowns_shp <- readOGR("../../Alaska/fhm_2018_uav/ak_spruce_crowns.shp")
#fhm_crowns_shp@data$Id <- 1:length(fhm_crowns_shp@data$Id) # add in unique id (not really needed in descriptive)
fhm_crowns_shp

# Read in LAS file
all_las_crowns <- readLAS("../../STV/code/R/AliyaMinh/data/all_las_crowns.las")

# Not needed unless you want to process the las files again and re-add treeid (already in the raw_df)
all_las_crowns <- merge_spatial(all_las_crowns, fhm_crowns_shp, "treeid") # filter such that only points within the polygons remains

###################################################################################
# Pre-Processing and Feature Extraction ----

#create model_df and run feature extraction
model_df <- raw_df %>%
  group_by(treeid) %>%
  #find color statistics
  mutate(height = max(Z),
         ht_mean = mean(Z), # changed to mean of (Z) from mean(height)
         ht_skw_Sp = skewness(Z),
         ht_kurt_sp = kurtosis(Z),
         ht_75p = as.numeric(quantile(Z, 0.75)),
         ht_90p = as.numeric(quantile(Z, 0.9)), 
         ht_98p = as.numeric(quantile(Z, 0.98)),
         green_75p = mean(G[Z >= ht_75p]),
         green_90p = mean(G[Z >= ht_90p]),
         green_98p = mean(G[Z >= ht_98p]),
         red_75p = mean(R[Z >= ht_75p]),
         red_90p = mean(R[Z >= ht_90p]),
         red_98p = mean(R[Z >= ht_98p]),
         blue_75p = mean(B[Z >= ht_75p ]),
         blue_90p = mean(B[Z >= ht_90p]),
         blue_98p = mean(B[Z >= ht_98p]),
         greenness_75p = mean(G[Z >= ht_75p]/(R[Z >= ht_75p]+B[Z >= ht_75p])),
         greenness_90p = mean(G[Z >= ht_90p]/(R[Z >= ht_90p]+B[Z >= ht_90p])),
         greenness_98p = mean(G[Z >= ht_98p]/(R[Z >= ht_98p]+B[Z >= ht_98p])),
         redness_75p = mean(R[Z >= ht_75p]/(G[Z >= ht_75p]+B[Z >= ht_75p])),
         redness_90p = mean(R[Z >= ht_90p]/(G[Z >= ht_90p]+B[Z >= ht_90p])),
         redness_98p = mean(R[Z >= ht_98p]/(G[Z >= ht_98p]+B[Z >= ht_98p])),
         blueness_75p = mean(B[Z >= ht_75p]/(G[Z >= ht_75p]+R[Z >= ht_75p])),
         blueness_90p = mean(B[Z >= ht_90p]/(G[Z >= ht_90p]+R[Z >= ht_90p])),
         blueness_98p = mean(B[Z >= ht_98p]/(G[Z >= ht_98p]+R[Z >= ht_98p])),
         med_height = median(Z),
         blueness_mean = mean((B - G)/(B + G)),
         greenness_mean = mean((G - R)/(G + R)),
         redness_mean = mean((R - B)/(R + B)),
         blueness_std = sd((B - G)/(B + G)),
         greenness_std = sd((G - R)/(G + R)),
         redness_std = sd((R - B)/(R + B)),
         blueness_med = median((B - G)/(B + G)),
         greenness_med = median((G - R)/(G + R)),
         redness_med = median((R - B)/(R + B)),
         blueness_skw = skewness((B - G)/(B + G)),
         greenness_skw = skewness((G - R)/(G + R)),
         redness_skw = skewness((R - B)/(R + B)),
         #find overall brightness statistics
         brightness_mean = mean(B + G + R),
         brightness_med = median(B + G + R),
         brightness_std = sd(B + G + R),
         brightness_skw = skewness(B + G + R),
         #find normalized statistics
         red_norm_mean = mean(R / (R + G + B)),
         blue_norm_mean = mean(B / (R + G + B)),
         green_norm_mean = mean(G / (R + G + B)),
         R = mean(R),
         G = mean(G),
         B = mean(B),
         R_ratio = R/(R + G + B ),
         G_ratio = G/(R + G + B ),
         B_ratio = B/(R + G + B ),
         G_R_ratio = G/R,
         G_R_ratio_2 = (G - R)/(G + R))%>% 
  dplyr::select(Id, treeid, status, region, R:G_R_ratio_2)

##################################################################################
# Quantile testing
test <- raw_df %>%
  filter(treeid == "HWY5_113G") # HWY5_113G is 36386.8, EH_117G is 40031.14

mean(test$G[test$Z>=quantile(test$Z, 0.75) & test$Z<quantile(test$Z, 0.90)]) # 36386.8,

# OR

test <- test %>%
  filter(Z>=quantile(Z, 0.75) & Z<quantile(Z, 0.90))

mean(test$G) # 36386.8

######################################################################################

model_df <- model_df  %>% 
  mutate(status = as.factor(status),
         region = as.factor(region),
         brightness_med = as.numeric(brightness_med))

# Transform to one row per tree
model_df <- model_df %>% group_by(treeid) %>% 
  filter(row_number()==1) %>%
  as.data.frame() %>%
  dplyr::select(-c(treeid, Id, region))

str(model_df)
summary(model_df)

######### Variable selection ###################################

## Best subset selection
# install.packages("leaps")

library(leaps)

2^45 (combination)

# reg.fit = regsubsets(status ~ ., data=model_df, nvmax=45)
# summary(reg.fit)

# plot( summary(reg.fit)$cp, main="Cp"); lines( summary(reg.fit)$cp )
# plot( summary(reg.fit)$bic, main="BIC"); lines( summary(reg.fit)$bic )
# plot( summary(reg.fit)$adjr2, main="ADJUSTED R-SQUARE"); lines( summary(reg.fit)$adjr2 )

# which.min( summary(reg.fit)$cp ) # 9 , 5 - 9
# which.min( summary(reg.fit)$bic ) # 4 , 3 - 10
# which.max( summary(reg.fit)$adjr2 ) # 16,  9 - 16 

# Region north, G, B, ht_mean, ht_75, greenness_mean, greenness_std, R_ratio
################################################################################################


# Forward and Backward

  
reg.fit.fwd <- regsubsets(status~.,data = model_df,nvmax=45,method = "forward")

summary(reg.fit.fwd)
par(mfrow=c(2,2))
plot(reg.fit.fwd)  # region north, G, ht_75p
plot(reg.fit.fwd, scale = "adjr2") # 16


# Backward:

reg.fit.bwd <- regsubsets(status~.,data = model_df,nvmax=45, method = "backward")

summary(reg.fit.bwd)
par(mfrow=c(2,2))
plot(reg.fit.bwd) # region north, G, height
plot(reg.fit.bwd, scale = "adjr2") # 15

# Running correlation between variables to check for any multicollinearity: 
corr <- as.data.frame(model_df)

corr <- as.data.frame(model_df) %>%
  dplyr::select(-c(region,status),R:G_R_ratio_2) %>% # loading dplyr because raster package causing issues
  cor()



corrplot(cor(corr), method="color", type = "upper", tl.col="black",tl.srt=40, addCoef.col = "gray8", diag = T, number.cex = 0.65)  


## Ridge and Lasso
# Ridge regression shrinks the coeﬃcients by imposing a penalty on their size. By penalizing the RSS we try to avoid that highly correlated regressors cancel each other.
# The lasso is a shrinkage method like ridge, but the L1 norm rather than the L2 norm is used in the constraints.

library(glmnet)
X = model.matrix(status~.,data=model_df)
Y = model_df$status
lasso = glmnet(X,Y, alpha =1, family = c("multinomial"), type.multinomial = "grouped")
plot(lasso)


# Perform 10 fold validation and find the optimum lambda
set.seed(1)
cv.lasso = cv.glmnet(X,Y,alpha=1,family = c("multinomial"), nfolds = 10, type.multinomial = "grouped") # What does lambda do?

plot(cv.lasso)

cv.lasso$lambda.min

# Perform validation set approach to compute test MSE
attach(model_df)
n= length(Y)

Z = sample(n,n/2)

lasso = glmnet(X[Z,],Y[Z], alpha=1, family = "multinomial",lambda = seq(0,10,0.01))
Yhat = predict(lasso,cv.lasso$lambda.min, newx=X[-Z,])

mean((Yhat - status[-Z])^2)

mean(Yhat == status[-Z]) 


## Final Chosen dataset variables for classification:

final_df <-model_df %>%
  dplyr::select(status, R,G,ht_90p, ht_75p,R_ratio)

attach(final_df)
# Logistic regression:

set.seed(1)
n= length(final_df$status)

Z = sample(1:nrow(final_df), 0.7*nrow(final_df))
glm.fit <- glm(status ~ ., data=final_df[Z,],family=poisson) # would it be possion?

summary(glm.fit)

# table
Probability = predict(glm.fit,final_df[-Z,], type="response")
Predicted.Direction = rep("0",length(Probability))
Predicted.Direction[ Probability > 0.5 ] = "1"

table( status[-Z], Predicted.Direction )

mean( status[-Z] == Predicted.Direction )


##### Model1 ###################
#classification tree
# install.packages("tree")

## Model_df### test####
library(tree)
class.tree <- tree(status ~., data=model_df, mindev=0.005)

summary(class.tree)

plot(class.tree,type ="uniform")
text(class.tree, pretty=0)


set.seed(1)

n= length(status)

Z = sample(1:nrow(model_df), 0.7*nrow(model_df))

train.tree = tree(status ~ .,data=model_df[Z,])

status.predict = predict(train.tree,model_df, type = "class")
table(status.predict[-Z], status[-Z])
mean(status.predict[-Z]!=status[-Z])



class.tree <- tree(status ~., data=model_df, mindev=0.005)

summary(class.tree)

plot(class.tree,type ="uniform")
text(class.tree, pretty=0)


# final_df####

library(tree)
class.tree <- tree(status ~., data=final_df, mindev=0.005)

summary(class.tree)

plot(class.tree,type ="uniform")
text(class.tree, pretty=0)


# estimate the correct classification rate by cross validation. 

set.seed(111)

n= length(status)

Z = sample(1:nrow(final_df), 0.7*nrow(final_df))

train.tree = tree(status ~ .,data=final_df[Z,])

status.predict = predict(train.tree,final_df, type = "class")
table(status.predict[-Z], status[-Z])
mean(status.predict[-Z]!=status[-Z])


# Using cross validation to determine the optimal complexity of a tree and the number of terminal nodes that minimizes the deviance. 

cv = cv.tree(train.tree)
cv
plot(cv)

# instead of optimizing by the smallest deviance, optimize the complexity and the number of terminal nodes by the smallest mis-classification error

cv = cv.tree(train.tree, FUN = prune.misclass)
cv
plot(cv)

#prune the tree to the optimal size which is 3 obtained above

tree.pruned = prune.misclass(train.tree, best = 3)

plot(tree.pruned)
text(tree.pruned, pretty=0)
