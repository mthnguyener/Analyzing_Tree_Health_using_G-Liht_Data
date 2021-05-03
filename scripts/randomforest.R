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
         status = as.factor(status)) # add raw DF for preprocesisng/analyzing
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
# Processing model data frame ----

model_df <- read.csv("../../STV/code/R/AliyaMinh/data/model_df.csv")  %>% 
  mutate(status = as.factor(status),
         brightness_med = as.numeric(brightness_med))

model_df <- model_df %>% group_by(treeid) %>% 
  filter(row_number()==1) %>%
  as.data.frame() %>%
  dplyr::select(-c(treeid, Id, region))

str(model_df)

###################################################################################
# Preparing Datasets ----

set.seed(1)

training <- sample(1:nrow(model_df), 0.7*nrow(model_df))
train_set <- model_df[training,]
test_set <- model_df[-training,]

###################################################################################
# Random Forest ----

# topepo.github.io/caret/measuring-performance.html 

set.seed(1)
bestMtry <- tuneRF(model_df[,2:46],model_df[,1], stepFactor = 1.5, improve = 1e-5, ntree = 500) #best mtry = 9 with 29.93% OOB error

# default RF model
set.seed(1)

model1 <- randomForest(status ~ . , data=train_set, importance=T)
model1 # 31.19%

model2 <- randomForest(status ~ . , data = train_set, ntree = 500, mtry = 9, importance = T)
model2 # 31.19%

model3 <- randomForest(status ~ . , data = train_set, ntree = 500, mtry = 18, importance = T)
model3 # 32.11%

model4 <- randomForest(status ~ . , data = train_set, ntree = 500, mtry = 24, importance = T)
model4 # 28.44% (best)

model5 <- randomForest(status ~ . , data = train_set, ntree = 500, mtry = 25, importance = T)
model5 # 32.11%

model6 <- randomForest(status ~ . , data = train_set, ntree = 500, mtry = 30, importance = T)
model6 # 32.11%

# Predicting on Validation set
predValid <- predict(model4, validation_set, type = "class")
# Checking classification accuracy
mean(predValid == validation_set$status) # 0.7142857
table(predValid,validation_set$status)

# To check important variables
importance(model4)
varImpPlot(model4)

plot(model4)

a=c()
i=0

for (i in 1:48) {
  set.seed(1)
  rf <- randomForest(status ~ ., data=train_set, ntree=500, mtry=i, importance=T)
  predValid <- predict(rf, validation_set, type = "class")
  a[i] <- mean(predValid == validation_set$status)
}

a # 6 variables considered each split of the tree - 0.7142857

###################################################################################
# Caret Package ----

# All 53 Variables

set.seed(1)

# Define the control
trControl <- trainControl(method = "cv", number = 10, search = "grid")

# Run default model
rf_default <- train(status ~., data=train_set, method="rf", metric="Accuracy", trControl=trControl)
print(rf_default)

# Search best mtry
set.seed(1)
tuneGrid <- expand.grid(.mtry = c(1:53))
rf_mtry <- train(status ~., data=train_set, method="rf", metric="Accuracy", tuneGrid=tuneGrid, trControl=trControl,
                 importance=TRUE, ntree=500)

print(rf_mtry)

c("best_mtry" = rf_mtry$bestTune$mtry, "Accuracy" = max(rf_mtry$results$Accuracy))

best_mtry <- rf_mtry$bestTune$mtry # 16 with accuracy of 0.8666667

# Search the best maxnodes
store_maxnode <- list()
tuneGrid <- expand.grid(.mtry = best_mtry)

for (maxnodes in c(5:15)) {
  set.seed(1)
  rf_maxnode <- train(status~.,
                      data = train_set,
                      method = "rf",
                      metric = "Accuracy",
                      tuneGrid = tuneGrid,
                      trControl = trControl,
                      importance = TRUE,
                      maxnodes = maxnodes,
                      ntree = 500)
  current_iteration <- toString(maxnodes)
  store_maxnode[[current_iteration]] <- rf_maxnode
}
results_mtry <- resamples(store_maxnode)
summary(results_mtry)

# Search for best ntrees
store_maxtrees <- list()

for (ntree in c(250, 300, 350, 400, 450, 500, 550, 600, 800, 1000, 2000)) {
  set.seed(1)
  rf_maxtrees <- train(status~.,
                       data = train_set,
                       method = "rf",
                       metric = "Accuracy",
                       tuneGrid = tuneGrid,
                       trControl = trControl,
                       importance = TRUE,
                       maxnodes = 5,
                       ntree = ntree)
  key <- toString(ntree)
  store_maxtrees[[key]] <- rf_maxtrees
}
results_tree <- resamples(store_maxtrees)
summary(results_tree)

# Final model
fit_rf <- train(status~.,
                train_set,
                method = "rf",
                metric = "Accuracy",
                tuneGrid = tuneGrid,
                trControl = trControl,
                importance = TRUE,
                ntree = 250,
                maxnodes = 5)

# Evaluate the model
prediction <- predict(fit_rf, test_set)

confusionMatrix(prediction, test_set$status) # 0.9167 Accuracy and 0.7983 Kappa

var_imp <- varImp(fit_rf)
var_imp # Top 5 variables need to be explained

plot(var_imp) # all

ggplot(var_imp, mapping = NULL)

# Test set assessment #2: ROC curves and AUC

# Needs to import ROCR package for ROC curve plotting:
library(ROCR)

# Calculate the probability of new observations belonging to each class
# prediction_for_roc_curve will be a matrix with dimensions data_set_size x number_of_classes
prediction_for_roc_curve <- predict(fit_rf, test_set[,-1],type="prob")

# Use pretty colours:
pretty_colours <- c("#F8766D","#00BA38","#619CFF")
# Specify the different classes 
classes <- levels(test_set$status)
# For each class
for (i in 1:2) {
  # Define which observations belong to class[i]
  true_values <- ifelse(test_set[,1]==classes[i],1,0)
  # Assess the performance of classifier for class[i]
  pred <- prediction(prediction_for_roc_curve[,i],true_values)
  perf <- performance(pred, "tpr", "fpr")
  if (i==1) {
    plot(perf,main="ROC Curve",col=pretty_colours[i]) 
  }
  else {
    plot(perf,main="ROC Curve",col=pretty_colours[i],add=TRUE) 
  }
  # Calculate the AUC and print it to screen
  auc.perf <- performance(pred, measure = "auc")
  print(auc.perf@y.values)
}
