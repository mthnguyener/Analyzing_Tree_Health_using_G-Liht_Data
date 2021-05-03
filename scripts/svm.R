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
         region = as.factor(region),
         brightness_med = as.numeric(brightness_med))

model_df <-model_df %>% group_by(treeid) %>% 
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
# Preparing Datasets ----
set.seed(1)

trctrl <- trainControl(method = "CV", number = 10)

# Linear
svm_Linear <- train(status ~., data = train_set, method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)

svm_Linear # 0.7566667 ACC and 0.5128205 Kappa

test_pred <- predict(svm_Linear, newdata = test_set)
test_pred

confusionMatrix(table(test_pred, test_set$status)) # 0.7917 ACC and 0.5833 Kappa

# Linear Grid
set.seed(1)

grid <- expand.grid(C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))
svm_Linear_Grid <- train(status ~., data = train_set, method = "svmLinear",
                         trControl=trctrl,
                         preProcess = c("center", "scale"),
                         tuneGrid = grid,
                         tuneLength = 10)
svm_Linear_Grid
plot(svm_Linear_Grid)

test_pred_grid <- predict(svm_Linear_Grid, newdata = test_set)
test_pred_grid

confusionMatrix(table(test_pred_grid, test_set$status)) # 0.6786

# Radial
set.seed(1)

svm_Radial <- train(status ~., data = train_set, method = "svmRadial",
                    trControl = trctrl,
                    preProcess = c("center","scale"),
                    tuneLength = 10)
svm_Radial

svm_Radial$bestTune # C=0.5 and Accuracy=0.8366667

test_pred_radial <- predict(svm_Radial, newdata = test_set)
test_pred_radial

confusionMatrix(table(test_pred_radial, test_set$status)) # 0.6429

# Radial Grid
set.seed(1)

grid_radial <- expand.grid(sigma = c(0,0.01, 0.02, 0.025, 0.03, 0.04,
                                       0.05, 0.06, 0.07,0.08, 0.09, 0.1, 0.25, 0.5, 0.75,0.9),
                             C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75,
                                   1, 1.5, 2,5))
svm_Radial_Grid <- train(status ~., data = train_set, method = "svmRadial",
                           trControl=trctrl,
                           preProcess = c("center", "scale"),
                           tuneGrid = grid_radial,
                           tuneLength = 10)
svm_Radial_Grid

plot(svm_Radial_Grid)

test_pred_Radial_Grid <- predict(svm_Radial_Grid, newdata = test_set)
test_pred_Radial_Grid

confusionMatrix(test_pred_Radial_Grid, test_set$status) # 0.6786
