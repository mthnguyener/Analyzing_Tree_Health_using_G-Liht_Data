###################################################################################
# Directories ----

# User should not have to commonly change anything beyond this code block

# set working directory and get data
# gliht SfM projects root dir

# Aliya
sfm_dir <- "F:/Users/aa0856a/ENVS_RS Dropbox/Projects/pix4d/201807XX_Denali_FHM_06/"

# Minh
sfm_dir <- "F:/mnguyen/ENVS_RS Dropbox/Projects/pix4d/201807XX_Denali_FHM_06/"

# Set working directory
setwd(sfm_dir); getwd()


#Read in hand drawn crowns shapefile for cal/val
fhm_crowns_shp <- readOGR("../../Alaska/fhm_2018_uav/ak_spruce_crowns.shp")

# NOT NEEDED IF SHAPE FILE HAS id - add in unique id
fhm_crowns_shp@data$Id <- 1:length(fhm_crowns_shp@data$Id)

# Check shape file
fhm_crowns_shp

# paths to normalized SfM point clouds
# F:/ENVS_RS Dropbox/Projects/STV/project_data/point_clouds
# assign each path to sfm_norm_path

# paths to project orthoimagery 


###################################################################################
# EXTRACT point cloud data from P4D SfM output data ----

# AUTO READ all .las files and concatenate into one vector
sfm_all <- list.files(path = "../../STV/project_data/point_clouds/",
                      pattern = "las$", full.names = TRUE) %>% c()

# Import LAS file (using the paths specified above)
for (i in seq_along(sfm_all)) {
  if(i==1) {
    start_time <- Sys.time()
    las_all <- list()
  }
  
  las_all[[str_replace_all(str_match(sfm_all[i], "\\_\\w*\\_"),"\\_|sfm", "")]] <- lidR::readLAS(sfm_all[i]) %>% 
    add_attribute(str_replace_all(str_match(sfm_all[i], "\\_\\w*\\_"),"\\_|sfm", ""), "region")
 
  if(i==length(sfm_all)) {
    las_all <- do.call(rbind,las_all)
    end_time <- Sys.time()
    print(paste0("Processing Time: ", end_time - start_time)) # 26.8120880126953 secs
  } 
  
}

las_all

unique(las_all@data$region)
head(las_all)
plot(las_all)


############################################################################################
# (NOT NEEDED since we all las files were merged into one) ----
# loop will take awhile to finish (RECOMMENDED - USE THE ABOVE METHOD)
# las_all MUST BE A LIST OF MULTPLE LAS FILES
for (i in seq_along(las_all)) {
  if(i==1) {
    start_time <- Sys.time()
    
    crs_list <- list() #las_crs
    reproj_list <- list() #fhm_crowns_reproj
    extent_list <- list() #las_extent
    sub_list <- list() #fhm_crowns_shp_sub
    crowns_list <- list() #las_crowns
  }
  
  crs_list[[i]] <- crs(las_all[[i]], asText = FALSE) #las_crs
  reproj_list[[i]] <- spTransform(fhm_crowns_shp,crs_list[[i]]) #fhm_crowns_repoj
  extent_list[[i]] <- extent(las_all[[i]]) # las_extent
  sub_list[[i]] <- crop(reproj_list[[i]], extent_list[[i]]) #fhm_crowns_shp_sub
  crowns_list[[i]] <- merge_spatial(las_all[[i]], sub_list[[i]], "treeid") #las_crowns
  crowns_list[[i]] <- filter_poi(crowns_list[[i]], treeid != "NA") #las_crowns without "NA"
  
  if(i==length(sfm_all)) {
    end_time <- Sys.time()
    print(paste0("Processing Time: ", end_time - start_time)) #16.3202919960022 secs
  } 
  
}

###################################################################################
# REPROJECT the shapefile to match all las files - we need this  ----

# Created get_las_crowns function (first argument = las file, second argument = shape file)
get_las_crowns <- function(las, shape) {
  
  start_time <- Sys.time()
  
  las_crs <- crs(las, asText = FALSE)
  fhm_crowns_reproj <- spTransform(shape,las_crs)
  las_extent <- extent(las)
  fhm_crowns_shp_sub <- crop(fhm_crowns_reproj, las_extent)
  
  # for points that land within each polygon, add the "treeid" attribute from the shapefile to the las dataset
  all_las_crowns <- merge_spatial(las, fhm_crowns_shp_sub, "treeid") # filter such that only points within the polygons remains
  all_las_crowns <- merge_spatial(all_las_crowns, fhm_crowns_shp_sub, "Id") # add in id - comment out to reduce time
  
  all_las_crowns <- filter_poi(all_las_crowns, treeid != "NA") # filter out NA treeid
  
  all_las_crowns <<- add_lasattribute(all_las_crowns, all_las_crowns@data$Id, "Id", "Shape ID#") # add Id as a las attribute into the las file - "extra bytes"
  
  end_time <- Sys.time()
  time <- paste0("Processing Time: ", end_time - start_time)
  output <- list(time, all_las_crowns)
  
  return(output)
}

get_las_crowns(las_all, fhm_crowns_shp) # 7.99595518509547 mins (LAS dimension: 233153 rows x23 columns)

# To check individual region
# selected_region <- filter_poi(all_las_crowns, region == "south3") #select a region for basic calculations
# length(unique(selected_region@data$treeid)) #137 (36 north, 36 mid, 16 south1, 18 south2, 31 south3)

###################################################################################
# CHECK missing data ----

# when we reproject the shape file with crs, all the points without SfM, dropped/did not recognized 
# here, we created a dataframe to find out which ones are missing to confirm our theory
crowns <- as.data.frame(unique(all_las_crowns@data$treeid)) %>%
  mutate(numbers = 1) %>%
  rename(tree_id = 1)
crowns

trees <- as.data.frame(fhm_crowns_shp@data$treeid) %>%
  mutate(numbers = 0) %>%
  rename(tree_id = 1)
trees

missing_trees <- trees %>% left_join(crowns, by="tree_id") %>%
  rename("confirmed"="numbers.y") %>%
  mutate(confirmed = replace_na(confirmed, 0)) %>%
  dplyr::select(-numbers.x)
missing_trees

###################################################################################
# WRITE FILES ----

raw_df <- as.data.frame(all_las_crowns@data) %>%
  mutate(status = str_replace(toupper(treeid),"^\\w+[\\_|\\-]\\d+","")) %>%
  dplyr::select(-c(gpstime:PointSourceID))
view(raw_df)

summary(raw_df)

id_df <- raw_df %>% group_by(treeid) %>% 
  filter(row_number()==1) %>%
  dplyr::select(region:status)
view(id_df) # 137 rows

getwd()
write.csv(id_df, "../../STV/code/R/AliyaMinh/data/id_df.csv", row.names=FALSE)
write.csv(missing_trees, "../../STV/code/R/AliyaMinh/data/missing_df.csv", row.names=FALSE)
write.csv(raw_df, "../../STV/code/R/AliyaMinh/data/raw_df.csv", row.names=FALSE)
writeLAS(all_las_crowns, "../../STV/code/R/AliyaMinh/data/all_las_crowns.las", index = FALSE)
