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

# Read in hand drawn crowns shapefile for cal/val
fhm_crowns_shp <- readOGR("../../Alaska/fhm_2018_uav/ak_spruce_crowns.shp")
#fhm_crowns_shp@data$Id <- 1:length(fhm_crowns_shp@data$Id) # add in unique id (not really needed in descriptive)
fhm_crowns_shp

# Read in LAS file
all_las_crowns <- readLAS("../../STV/code/R/AliyaMinh/data/all_las_crowns.las")

# Not needed unless you want to process the las files again and re-add treeid (already in the raw_df)
all_las_crowns <- merge_spatial(all_las_crowns, fhm_crowns_shp, "treeid") # filter such that only points within the polygons remains

###################################################################################
# PLOT tree crowns in point clouds 3D ----

#plot out cal/val tree crowns
plot(all_las_crowns, color ="RGB")
# plot a single crown for examplea 
# first get the unique treeids
unique(all_las_crowns@data$treeid)
tree_A <- filter_poi(all_las_crowns, treeid == "HWY5_112A")
tree_D <- filter_poi(all_las_crowns, treeid == "HWY4_64D")
plot(tree_A, size = 8, bg = "white")
plot(tree_D, size = 8, bg = "white")

# plot vertical structure of this tree
dev.new(); hist(tree@data$Z)

# plot greeness vs. height as example of a potential variable of interest
par(mfrow=c(2,1))
plot(tree_A@data$Z, tree_A@data$G)
plot(tree_A@data$Z,tree_A@data$R)

par(mfrow=c(2,1))
plot(tree_D@data$Z, tree_D@data$G)
plot(tree_D@data$Z,tree_D@data$R)

###################################################################################
# Descriptive Analytics----

# Missing Trees
missing_trees <- missing_df %>% filter(confirmed == 0)
missing_trees[1] # HWY3_53D, HWY3_52D, HWY2_30A, HWY2_29D, HWY2_28G, HWY4_62A, HWY5_103A, HWY5_100A, HWY5_101A, HWY5_102A, HWY5_99A, HWY5_97A, HWY5_98G, HWY5_90A, HWY5_92G

# Summary
summary(raw_df) # summary of whole df

# Height of points
summary(raw_df$Z)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -1.340   6.243  10.760  10.331  14.416  29.496 

# Colors
summary(raw_df$R)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -1.340   6.243  10.760  10.331  14.416  29.496 

summary(raw_df$G)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 7936   22784   32000   31953   40704   64512

summary(raw_df$B)
# in. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 512   16384   21760   22377   27904   55296 

# Region
summary(raw_df$region)
# mid  north south1 south2 south3 
# 55341  63311  58920  19611  35970 

# Status
summary(raw_df$status)
# A     D     G 
# 60340 73866 98947 

# trying to sum all the points for R and G that is z>4
tree@data %>% filter(Z>4) %>%
  rowSums(tree@data$G)

# Height of points
summary(raw_df$Z)

# this will plot the Z values and flip the histogram 90 degrees to make it more intuitive since we're examining elevations/heights
ggplot(data=raw_df, aes(x=Z)) + geom_histogram(color= "black", fill="transparent", alpha=0.5, position="identity", binwidth = 1) +  theme_minimal() + coord_flip() + xlim(-5,30)

# mean and median height of points in this las file also percentile (or quantile) heights. 
# For example, what is the Z value below which 90% of points exist ("pct90") in this study area?
raw_df_summary <- raw_df %>% summarise(mean=mean(Z), pct10=quantile(Z,.1), pct30=quantile(Z,.3),
                                       median=median(Z), pct75=quantile(Z,.75), pct90=quantile(Z,.90), pct98=quantile(Z,.98))

# histogram with pct10, mean, and pct90 plotted on top with green, red, and orange lines respectively
ggplot(data=raw_df, aes(x=Z)) + geom_histogram(color= "black", fill="transparent", alpha=0.5, position="identity", binwidth = 1) +  theme_minimal() + coord_flip() + 
  geom_vline(aes(xintercept = mean(Z)),col='red',size=1) + # <-- this line is at the mean height
  geom_text(aes(x=12, label="Med", y=-700), colour="red", text=element_text(size=10)) +
  geom_vline(aes(xintercept = quantile(Z,0.1)),col='green',size=1) + # <-- this line is at pct10 height
  geom_text(aes(x=3.5, label="Pct10", y=-700), colour="green", text=element_text(size=10)) +
  geom_vline(aes(xintercept = quantile(Z,0.75)),col='orange',size=1) + # <-- this line is at pct75 height
  geom_text(aes(x=16, label="Pct75", y=-700), colour="orange", text=element_text(size=10)) +
  geom_vline(aes(xintercept = quantile(Z,0.90)),col='blue',size=1) + # <-- this line is at pct98 height
  geom_text(aes(x=23, label="Pct90", y=-700), colour="blue", text=element_text(size=10))

#show all the quantile values calculated above
raw_df_summary

###################################################################################
# PLOT Variables ----

raw_df %>% 
  group_by(treeid) %>%
  ggplot(aes(Z)) + 
  geom_histogram() +
  facet_wrap(~treeid) # height by treeid

raw_df %>% 
  group_by(treeid) %>%
  ggplot(aes(Z)) + 
  geom_histogram() +
  facet_wrap(~status) # height by status

raw_df %>% 
  group_by(treeid) %>%
  filter(Z == quantile (Z, 0.75)) %>%
  ggplot(aes(Z)) + 
  geom_histogram() +
  facet_wrap(~status) # height by status in 75% quantile

raw_df %>% 
  group_by(treeid) %>%
  filter(Z == quantile (Z, 0.90)) %>%
  ggplot(aes(Z)) + 
  geom_histogram() +
  facet_wrap(~status) # height by status in 90% quantile

raw_df %>% 
  group_by(treeid) %>%
  filter(Z == quantile (Z, 0.98)) %>%
  ggplot(aes(Z)) + 
  geom_histogram() +
  facet_wrap(~status) # height by status in 98% quantile

raw_df %>% 
  group_by(treeid) %>%
  filter(Z == quantile (Z, 0.90)) %>%
  ggplot(aes(y = Z)) +
  geom_line(aes(x = R, color = "red")) +
  geom_line(aes(x = G, color = "green")) +
  geom_line(aes(x = B, color = "blue")) +
  scale_color_manual(values=c("blue", "green", "red")) +
  labs(x = "Color Scale", y = "Height") +
  facet_wrap(~status)# height by color and status

raw_df %>% 
  group_by(treeid) %>%
  ggplot(aes(y = Z)) +
  geom_line(aes(x = R, color = "red")) +
  geom_line(aes(x = G, color = "green")) +
  geom_line(aes(x = B, color = "blue")) +
  scale_color_manual(values=c("blue", "green", "red")) +
  facet_wrap(~status) # height by color and status

H62A <- raw_df %>%
  filter(treeid == "EH_152G") %>% 
  mutate(height = max(Z),
         ht_75p = quantile(Z,0.75),
         ht_90p = .9* height,
         ht_98p = .98 * height)

quantile(H62A$Z, 0.75)
summary(H62A$Z)
summary(H62A)

###################################################################################
# Features Extraction ----

# MUTATE Variables
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

######################################################################################
# Checking our final data frame

descriptives(model_df, vars = vars(status), freq = TRUE)

summary(model_df)

model_df %>%
  ggplot(aes(x = G, fill=status)) +
  geom_histogram() +
  scale_colour_manual(values = c("green", "red"), 
                      aesthetics = c("colour", "fill")) +
  labs(title="Total Green Count", x="Green")

model_df %>% 
  ggplot() +
  geom_histogram(aes(x = greenness_75p, fill = "orange")) +
  geom_histogram(aes(x = greenness_90p, fill = "blue")) +
  labs(title="Greenness at 75% and 90%", x="Greenness") +
  scale_fill_discrete(labels = c("75%", "90%")) +
  guides(fill=guide_legend(title="Quantile")) +
  facet_wrap(~status)

model_df %>%
  ggplot(aes(x = height, fill=status)) +
  geom_histogram() +
  scale_colour_manual(values = c("green", "red"), 
                      aesthetics = c("colour", "fill")) +
  labs(title="Height Comparison", x="Height")

model_df %>% 
  ggplot() +
  geom_histogram(aes(x = ht_75p, fill = "orange")) +
  geom_histogram(aes(x = ht_90p, fill = "blue")) +
  labs(title="Height at 75% and 90%", x="Height") +
  scale_fill_discrete(labels = c("75%", "90%")) +
  guides(fill=guide_legend(title="Quantile")) +
  facet_wrap(~status)

# Checking Multicolinearity

corr <- as.data.frame(model_df) %>%
  dplyr::select(-status) %>% # loading dplyr because raster package causing issues
  cor()

corrplot(cor(corr), method="color", type = "upper", tl.col="black",tl.srt=40, addCoef.col = "gray8", diag = T, number.cex = 0.65)  
