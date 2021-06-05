##########################
# Exercise 5 - template
# 5_1 Preparation of training and validation data:
#    - filters polygons >= 3 ha 
#    - adds Polygons for other classes
#    - samples 4 polygons per "LUSE_ID" 
#    - writes shapefiles for sample data
#    - creates plots for checking training and validation data
#
# 5_2 Classification of the NDVI time series using the training and validation samples:
#    - peforms classification using superClass
#    - prepares plots to explore results (e.g. variable importance, classification result)
#   -  export classification result
#
# Date May 2021
# Author Anja Klisch <anja.klisch@boku.ac.at>
# GPL >= 3
##########################

# missing code is indicated by ???

library(sf)
library(tidyverse)
library(terra)
# install.packages("caret","RStoolbox") # install if necessary
library(caret)
library(RStoolbox) 

# adjust to you own path !!!
inPath <- "/home/philipp/Documents/boku/remote_sensing_time_series/04_exercise/exerc4_QGISdata/HLSv14/33UVP/2018/HLS30/"
outPath <- "/home/philipp/Documents/boku/remote_sensing_time_series/05_exercise/"

# adjust the lambda value you have chosen
lambda <- 500 # 100 500 1000 3000 9999
inFile <- paste0("HLS.T33UVP.v1.4.NDVIcropped_smooth",sprintf("%04d",lambda),".tif")

##########################
# Exercise 5_1 
# Preparation of training and validation data
##########################

#######################
# a) create training and validation data 
################

# read shapefile with all IACS polygons
roi1 <- st_read("/home/philipp/Documents/boku/remote_sensing_time_series/04_exercise/exerc4_QGISdata/shape/invekos2018_32633_roi1.shp")

# test number of polygons per "LUSE_ID" (count includes grouping by the column)
roi1 %>% filter(SL_FLAECHE >= 3) %>% count(LUSE_ID,LUSE,LUSE1_ID,LUSE1) %>% print(n=20)
# or View()

# filter for polygons with area >= 3 ha and randomly choose 4 polygons per "LUSE_ID" 
# set a seed to provide reproducibility
# slice_sample: n number of rows to select - if data frame is grouped, n applies to each group
set.seed(10555) 
samples <- roi1 %>% filter(SL_FLAECHE >= 3) %>% group_by(LUSE_ID) %>% slice_sample(n=4)


# read polygons of other classes (e.g. water, urban, conif forest, decid forest)
samples_other <- "/home/philipp/Documents/boku/remote_sensing_time_series/05_exercise/exerc5_QGISdata/shape/samples_other.shp"
samples_other %>% count(LUSE_ID,LUSE,LUSE1_ID,LUSE1)

# add polygons of other classes to training samples - both data frames must have same columns!
# order by column "LUSE1_ID" using arrange()
samples <- bind_rows(samples, samples_other) %>% arrange(by=LUSE_ID)
samples %>% count(LUSE_ID,LUSE,LUSE1_ID,LUSE1) %>% print(n=30)

# split into 1/2 training and 1/2 validation
set.seed(42) 
trainIndex <- as.vector(createDataPartition(factor(samples$LUSE_ID), p=0.5, list = FALSE))

# write shapefile for training samples
st_write(samples[trainIndex,],
         dsn=paste0(outPath,"shape/","samples2018_roi1_train.shp"),delete_dsn = TRUE)

# write shapefile for validation samples: remaining polygons
st_write(samples[-trainIndex,],
         dsn=paste0(outPath,"shape/","samples2018_roi1_val.shp"),delete_dsn = TRUE)


#######################
# b) check training and validation data 
################

# create SpatRaster with smoothed NDVI data and prepare for extracting 
img <- terra::rast("/home/philipp/Documents/boku/remote_sensing_time_series/04_exercise/exerc4_QGISdata/HLSv14/33UVP/2018/HLS30/HLS.T33UVP.v1.4.NDVIcropped_smooth0500.tif")
NAflag(img) <- -3000
names(img) <- 1:nlyr(img)

# create SpatVector with samples 
samples_t <- terra::vect(samples)

# extract smoothed NDVI data for polygons and calculate NDVI means per polygon 
samples_ndvi <- terra::extract(img, samples_t, fun=mean, na.rm=TRUE)

# convert to tibble and add the following columns/variables:
# - LUSE, LUSE1 from samples
# - a new column "type“ containing a flag for "training“ or “validation“  
samples_ndvi <-  samples_ndvi %>%  
  as_tibble() %>%
  mutate(LUSE=samples_t$LUSE,
         LUSE1=samples_t$LUSE1,
         type="validation")
samples_ndvi$type[trainIndex] <- "training"
samples_ndvi
samples_ndvi$week <- samples_ndvi$week %>% str_sub(start=2, end=3) %>% as.integer()

# reshape the data with target of having:
# - the columns: "ID", "LUSE“, "LUSE1“, "type"
# - one column with the weeks (type integer)
# - one column with the ndvi values
samples_ndvi <- samples_ndvi %>%
  pivot_longer(cols=`1`:`53`,
               names_to="week",
               names_transform = list(week = as.integer),
               values_to="ndvi")


# define colors for the classes
colors <- c("darkgreen","forestgreen","darkolivegreen2", "gold","red", "dodgerblue","gold4")

# plot the NDVI profiles of the sample polygons and colorise by class (LUSE1) 
# create a matrix of panels e.g. columns->type and rows->LUSE1
ggplot(data = samples_ndvi,mapping = aes(x=week, y=ndvi, colour=LUSE1)) + 
  geom_line(size=0.6,aes(group = ID)) +
  scale_colour_manual(values=colors)+
  #scale_colour_brewer(palette="Paired") +
  ggtitle("Samples") + 
  facet_grid(rows=vars(LUSE1),cols=vars(type))

# save the plot in the working directory
ggsave(paste0(outPath,"exerc51_samples",sprintf("%04d",lambda),".png"), width = 9, height=6, dpi = 300)


#######################
# visually check in QGIS ...
################


##########################
# Exercise 5_2
# Classification of the NDVI time series using the training and validation samples
##########################

#######################
# a) prepare input data and perform classification
# http://bleutner.github.io/RStoolbox/rstbx-docu/superClass.html
################

# read smoothed NDVI data, define NA and names
img <- raster::brick(paste0("/home/philipp/Documents/boku/remote_sensing_time_series/04_exercise/exerc4_QGISdata/HLSv14/33UVP/2018/HLS30/", inFile))
raster::NAvalue(img) <- -3000
names(img) <- paste0("W",1:53)

# coercion to SpatialPolygonsDataFrame (sp package) necessary -> as(x, "Spatial")
randForest <- superClass(img, 
                         trainData = as(samples[trainIndex,], "Spatial"),
                         valData = as(samples[-trainIndex,], "Spatial"),
                         responseCol = "LUSE1",
                         polygonBasedCV = TRUE, 
                         model = "rf",
                         verbose = TRUE,
                         tuneLength = 6)


#######################
# b) prepare plots to explore results
################

# to find out about variable importance (1...53) from package caret
varimp_rf <- varImp(randForest$model)

# plot variable importance
plot(varimp_rf, main="Variable Importance for Random Forest")


library(tmap)

m<-tm_shape(randForest$map) +
  tm_raster(style="cat",palette = colors, colorNA="grey", title="Land cover/land use",legend.hist = TRUE) +
  tm_legend(outside = TRUE, hist.width = 2)
tmap_save(m, paste0(outPath,"exerc52_map",sprintf("%04d",lambda),".png"), height = 5,width = 7)

tmap_mode("view") # interactive map
tm_shape(randForest$map) +
  tm_raster(style="cat",palette = colors, colorNA="grey", title="Classifiaction result") + 
  tm_shape(samples) +
  tm_borders("white", lwd = .5) +
  tm_fill("white", alpha = .5)

tmap_mode("plot") # static map


#######################
# write classification result to geotiff
################

# what are the classIDs in our classification result? -> need to be reclassified
randForest$classMapping

# create a reclass tibble that contains just unique values for LUSE1 and LUSE1_ID
rcl <- samples %>% 
  st_drop_geometry %>% 
  distinct(LUSE1,LUSE1_ID)

# assign LUSE1_ID to class mapping
rcl <- left_join(randForest$classMapping, rcl,by=c("class" = "LUSE1"))

# use classify from terra package to reclassify and
# write classification result to Geotiff
reclassMap <- classify(rast(randForest$map), rcl[,-2],
                       filename=paste0(outPath,"classRF_2018.tif"),
                       overwrite=TRUE, 
                       wopt=list(gdal=c("COMPRESS=LZW","PREDICTOR=1"),
                                 NAflag=255, datatype="INT1U"))


#######################
# Zonal stats: assign classes from classification result
################

# use modal value to extract random forest class for all IACS polygons
rf_id <- terra::extract(reclassMap, vect(roi1), fun=modal, na.rm=TRUE)
head(rf_id)

# assign random forest class to IACS data 
roi1$RF_ID <- rf_id[,2]

# write new shapefile
st_write(roi1,
         dsn=paste0(outPath,"shape/","invekos2018_32633_roi1_RF",sprintf("%04d",lambda),".shp"),delete_dsn = TRUE)

#######################
# further analysis in QGIS
################




