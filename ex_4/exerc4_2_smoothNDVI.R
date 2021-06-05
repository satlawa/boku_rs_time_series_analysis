##########################
# Exercise 4_2
# Processing of Harmonized Landsat Sentinel-2 (HLS) data:
#    - Excludes observations contaminated by cloud/cirrus according to Status Map (SM) data
#    - Finds pixel-specific winter NDVI and applies for winter period 
#    - Smooths and interpolates NDVI data
#    - Writes resulting 7-day NDVI images into GeoTiff
# Date May 2021
# Author Anja Klisch <anja.klisch@boku.ac.at>
# GPL >= 3
##########################

#######################
# settings 
################

# load libraries and functions
library(terra)
library(ptw)
library(tidyverse)
library(lubridate)

Sys.setlocale("LC_ALL","English")

# adjust to you own path !!!
source("d:/RSTSA2021/Exerc3/whittaker.R") # include local function

# define paths: adjust to you own path !!!
inPath <- "d:/RSTSA2021/Exerc4/HLSv14/33UVP/2018/"
outPath <- "d:/RSTSA2021/Exerc4/HLSv14/33UVP/2018/HLS30/"

pattern <- glob2rx("*.NDVI_cropped.tif")
noValue <- -3000
minValue <- -2000
maxValue <- 10000

startDay <- "2018-01-01"
endDay <- "2018-12-31"
period <- 7 

lambda <- 500 # 100 500 1000 3000 9999
outFile <- paste0("HLS.T33UVP.v1.4.NDVIcropped_smooth",sprintf("%04d",lambda),".tif")

#######################
# prepare input data 
################

# list all NDVI file names for Sentinel-2 and Landsat 8 including paths
files <- list.files(path=inPath, pattern=pattern, full.names=TRUE,recursive=TRUE)

# extract YEARDOY from name and convert to date
#dates <- as.Date(str_split(basename(files),"[.]",simplify=TRUE)[,4],format="%Y%j")
dates <- as.Date(str_sub(basename(files),16,22),format="%Y%j")

# remove duplicates
files <- files[!duplicated(dates)]
dates <- dates[!duplicated(dates)]

# sort file names and dates by date
files <- files[order(dates)]
dates <- dates[order(dates)]

# create SpatRast for all NDVI files and set NAflag
img <- rast(files)
NAflag(img) <- noValue 

# create SpatRast for all SM files
imgSM <- rast(sub("NDVI_cropped.tif","SM_cropped.tif",files))

# get values from both SpatRast (NDVI and SM): matrix 210681 x 184
val <- values(img)
valSM <- values(imgSM)

# exclude NDVI observations contaminated by cloud/cirrus according to the Status Map (assign NA)
val[valSM==1] <- NA 

rm(valSM) # remove status map matrix

#######################
# prepare output data
################

# define dates for smoothing
datesAll <- seq(ymd(startDay),ymd(endDay),by=1)

# define index for input
idxIn <- dates-datesAll[1]+1

# define index for winterNDVI
idxWinter <- which(!between(month(datesAll),3,11))

# define index for output
idxOut <- seq(1,length(datesAll),by=period)

# create output path
dir.create(outPath, showWarnings = FALSE)

# create empty matrix for smoothed NDVI 
result <- matrix(data=NA, nrow=nrow(val),ncol=length(idxOut))

#######################
# start processing
################

cat("Start processing ...", "\n")
start_time <- Sys.time()

# Loop for all cells/pixels of images
for (j in (1:nrow(val))) { # j<-1

  cat("Processing pixel:",j,"/",nrow(val), "\r") 
  
  # create empty vec
  vecAll <- rep(NA,(length(datesAll)))
  
  # fill observations
  vecAll[idxIn] <- val[j,]
  
  # fill winter NDVI as 10th percentile per pixel
  winterNDVI <- quantile(val[j,], probs = 0.05,names = FALSE,na.rm=TRUE)
  vecAll[idxWinter] <- winterNDVI
  
  ############ lambda test
  data <- tibble(date=datesAll,ndvi=vecAll)
  ggplot(data = data, mapping = aes(x=date)) +
    geom_point(mapping = aes(y=ndvi),col="black",size=0.7) +
    geom_line(col="red",
              aes(y=whittaker(ndvi, l=100, minval=winterNDVI, maxval=maxValue))) +
    geom_line(col="gold",
              aes(y=whittaker(ndvi, l=500, minval=winterNDVI, maxval=maxValue))) +
    geom_line(col="darkgreen",
              aes(y=whittaker(ndvi, l=1000, minval=winterNDVI, maxval=maxValue))) +
    geom_line(col="blue",
              aes(y=whittaker(ndvi, l=3000, minval=winterNDVI, maxval=maxValue)))
  ############
  
  # smooth with Whittaker
  smoothVec <- whittaker(vecAll, l=lambda, minval=minValue, maxval=maxValue)
  
  # add smoothed 7-day values to result matrix
  result[j,] <- smoothVec[idxOut] 
}

# create empty SpatRast from existing object img
smoothImg <- rast(img[[1:length(idxOut)]])

# assign values to SpatRast
smoothImg <- setValues(smoothImg, result)

# write SpatRast to file
writeRaster(smoothImg, filename=paste0(outPath,outFile),
            overwrite=TRUE,
            wopt=list(gdal=c("COMPRESS=LZW","PREDICTOR=2"),
                      NAflag=noValue, datatype="INT2S"))


rm(val) # remove ndvi matrix
cat("\n","Processing finished!", "\n")
end_time <- Sys.time()
end_time - start_time

#######################
# end processing
################


## visualize the results
# install necessary packages and adjust to your own variable names 

# cubeview works only for objects from raster package
ndvi <- raster::stack(paste0(outPath,outFile)) 
names(ndvi) <- 1:raster::nlayers(ndvi)
raster::NAvalue(ndvi) <- noValue

library(tmap)
tm_shape(ndvi) +
  tm_raster(style="fixed",palette = colorRampPalette(c("orangered3","yellow","darkgreen"))(13),
            breaks=seq(-2000,10000,by=1000),midpoint = NA) +
  tm_legend(outside = TRUE)

tmap_mode("view") # interactive map
tm_shape(ndvi[[c(10,20,30,40)]]) +
  tm_raster(style="fixed",palette = colorRampPalette(c("orangered3","yellow","darkgreen"))(13),
            breaks=seq(-2000,10000,by=1000),midpoint = NA) 
tmap_mode("plot") # static map

library(cubeview)
cubeview(ndvi,col.regions = colorRampPalette(c("orangered3","yellow","darkgreen")))



