##########################
# Exercises 1-1
# Spatial data in R - vector data
# Date May 2021
# Autor Anja Klisch <anja.klisch@boku.ac.at>
# GPL >= 3
##########################

## install packages if necessary
pkgs <- c("sp", "rgdal", "sf", "raster", "terra", "tidyverse", 
          "RColorBrewer", "colorRamps", "viridis", "rasterVis", "tmap")
to_install <- !pkgs %in% installed.packages()
if (any(to_install)) {
	install.packages(pkgs[to_install])
}


#######################
# 1-1 Vector objects with package sf
################

## load the sf library
if ( !require(sf) )         { install.packages("sf");                                        library(sf)      }

## list available drivers


## set working directory e.g. "D:/RSTSA2021/Exerc1/": adjust to your own path !!!
setwd("~/Code/r/boku_rs_time_series_analysis/ex_1")

## read the shapefile  "county2019.*" and assign to the object "shp"
shp <- st_read("county2019.shp")

## view metadata and attribute data of "shp"
str(shp)

## plot "shp"
# plot geometry with attributes: sf object


# plot geometry only: sfc object
shp$geometry

# subsetting: colourize shapes with area > 2
shp <- shp %>% mutate(area2 = Shape_Area > 2)
shp 

## add a new column "id" with IDs (e.g. consecutive numbers) to the data frame
shp$ID <- 

## write a new shapefile "county2019_ID.*"
st_write()


## use tmap to prepare thematic maps
# https://geocompr.robinlovelace.net/adv-map.html
library(tmap)

# by default static map
tm_shape(shp) + 
  tm_polygons(col = "Shape_Area", n = 3) + 
  tm_text("ADM1_EN", size = "Shape_Area",legend.size.show=FALSE) +
  tm_scale_bar(position = c("left","bottom")) + 
  tm_legend(outside = TRUE) +
  tm_compass()

tmap_mode("view") # interactive map
tm_shape(shp) + 
  tm_polygons() +
  tm_text("ADM1_EN")

tmap_mode("plot") # static map


##########################
# Exercises 1-2
# Spatial data in R - raster  data
##########################

#######################
# SpatRaster
################
library(terra)
library(sf)

###########################
# 1-2a reading

## set working directory adjust to you own path !!!
setwd("~/Code/r/boku_rs_time_series_analysis/ex_1")

## list all files in the present path (here working directory) of a certain pattern (e.g. glob2rx) and assign to the object "files"
files <- list.files(path= '~/Code/r/boku_rs_time_series_analysis/ex_1', pattern=glob2rx("*SOS.thr07.AT.tif"), full.names=TRUE)

# note on glob2rx: 
# file search is done with regular expressions (search pattern that describes a set of strings)
# if you are not familiar with regular expressions, use glob2rx which works on usual wildcards (placeholders), e.g. *,?
# and translates them to the corresponding regular expressions

## create single-layer SpatRaster from first file (e.g. "r1")
#r1 <- raster(files[1])
r1 <- rast(files[1])

## explore properties of the SpatRaster
ncol(r1); nrow(r1); ncell(r1); ext(r1); crs(r1)

## set the NA flag for the SpatRaster
NAflag(r1) <- -9999

## plot the SpatRaster
plot(r1)

## create SpatRaster from all files (e.g. "s1")
s1 <- rast(files)

## plot the multi-layer SpatRaster
plot(s1)

###########################
# 1-2b plotting

## create a boxplot of the multi-layer SpatRaster
boxplot(s1)

## plot uses different types of map/legend: "continuous", "classes", or "interval"
col_inf <- viridis::inferno(200)
plot(s1, col=col_inf)
boxplot(s1, col=viridis::inferno(15))

# create your own palette and make a continuous plot
cols <- colorRampPalette(c("lightyellow", "darkgreen"))(200)
plot(r1,type="continuous",col=cols,colNA="grey",main="Start of season (SOS) in days")

## prepare an interval plot of the SpatRaster with a meaningful color scheme: define cut/break values and associated colors 
# OR
## prepare a continuous plot of the SpatRaster with a meaningful color scheme; use range if necessary
plot(r1,type="continuous",col=cols,colNA="grey",main="Start of season (SOS) in days", range=c(60,180))

## add the administrative boundaries "bundeslaender2019.*" to your plot (SpatVector)
plot(add=TRUE, vect("bundeslaender2019.shp"))


###################
# more plot functions from other packages

crp <- viridis::viridis(200)
cuts<-c(1,seq(60,180,by=20),366)

## use the tmap package
library(tmap)
# map single-layer SpatRaster 
tm_shape(r1) +
  tm_raster(style="fixed",palette = crp, breaks=cuts, colorNA="grey", title="SOS in days",legend.hist = TRUE) +
  tm_legend(outside = TRUE, hist.width = 2) +
  tm_shape(st_read("bundeslaender2019.shp")) +
  tm_borders("white", lwd = .5)

# map multi-layer SpatRaster
tm_shape(s1) +
  tm_raster(style="fixed",palette = crp,breaks=cuts) +
  tm_legend(outside = TRUE)


## use spplot (sp package, requires raster object) to display all years 
s2 <- raster::stack(files); raster::NAvalue(s2) <- -9999; names(s2) <- 2002:2017
sp::spplot(s2,col.regions=crp,
           at=cuts,main="Start of season (SOS) in days")


## use the levelplot function (rasterVis package, requires object) 
r2 <- raster::raster(files[1]); raster::NAvalue(r2) <- -9999 
rasterVis::levelplot(r2,par.settings=rasterVis::viridisTheme)



###########################
# 1-2c accessing & writing

## write a multi-layer raster to GeoTiff file
writeRaster(s1,
            filename="MCD13A2.006.1_km_10_days_NDVI.OF.SOS.thr07.ATall.tif",
            overwrite=TRUE, 
            wopt=list(gdal=c("COMPRESS=LZW","PREDICTOR=2"),
                      NAflag=-9999, datatype="INT2S"))

## read a SpatRaster from the previous GeoTiff file (e.g. "s3")
s3 <- rast("MCD13A2.006.1_km_10_days_NDVI.OF.SOS.thr07.ATall.tif")

## get all cell values from the SpatRaster and assign to the object "val3" & what is the result?
val3 <- 

## calculate the median per pixel and assign to the object "mval3"
mval3 <- 

## create a new SpatRaster (e.g. "mr3") and assign the pixel medians


# same as: 
# median(s3,na.rm=TRUE) 
# app(s3,fun=median,na.rm=TRUE) # much slower

## plot the result using the color scheme of the previous plot
# use tmap or levelplot


## write the SpatRaster to a GeoTiff file "MCD13A2.y20022017.006.1_km_10_days_NDVI.OF.SOS.thr07.ATmedian.tif"
writeRaster(x, "MCD13A2.y20022017.006.1_km_10_days_NDVI.OF.SOS.thr07.ATmedian.tif")

## calculate the median for each layer of the SpatRaster


###########################
# 1-2d cropping & calculations

## crop the median SOS SpatRaster by drawing an extent: plot(), draw(), crop()

# or

## mask the the median SOS SpatRaster using the administrative boundaries

# or

## calculate zonal SOS statistics for administrative boundaries (vector) and median SOS (SpatRaster)

## add new column to administrative boundaries for medianSOS

## plot administrative boundaries and colorize by medianSOS









