##########################
# Exercise 4_1
# Create a customised plot of HLS data for visual inspection 
# Date May 2021
# Author Anja Klisch <anja.klisch@boku.ac.at>
# GPL >= 3
##########################

#######################
# settings 
################

library(terra)
Sys.setlocale("LC_ALL","English")

# adjust to you own path !!!
inPathS30 <- "d:/RSTSA2021/Exerc4/HLSv14/33UVP/2018/S30/"
inPathL30 <- "d:/RSTSA2021/Exerc4/HLSv14/33UVP/2018/L30/"
outPath <- "d:/RSTSA2021/Exerc4/Rplots/"

pattern <- "NDVI_cropped.tif$"
noValue <- -3000

x <- c(438009,440810,438707,438817)
y <- c(5335609,5338611,5335309,5335854)

#######################
# prepare data 
################

# list all NDVI file names for Sentinel-2 and Landsat 8 including paths
filesS30 <- list.files(path=inPathS30, pattern=pattern, full.names=TRUE)
filesL30 <- list.files(path=inPathL30, pattern=pattern, full.names=TRUE)
files <- c(filesS30,filesL30)

# extract YEARDOY from name by splitting at "." and convert to date
dates <- as.Date(sapply(strsplit(basename(files), "[.]"), "[[", 4),format="%Y%j")

# sort file names and dates by date
files <- files[order(dates)]
dates <- dates[order(dates)]

# create SpatRast for all NDVI files
img <- rast(files)
NAflag(img) <- noValue 
# create SpatRast for all SM files
imgSM <- rast(sub("NDVI_cropped.tif","SM_cropped.tif",files))

# create output path
dir.create(outPath, showWarnings = FALSE)

# create matrix from x y coordinates
xy <- cbind(x,y)

# alternatively, coordinates from plot
# plot(img[[60]]) # plot one of the images
# points <- locator() # returns list of x/y coordinates from clicked postions in plot
# xy <- as.data.frame(points) # convert to data frame

# extract NDVI & SM at points xy (coordinates as data frame or matrix)
valNDVI <- extract(img, xy)*0.0001 # extract and convert DN to NDVI
valSM <- extract(imgSM, xy) # extract SM

# set working directory_ path for plot files
setwd(outPath)

#######################
# plot data 
################

#Loop for points
for (i in (1:dim(xy)[1])) 
{ #i<-1
  
  # get NDVI & SM at point
  NDVI <- valNDVI[i,]
  SM <- valSM[i,]
  
  png(paste0("test",i,".png"),  width=900, height=600,res=150)   
  
  plot(x=dates, y=NDVI,ylab="NDVI", ylim=c(-0.2, 1.00),
       xlab="Dates",
       main = paste0("Point (",round(xy[i,1]),"|",round(xy[i,2]),")"),
       type = "l") # plot data as lines
  
  # valid obs  
  points(x=dates[SM==0],y=NDVI[SM==0],pch = 19, cex=0.8,col="darkgreen")
  # cirrus, cloud
  points(x=dates[SM==1],y=NDVI[SM==1],pch = 1, cex=1,col="red") 
  
  lines(x=dates[SM==0 & !is.na(NDVI)],y=NDVI[SM==0 & !is.na(NDVI)], col="darkgreen", lwd=2) 
  
  abline(v=seq(as.Date("2018-01-01"),as.Date("2019-01-01"),by="month"),col="grey")

  legend("bottomleft", 
         legend = c(paste0("valid obs (",sum(SM==0 & !is.na(SM) & !is.na(NDVI)),")"),
                    paste0("cirrus/clouds (",sum(SM==1 | is.na(SM) | is.na(NDVI)),")")),
         col = c("darkgreen","red"),
         pch = c(19,1),
         bg="white",ncol=2)
  
  dev.off()

}




