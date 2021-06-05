##########################
# Exercise 3 - final
# Processing and analysis of single pixel time series 
# Date May 2021
# Author Anja Klisch <anja.klisch@boku.ac.at>
# GPL >= 3
##########################



## install packages if necessary
pkgs <- c("nloptr", "ptw")
to_install <- !pkgs %in% installed.packages()
if (any(to_install)) {
  install.packages(pkgs[to_install])
}

## load packages
library(ptw)
library(tidyverse)
library(lubridate)

## create and set working directory for Exercise 3
# define output path: adjust to you own path !!!
outPath <- "d:/RSTSA2021/Exerc3/"

# set working directory
setwd(outPath)


##########################
# Exercise 3_1
# Prepare and process the time series
##########################


## include the R script "whittaker.R" 
source("d:/RSTSA2021/Exerc3/whittaker.R") # include local function

file_out <- "_AT"
#file_out <- "_DE"

## load the NDVI time series of Exercise 2_2: adjust to you own path !!!
load(paste0("d:/RSTSA2021/Exerc2/Sentinel2_L2A_3_NDVI",file_out,".RData"))


#######################
# a) test Whittaker smoother for one time series and different lambdas
################

# filter one time series (e.g. undisturbed)
obs <- dataS2 %>% filter(id==6) 
#obs <- dataS2 %>% filter(label=="reference") 

# create a tibble with column "date" containing daily time steps
smoothed <- tibble(date=seq(ymd("2017-01-01"),ymd("2020-12-31"),by=1))

# use mutate join to load ndvi observations
smoothed <- smoothed %>% 
  left_join(obs,by="date")

# boxplot(smoothed$ndvi) # find outliers

# calculate winter NDVI as 10th percentile of time series
winterNDVI <- quantile(smoothed$ndvi, probs = 0.1,names = FALSE,na.rm=TRUE)

# create column "NDVI_winter" with observations from March to November 
# and winterNDVI from December to February
smoothed <- smoothed %>% 
  mutate(month=month(date),
         ndvi_winter = if_else(between(month,3,11), ndvi, winterNDVI)) 

# create a column "ndvi_smooth" that contains the smoothed time series (e.g. lambda 1000)
smoothed <- smoothed %>% 
  mutate(ndvi_smoothed=whittaker(ndvi_winter, l=1000, minval=winterNDVI, maxval=1))

# prepare plot with observations and smoothed time series for different lambda
crp <- RColorBrewer::brewer.pal(9,"Set1")
ggplot(data = smoothed, mapping = aes(x=date)) + 
  geom_point(mapping = aes(y=ndvi),col="grey",size=1.3) + 
  geom_point(mapping = aes(y=ndvi_winter),col="black",size=0.7) +
  geom_line(col=crp[1],size=0.7, 
            aes(y=whittaker(ndvi_winter, l=1000, minval=winterNDVI, maxval=1))) + 
  geom_line(col=crp[2],size=0.7, 
            aes(y=whittaker(ndvi_winter, l=10000, minval=winterNDVI, maxval=1))) +    
  geom_line(col=crp[3],size=0.7, 
            aes(y=whittaker(ndvi_winter, l=50000, minval=winterNDVI, maxval=1))) +   
  geom_line(col=crp[4],size=0.7, 
            aes(y=whittaker(ndvi_winter, l=100000, minval=winterNDVI, maxval=1))) +
  ylim(0,1)

# save plot in the working directory
ggsave(paste0("exerc3_1_testWhittaker",file_out,".png"), width = 8, height=5, dpi = 300)


#######################
# b) apply the processing steps to all time series
################


smoothTS <- function(i,data,lambda) {
  obs <- data %>% filter(id==i)
  
  # calculate winter NDVI as 10th percentile of time series
  winterNDVI <- quantile(obs$ndvi, probs = 0.1,names = FALSE,na.rm=TRUE)
  
  # create tibble for full daily time series
  smoothed <- tibble(date=seq(ymd("2017-01-01"),ymd("2020-12-31"),by=1),
                     id=obs$id[1], label=obs$label[1]) %>%
    left_join(select(obs, date, ndvi), by="date") %>%
    mutate(month=month(date),
           ndvi_winter = if_else(between(month,3,11), ndvi, winterNDVI),
           ndvi_smoothed=whittaker(ndvi_winter, l=lambda, minval=winterNDVI, maxval=1))
  
  return(smoothed)
}


dataS2_smoothed <- lapply(unique(dataS2$id), smoothTS,data=dataS2,
                          lambda=10000) %>% bind_rows()


#######################
# Prepare a plot with the observations and the smoothed time series
################

#title <- "Amaliendorf, Austria"
title <- "Jüterbog, Germany"

# create the plot similar to exercise 2-2b with:
# - observations including the winter NDVI as points 
# - smoothed times series as line
ggplot(data = dataS2_smoothed,mapping = aes(x=date, color=id)) + 
  geom_point(size=0.5,mapping = aes(y=ndvi_winter)) + 
  geom_line(size=0.5,mapping = aes(y=ndvi_smoothed)) +
  scale_colour_brewer(palette="Paired",direction = -1) +
  ggtitle(title) + ylim(0,1) +
  facet_wrap(~label,nrow=3)

# save the plot in the working directory
ggsave(paste0("exerc3_1_plotNDVI_smoothed",file_out,".png"), width = 9, height=6, dpi = 300)

# save the smoothed NDVI data in Rdata format
save(dataS2_smoothed, file = paste0("Sentinel2_L2A_3_NDVI_smoothed",file_out,".RData"))


##########################
# Exercise 3_2
# Create boxplots for displaying intra-annual and inter-annual variability of NDVI
##########################


title <- "Amaliendorf, Austria"
#title <- "Jüterbog, Germany"

file_out <- "_AT"
#file_out <- "_DE"

## load smoothed time series of Exercise 3_1 
load(paste0("Sentinel2_L2A_3_NDVI_smoothed",file_out,".RData"))

# create boxplots for entire time series
ggplot(data = dataS2_smoothed,mapping = aes(x=id, y=ndvi_smoothed, fill=id)) + 
  geom_boxplot(notch=TRUE) +
  scale_fill_brewer(palette="Paired",direction = -1) +
  ggtitle(title) + ylim(0,1)
ggsave(paste0("exerc3_2_boxplot_pixel_NDVI",file_out,".png"), width = 6, height=4, dpi = 300)

# add columns with year, month, week derived from date using lubridate
dataS2_smoothed <- dataS2_smoothed %>% 
  mutate(year=year(date), month=month(date),week=week(date))

## use the ggplot2 package to prepare the boxplots
## create a subplot for each id and fill boxes by label

# create boxplots for time series by year: inter-annual variability
ggplot(data = dataS2_smoothed,mapping = aes(x=year, y=ndvi_smoothed, fill=label, group=year)) + 
  geom_boxplot() +
  scale_fill_brewer(palette="Set1") +
  ggtitle(title) + facet_wrap(~id, nrow=max(as.integer(dataS2_smoothed$id)))
ggsave(paste0("exerc3_2_boxplot_pixel_NDVI_year_",file_out,".png"), width = 4, height=8, dpi = 300)

# create boxplots for time series by month:intra-annual variability
ggplot(data = dataS2_smoothed,mapping = aes(x=month, y=ndvi_smoothed, fill=label, group=month)) + 
  geom_boxplot() +
  scale_fill_brewer(palette="Set1") +
  ggtitle(title) + facet_wrap(~id, nrow=max(as.integer(dataS2_smoothed$id)))
ggsave(paste0("exerc3_2_boxplot_pixel_NDVI_month",file_out,".png"), width = 5.5, height=8, dpi = 300)

# create boxplots for time series by week:intra-annual variability
ggplot(data = dataS2_smoothed,mapping = aes(x=week, y=ndvi_smoothed, fill=label, group=week)) + 
  geom_boxplot() +
  scale_fill_brewer(palette="Set1") +
  ggtitle(title) + facet_wrap(~id, nrow=max(as.integer(dataS2_smoothed$id)))
ggsave(paste0("exerc3_2_boxplot_pixel_NDVI_week_",file_out,".png"), width = 6, height=8, dpi = 300)


##########################
# Exercise 3_3
# Calculate and plot z-score from smoothed Sentinel-2 time series
##########################

title <- "Amaliendorf, Austria"
#title <- "Jüterbog, Germany"

file_out <- "_AT"
#file_out <- "_DE"


########################
# 3_3a calculate statistics: mean/sd from reference region
################ 

## calculate daily mean and standard deviation from reference time series 
ref_stats <- dataS2_smoothed %>% 
  filter(label=="reference") %>% 
  mutate(doy=yday(date)) %>%
  group_by(doy) %>% 
  summarise(mean=mean(ndvi_smoothed),sd=sd(ndvi_smoothed))

## check the result of the calculations
ggplot(ref_stats, aes(x=doy, y=mean)) + 
  geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd), alpha = 0.2)  +
  ylim(0,1) +
  geom_line()


########################
# 3_3b calculate z-score: all pixel time series
################ 

# calculate daily z-score only from March to November (otherwise NA)
dataS2_smoothed_zscore <- dataS2_smoothed %>% 
  mutate(doy=yday(date)) %>%
  left_join(ref_stats,by="doy") %>%
  mutate(zscore=ifelse(between(month,3,11),(ndvi_smoothed-mean)/sd,NA))

########################
# plot smoothed NDVI and z-score: all pixel time series
################ 

# plot smoothed NDVI
ggplot(data = dataS2_smoothed_zscore,mapping = aes(x=date, color=id)) + 
  geom_line(size=0.7,mapping = aes(y=ndvi_smoothed)) +
  scale_colour_brewer(palette="Paired",direction = -1) +
  ylim(0,1) + 
  ggtitle(title) 
ggsave(paste0("exerc3_3_plotNDVI",file_out,".png"), width = 6, height=4, dpi = 300)

# plot zscore
ggplot(data = dataS2_smoothed_zscore,mapping = aes(x=date, color=id)) + 
  geom_line(size=0.7,mapping = aes(y=zscore)) +
  scale_colour_brewer(palette="Paired",direction = -1) +
  ylim(-40,10) + 
  ggtitle(title) 
ggsave(paste0("exerc3_3_plotNDVI_zscore",file_out,".png"), width = 6, height=4, dpi = 300)




