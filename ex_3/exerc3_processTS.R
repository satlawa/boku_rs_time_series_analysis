##########################
# Exercise 3 - template
# Processing and analysis of single pixel time series 
# Date May 2021
# Author Anja Klisch <anja.klisch@boku.ac.at>
# GPL >= 3
##########################

# missing code is indicated by ???

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

## define and set working directory for Exercise 3



##########################
# Exercise 3_1
# Prepare and process the time series
##########################


## include the R script "whittaker.R" 
#source(???)

## import data from Exercise 2-1 
#load(???)

#######################
# a) test Whittaker smoother for one time series and different lambdas
################

# filter one time series (e.g. undisturbed)
#obs <- ???

# create a tibble with column "date" containing daily time steps
#smoothed <- ???

# use mutate join to load ndvi observations
#smoothed <- ???

# calculate winter NDVI as 10th percentile of time series (e.g. quantile())
#winterNDVI <- ???

# create column "NDVI_winter" with observations from March to November 
# and winterNDVI from December to February
smoothed <- smoothed %>% 
  mutate(month=month(date),
         ndvi_winter = if_else(between(month,3,11), ndvi, winterNDVI)) 

# create a column "ndvi_smooth" that contains the smoothed time series (e.g. lambda 1000)
#smoothed <- ???

# prepare plot with observations and smoothed time series for different lambda
crp <- RColorBrewer::brewer.pal(9,"Set1")
ggplot(data = smoothed, mapping = aes(x=date)) + 
  geom_point(mapping = aes(y=ndvi),col="grey",size=1.3) + 
  geom_point(mapping = aes(y=ndvi_winter),col="black",size=0.7) +
  geom_line(col=crp[1],size=0.7, 
            aes(y=whittaker(ndvi_winter, l=1000, minval=winterNDVI, maxval=1))) + 
  #??? # add smoothed time series with further lambda values
  ylim(0,1)


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


# create the plot similar to exercise 2-2b with:
# - observations including the winter NDVI as points 
# - smoothed times series as line


# save the plot in the working directory


# save the tibble with the smoothed NDVI data in the Rdata format



##########################
# Exercise 3_2
# Create boxplots for displaying intra-annual and inter-annual variability of NDVI
##########################


## load smoothed time series of Exercise 3_1 if necessary

# add columns with year, month, week derived from date using lubridate library
dataS2_smoothed <- dataS2_smoothed %>% 
  mutate(year=year(date), month=month(date),week=week(date))

## use the ggplot2 package to prepare the boxplots
## create a subplot for each id and colorize by label

# create boxplots for time series by year: inter-annual variability


# create boxplots for time series by month or week: intra-annual variability


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


########################
# plot smoothed NDVI and z-score: all pixel time series
################ 

# plot smoothed NDVI 


# plot zscore



