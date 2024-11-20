# ECL 216 Workshop on data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABIAAAASCAYAAABWzo5XAAAAWElEQVR42mNgGPTAxsZmJsVqQApgmGw1yApwKcQiT7phRBuCzzCSDSHGMKINIeDNmWQlA2IigKJwIssQkHdINgxfmBBtGDEBS3KCxBc7pMQgMYE5c/AXPwAwSX4lV3pTWwAAAABJRU5ErkJggg==Spatial Analysis: Bumblebee Example!
# Maureen Page and Neal M Williams
# 28 feb 2019 Updated Jan 2022 NMWilliams


# Disclaimer on how I learned to do this / where some of the code came from (also great resources!)
# https://nceas.github.io/oss-lessons/spatial-data-gis-law/4-tues-spatial-analysis-in-r.html
# http://zevross.com/blog/2015/03/30/map-and-analyze-raster-data-in-r/
# https://crd230.github.io/index.html

# things in R are always on the move=being updated.  For my machine there is one package we need "terra" that is such a package 
# to run it we need to use an uncompiled version of so we need to compile it using Rtools4. for me on Windows
# website is https://cran.r-project.org/bin/windows/Rtools/rtools40.html
# check with Maureen about Mac version of this  
# this will allow you to use the most recent version https://cran.r-project.org/web/packages/terra/terra.pdf check it out Jan 13 2022!
# created by UC Davis' Robert Hijmans 



## ----Install and Library necessary packages------------------------------------------

# Set working directory and upload data
#rm(list = ls()) # start clean
setwd("C:\\Users\\Neal\\Dropbox\\Documents\\Teaching\\Courses\\Ecology and Agriculture ECL216\\2024-Class\\Spatial Ecology Activity") # rename this so it's the same as the directory you're using right now
#bumbles = read.csv("Bumblebee_colony_data.csv") 
#str(bumbles) # we'll come back to this later - but this dataset contains data on colony performance (e.g. rworkers - residual worker number), etc.

# Packages (you can un-hashtag these if you need to download them - I recommend doing so even if you've already downloaded them, for instance, you need the latest version of tidyverse for some of the plotting functions to work)

 install.packages("rmapshaper")
 install.packages("tigris")
 install.packages("sf")
 install.packages("tmap")
 install.packages("tidycensus")
# install.packages("tidyverse")
 install.packages("matrixStats")
 install.packages("spData")
 install.packages("raster")
 install.packages("sp")
 install.packages("terra")
 install.packages("landscapemetrics")
 install.packages("ggplot2")

# Load packages
library(ggplot2)
library(units)
library(matrixStats)
library(sf)
library(tigris)
#you need to let R know to bring in the spatial data as sf objects
options(tigris_class = "sf")
library(rmapshaper)
library(rgdal)
library(rgeos)
library(geos)
library(ps)
library(tidyverse)
library(tmap)
library(leaflet)
library(spData)
library(dplyr)
library(raster) # note this runs a new version from Robert Hjimans UC Davis, but its uncompiled, so you need to use Rtools to compile the new version of Terra
library(sp)
library(landscapemetrics)

## ----Bringing in the "regular" data-----------------------------------------------

# Set working directory and upload data
setwd("C:\\Users\\nwilliam\\Dropbox\\Documents\\Teaching\\Courses\\Ecology and Agriculture ECL216\\2024-Class\\Spatial Ecology Activity") # rename this so it's the same as the directory you're using right now
# remame this so it's the same as the directory you're using right now
bumbles = read.csv("Bumblebee_colony_data.csv") 
str(bumbles) # we'll come back to this later - but this dataset contains data on colony perfomance (e.g. rworkers - residual worker number), etc.

## ----Bringing in the spatial  data!-----------------------------------------------

# Today we'll work with two types of spatial data - vector data (shape files) and raster data
# Shape files and the vector data they contain are used to map data and can be points or polygons - (e.g. boudaries around states or farm properties(polygons), 
# gps locations of bumblebee colonies or field sites (points), or "buffers" around a particular gps location (polygons)
# A "raster" is a data structure that divides a region into gridded cells (or 'pixels') 
# that can store its location in the grid plus one or more values for each of these cells (e.g. a number representing a landscape category for that pixel location on the map)

# The shape file we'll be working with contains the gps locations of bumblebee colonies 
# at different almond orchards in the central valley. This shape file also contains data 
# about the points (e.g TYPE - whether the site is an organic orchard, conventional orchard, or a "wild" non-farm natural site)
# Bring in the Bombus shape file using st_read()
bombus.sites <- st_read("BombusSites3type.shp") # GPS locations of BB colonies as a shape file - provided by Neal
str(bombus.sites)
# The raster file we'll be using was downloaded from this website: https://www.mrlc.gov/viewer/. If you copy-paste the link into your browser you'll see that this website allows you do download landcover data for set regions, or you can download data for the continental U.S.! (a very large file - would not recommend using it if you have a small study region)
# Bring in land cover raster file using raster()
NLCD <- raster("nlcd_2011_landcover_2011_edition_2014_10_10_0wQL4oSkdXK7DoAQbjRh.tiff") # dowloaded at this website: https://www.mrlc.gov/viewer/
#rasterOptions(memfrac=.3) optional setting if needed see below at the reprojecting step - approx line 132
# look at the raster attributes
NLCD # what type of data are these = grid or cells - provides the project landscape ccontext (how it is represented on the earth), 
     # each takes a value representing a cover type, also note the scale of the grids, etc.

## ----Plotting raster - whole northern part of the central valley --------------------------------------------------

# plot the raster
plot(NLCD, 
     main="Nor. Cal. Central Valley")  # note if you get an error about margins, adjust your window size

## ----Determine raster layer values-----------------------------------------

vals<-unique(values(NLCD))
vals # 11 is open water, # 21,22,23,24 are all developed land, # 31 Barren land, # 41,42,43,52, 71 are forest /
#shrubland, # 81 is pasture land / grassland, # 82 is agriculture, # 90 is woody wetland and 95 is herbaceous wetland. 
#We know this thanks to this website: https://www.mrlc.gov/data/legends/national-land-cover-database-2011-nlcd2011-legend - What are metadata?

## ----Reclassify layer values so we only have two, 1 (natural) and 2 (not nat.) ----
# We're going to say anything except developed land, open water, and agriculture is natural habitat
recl<-matrix(c(vals, c(rep(1,3),0,rep(1,2),0,1,0,1,rep(0,4),1,0)),ncol=2) # made a function to reasign values
recl # on the left are the former values, on the right we've reclassified everything as 1 - natural habitat, and 0 - everything else

# STOP HERE The following code is used to reclassify the layer values in the raster 
# in order for us to calculate precent natural habitat later on, it's #hastaged-out here because it takes forever to run
# from what I can tell we have to run it once each. After that we can comment out and reuse.
landuse<-reclassify(NLCD, rcl=recl) # this code takes forever to run running the little function across all pixels
save(landuse,file="landuse.Rdata") # saving raster so we never have to run that code again
load("landuse.Rdata") # Here in the future we instead upload the version saved already
# NOTE now we only have two land use categories - natural and non natural
plot(landuse, legend=FALSE, axes=FALSE)

# could we trim down the extent of the raster data file so working with a smaller area?

##----Determine raster CRS (coordinate reference system) and re-project as needed----
# Before we start working with the bumblebee colony data, we need to make sure that both files are using the same coordinate reference system
# Check the CRS of all boundary files
st_crs(bombus.sites)
st_crs(landuse) # no espg code !  European standard - google to see what these are all about.

# STOP  - the two have different crs - so we need to reproject NLCD data

##----Re-project the data so both are using the same CRS----------------------------

# NOTE Again, I've #hashtagged-out the code because it takes too long to run tr= transposed
#landuse.tr <- projectRaster(landuse, crs = crs(bombus.sites)) 
#if the program hangs up on the above command- gives a memory related error message - try running the following
# rasterOptions(memfrac=.3) in the command console.  I have it commented out above too right after
#you load the landcover raster

save(landuse.tr,file="landuse.tr.Rdata") # saving raster so we never have to run that code again
load("landuse.tr.Rdata") # instead we'll download the version I've saved
st_crs(landuse.tr)
plot(landuse.tr)
plot(bombus.sites,
     add = TRUE, pch =21,bg=12)

# first let's make sure we're using the same units for both 
# Note: if you are going to use a buffer step independently will want to add to that set of codes so you are checking
st_crs(landuse.tr)$units
st_crs(bombus.sites)$units
## both are meters, so that's good

# STOP     NOW what is our question: everyone weigh in.

##----Draw buffers around each colony------------------------------------------------
# draw a buffer around each dot

# We use the sf function st_buffer() to create buffers. The required arguments are your sf object and the distance.
# Remember that the units are meters, so specifying dist = 1000 means 1km.

bumble.buff <-st_buffer(bombus.sites, dist = 1000) # 1000m = 1km buffer
bumble.buff

# crop the data, including buffer boundaries, to allow for higher resolution when plotting (the data we downloaded originally covered a larger geographical area than our study area)
landuse.tr_cr <- crop(landuse.tr,
                   bumble.buff)
plot(landuse.tr_cr)

## ----NOW LET'S MAKE AN EPIC MAP !!!!------------------------------------

# !!
ex.map<-tm_shape(landuse.tr_cr) +
  tm_raster() +
  tm_shape(bombus.sites) +
  tm_dots(col = "TYPE", palette = "Blues", style = "quantile", size = 0.02) +
  tm_shape(bumble.buff) +
  tm_borders(col="blue") +  
  tmap_mode("view")
ex.map 

# now we can zoom in on the colonies - and see the 1km buffer around them!

## ----Percent Natural Habitat Calculations------------------------------------

# now we want to calculate the percent natural habitat around in each buffer
nlcd_mean <- raster::extract(landuse.tr_cr, # the raster that you wish to extract values from
                                bumble.buff, # a point, or polygon spatial object
                                fun = mean, # extract the MEAN value from each plot
                                sp = TRUE) # create spatial object

nlcd_mean$layer
nlcd_mean<-as.data.frame(nlcd_mean) 
nlcd_mean$nlcd_2011_landcover_2011_edition_2014_10_10_0wQL4oSkdXK7DoAQbjRh # is the percent natural habitat from earlier code- now gives output in variable named "layer".
str(nlcd_mean)
# now that we've caluculated the percent natural habitat, let's retrieve data on the performance of the colonies at each site and merge the datasets
bombus_merger22 <- merge(nlcd_mean, bumbles, by.x="SITE", by.y="site")
#bombus_merger22$percentnat<-bombus_merger22$layer
bombus_merger22$percentnat<-bombus_merger22$nlcd_2011_landcover_2011_edition_2014_10_10_0wQL4oSkdXK7DoAQbjRh
bombus_merger22$percentnat
str(bombus_merger22)
bombus_merger22

write.csv(bombus_merger22, file = "Bumblebee_landscape_output2022.csv")


ggplot(bombus_merger22, aes(x=percentnat, y=rworkers)) +
  geom_point(aes(col=TYPE), shape = 1, size = 2.5, stroke = 1.25) +
  geom_smooth(method = lm, # Add linear regression lines
              se = FALSE, color="black") +  # Don't add shaded confidence region
  labs(x = "Proportion of Natural Habitat", y = "Residual Worker Number") +
  theme_classic() + 
  theme(legend.title=element_blank(),legend.position = c("top"),
        legend.direction = "horizontal",
        text=element_text(family="Times", color = "black", size=22), axis.text=element_text(family="Times", color = "black", size=14))

#figure out how to plot a function that is curvilinear)
ggplot(bombus_merger22, aes(x=percentnat, y=rworkers)) +
  geom_point(aes(col=TYPE), shape = 1, size = 2.5, stroke = 1.25) +
  geom_smooth(method = lm, formula = y~poly(x,2), # Add linear regression lines
             se = TRUE, color="black") +  # Don't add shaded confidence region
  labs(x = "Proportion of Natural Habitat", y = "Residual Worker Number") +
  theme_classic() + 
  theme(legend.title=element_blank(),legend.position = c("top"),
        legend.direction = "horizontal",
        text=element_text(family="Times", color = "black", size=22), axis.text=element_text(family="Times", color = "black", size=14))
#__________________________________________________________________________

ggplot(bombus_merger22, aes(x=percentnat, y=rworkers)) +
  geom_point(aes(col=TYPE), shape = 1, size = 2.5, stroke = 1.25) +
  geom_smooth(method = lm, formula = y ~ splines::bs(x,2), # Add linear regression lines
             se = FALSE, color="black") +  # Don't add shaded confidence region
  labs(x = "Proportion of Natural Habitat", y = "Residual Worker Number") +
  theme_classic() + 
  theme(legend.title=element_blank(),legend.position = c("top"),
        legend.direction = "horizontal",
        text=element_text(family="Times", color = "black", size=22), axis.text=element_text(family="Times", color = "black", size=14))


# Is there an effect of habitat type?
ggplot(bombus_merger22, aes(x=percentnat, y=rworkers, color=TYPE)) +
  geom_boxplot() +
  labs(x = "Local Habitat Type", y = "Residual Worker Number") +
  theme_classic() + 
  theme(legend.title=element_blank(),legend.position = c("top"),
        text=element_text(family="Times", color = "black", size=22), axis.text=element_text(family="Times", color = "black", size=14))

# Is there an interaction between habitat type and percent nat. habitat?
ggplot(bombus_merger22, aes(x=percentnat, y=rworkers, color=TYPE)) +
  geom_point(shape = 1, size = 2.5, stroke = 1.25) +
  geom_smooth(method = lm,   # Add linear regression lines
              se = TRUE) +  # Don't add shaded confidence region
  labs(x = "Proportion of Natural Habitat", y = "Residual Worker Number") +
  theme_classic() + 
  theme(legend.title=element_blank(),legend.position = c("top"),
        legend.direction = "horizontal",
        text=element_text(family="Times", color = "black", size=22), axis.text=element_text(family="Times", color = "black", size=14))



