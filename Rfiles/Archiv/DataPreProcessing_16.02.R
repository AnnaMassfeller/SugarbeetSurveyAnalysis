############################
#  Data Pre-processing    #
###########################

rm(list = ls())
#load packages
library(readxl)
library(dplyr)
library(jsonlite)
library(stringr)
library(geosphere)
library(rgdal)
library(terra)
library(raster)
library(rgeos)
library(sp)
library(leaflet)

###load data
setwd("C:/Users/Massfeller/sciebo/PhD/Paper/Paper2_socioSpat/AnalysisPaper2")
FullSample<- read_excel("Data_prep/sample_15.02.xlsx")#excel
#create subsample (just for trial purposes) with entries in the coordinate columns
SampleDF<- FullSample[!is.na(FullSample$q4_shape_own),]
SampleDF<- FullSample[!is.na(FullSample$q5_shape_other),]
#SampleDFcsv <- read.csv("Data_prep/sample.csv")#csv

#directly as json
#json_file <- "https://fruchtfolge.agp.uni-bonn.de/db/survey_anna/_all_docs?include_docs=true"
#json_data <- fromJSON(paste(readLines(json_file), collapse=""))



######################
###let's go spatial###
######################

#install.packages('terra', repos='https://rspatial.r-universe.dev')
#if only one plot/marker was selected, we need to copy the coordinates in order to be able to create a 
#SpatialPointsDataFrame, as it shouldn't make any difference, we just copy it for all

#2-copy corrdinates into oroginal column
SampleDF$coord_own <- SampleDF$q4_shape_own
SampleDF$q4_shape_own <- paste(SampleDF$q4_shape_own,SampleDF$coord_own)

#same for those who only selected one neighbouring field
SampleDF$coord_nei <- SampleDF$q5_shape_other
SampleDF$q5_shape_other <- paste(SampleDF$q5_shape_other,SampleDF$coord_nei)




#manipulating string type, create list with coordinates for own fields
own_fields <- SampleDF$q4_shape_own
own_fields<- chartr("{}lat:lng()'[]","                ", own_fields)
summary(own_fields)
own_fields<-str_split(own_fields, boundary("word"))
str_sub(own_fields[[2]][3])#third coordinate of second farmer (1.coordinate of second field)

#same for neighbours fields
nei_fields <- SampleDF$q5_shape_other
nei_fields<- chartr("{}lat:lng()'[]","                ", nei_fields)
nei_fields<-str_split(nei_fields, boundary("word"))

#get number of fields choosen
#for own fields
SampleDF$Nr.own_fields <-0
for(i in seq_along(own_fields)){
  SampleDF$Nr.own_fields[i]<-length(own_fields[[i]])
}
SampleDF$Nr.own_fields <-SampleDF$Nr.own_fields/2

#for neighbouring fields
SampleDF$Nr.nei_fields <-0
for(i in seq_along(nei_fields)){
  SampleDF$Nr.nei_fields[i]<-length(nei_fields[[i]])
}
SampleDF$Nr.nei_fields <-SampleDF$Nr.nei_fields/2

#####
######manually for each farmer
#define ceontroid of own fields as farm location
own_fields.long <- as.numeric(c(own_fields[[2]][1],own_fields[[2]][3]))
own_fields.lat <- as.numeric(c(own_fields[[2]][2],own_fields[[2]][4]))
own_fields.coor <- cbind(own_fields.long, own_fields.lat)
own_fields.coor <- as.data.frame(own_fields.coor)

own_fields.spat <- SpatialPointsDataFrame(coords = own_fields.coor, data = own_fields.coor)#has to be saved in the to go data set then
centroids.farms = gCentroid(own_fields.spat)#has to be saved in the to go dataset then


#others fields
nei_fields.lon <- as.numeric(c(nei_fields[[2]][1],nei_fields[[2]][3],nei_fields[[2]][5]))
nei_fields.lat <- as.numeric(c(nei_fields[[2]][2],nei_fields[[2]][4],nei_fields[[2]][6]))
nei_fields.coor <- cbind(nei_fields.lon, nei_fields.lat)
nei_fields.coor <- as.data.frame(nei_fields.coor)

nei_fields.spat <- SpatialPointsDataFrame(coords = nei_fields.coor, data = nei_fields.coor)

#distance between centroid and neighbours fields
dist.nei_fields.farm <- pointDistance(nei_fields.spat,centroids.farms, lonlat = T) #in luftlinie in m
dist.nei_fields.farm <- dist.nei_fields.farm/1000
mean(dist.nei_fields.farm)#save in to go data frame


plot(centroids.farms,add=T)
plot(own_fields.spat,col='red',add=TRUE)
plot(nei_fields.spat,col='blue')  
#####

######################################
##loop over all elements of the list##
######################################

#create function to get every item
coord.as.df <- function(x) cbind.data.frame(split(x[[i]], rep(1:2, times=length(x[[i]])/2)), stringsAsFactors=F) 

#create empty data frames
coord.own_fields <- data.frame()
Centroids <- data.frame()

#create df of coordinates for each farmer for all his/her fields and calculate centroid = farm location
for(i in seq_along(own_fields)){
  coord.own_fields <- coord.as.df(own_fields) #need zwischenschritt um cooord. als list zu speiichern
  coord.own_fields<- as.data.frame(sapply(coord.own_fields,as.numeric))
  own_fields.spat <- SpatialPointsDataFrame(coords = coord.own_fields, data = coord.own_fields)
  centroids.farms= gCentroid(own_fields.spat)
  assign( paste("farms.spat", i, sep = "_") , centroids.farms)
    }
#Centroids <- rbind(farms.spat_1, farms.spat_2)
list.centroids <- do.call("list",mget(ls(pattern = "^farms.spat.*")))

####################################
#do the same for neighbouring fields
#create empty data frame
coord.nei_fields <- data.frame()

#create df of coordinates for each farmer for aneighbouring fields
for(i in seq_along(nei_fields)){
  coord.nei_fields <- coord.as.df(nei_fields)
  coord.nei_fields<- as.data.frame(sapply(coord.nei_fields,as.numeric))
  nei_fields.spat <- SpatialPointsDataFrame(coords = coord.nei_fields, data = coord.nei_fields)
  assign( paste("nei_fields.spat", i, sep = "_") , nei_fields.spat)
}

#put all nei_farms in a list
list.nei_fields <- do.call("list",mget(ls(pattern = "^nei_fields.spat_.*")))

#loop over list and centroids to get distance between centroid and neighbours fields
SampleDF$meanDist <- 0
for(i in seq_along(list.centroids)){
  SampleDF$meanDist[i] <- mean(pointDistance(list.nei_fields[[i]],list.centroids[[i]], lonlat = T)/1000)
}


#check manually
SampleDF$meanDist_test <- 0
SampleDF$meanDist_test[1] <- mean(pointDistance(list.nei_fields[[1]],list.centroids[[1]], lonlat = T)/1000)
SampleDF$meanDist_test[2] <- mean(pointDistance(list.nei_fields[[2]],list.centroids[[2]], lonlat = T)/1000)
SampleDF$meanDist_test[3] <- mean(pointDistance(list.nei_fields[[3]],list.centroids[[3]], lonlat = T)/1000)
SampleDF$meanDist_test[4] <- mean(pointDistance(list.nei_fields[[4]],list.centroids[[4]], lonlat = T)/1000)
SampleDF$meanDist_test[5] <- mean(pointDistance(list.nei_fields[[5]],list.centroids[[5]], lonlat = T)/1000)
SampleDF$meanDist_test[6] <- mean(pointDistance(list.nei_fields[[6]],list.centroids[[6]], lonlat = T)/1000)



#Todo
#if only one field selected, thsi can be used to calculat ethe distance directly
#check what happens with those who überspringen, how can the data.frame look like then?














##mapping centroid to county(Landkreis)--> Seperate file togetehr with Dario
library(sp)
library(rgdal)
library(sf)
library(sp)
library(RColorBrewer)
gadm36_DEU_2_sp <- readRDS("Data_prep/gadm36_DEU_2_sp.rds")
# get spatial data for Germany on county level
county <- gadm36_DEU_2_sp
plot(county, col ='grey')



