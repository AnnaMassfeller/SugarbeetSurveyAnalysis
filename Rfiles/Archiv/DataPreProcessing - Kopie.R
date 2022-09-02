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
library(car)
library(stargazer)

###load data
#directly as json
json_file <- "https://fruchtfolge.agp.uni-bonn.de/db/survey_anna/_all_docs?include_docs=true"
json_data <- fromJSON(paste(readLines(json_file), collapse=""))

#as excel
setwd("C:/Users/Massfeller/sciebo/PhD/Paper/Paper2_socioSpat/AnalysisPaper2")
FullSample<- read_excel("Data/{current_time}_survey.xlsx")#excel

#remove strings
FullSample[] <- lapply(FullSample, function(x) gsub("[][(),]", "", x)) # check later if this makes sense for all columns

#change all empty cells to NA
FullSample <- FullSample %>% mutate_all(na_if,"")

#create subsample (just for trial purposes) with entries in the coordinate columns
SampleDF<- FullSample[!is.na(FullSample$q4_own),]
#SampleDF<- FullSample[!is.na(FullSample$q5_other),]
#SampleDFcsv <- read.csv("Data_prep/sample.csv")#csv




######################
###let's go spatial###
######################

#install.packages('terra', repos='https://rspatial.r-universe.dev')
#if only one plot/marker was selected, we need to copy the coordinates in order to be able to create a 
#SpatialPointsDataFrame, as it shouldn't make any difference, we just copy it for all

#2-copy corrdinates into oroginal column
SampleDF$coord_own <- SampleDF$q4_own
SampleDF$q4_own <- paste(SampleDF$q4_own,SampleDF$coord_own)

#same for those who only selected one neighbouring field
SampleDF$coord_nei <- SampleDF$q5_other
SampleDF$q5_other <- paste(SampleDF$q5_other,SampleDF$coord_nei)




#manipulating string type, create list with coordinates for own fields
own_fields <- SampleDF$q4_own
own_fields<- chartr("{}lat:lng()'[]","                ", own_fields)
summary(own_fields)
own_fields<-str_split(own_fields, boundary("word"))
str_sub(own_fields[[1]][1])#third coordinate of second farmer (1.coordinate of second field)

#same for neighbours fields
nei_fields <- SampleDF$q5_other
nei_fields<- chartr("{}lat:lng()'[]","                ", nei_fields)
nei_fields<-str_split(nei_fields, boundary("word"))

#get number of fields choosen --> better take q4_len_own / q5_len_other here
#for own fields
#SampleDF$Nr.own_fields <-0
#for(i in seq_along(own_fields)){
 # SampleDF$Nr.own_fields[i]<-length(own_fields[[i]])
#}
#SampleDF$Nr.own_fields <-SampleDF$Nr.own_fields/4

#for neighbouring fields
#SampleDF$Nr.nei_fields <-0
#for(i in seq_along(nei_fields)){
 # SampleDF$Nr.nei_fields[i]<-length(nei_fields[[i]])
#}
#SampleDF$Nr.nei_fields <-SampleDF$Nr.nei_fields/4


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

#create df of coordinates for each farmer for neighbouring fields
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



#Run test regression
SampleDF$q1_adopt <- as.factor(SampleDF$q1_adopt)
SampleDF$q3_info <- as.factor(SampleDF$q3_info)
SampleDF$q6_col1 <- as.factor(SampleDF$q6_col1)
SampleDF$q6_col2 <- as.factor(SampleDF$q6_col2)
SampleDF$q6_col3 <- as.factor(SampleDF$q6_col3)
SampleDF$q7_age <- as.factor(SampleDF$q7_age)
SampleDF$q7_size <- as.factor(SampleDF$q7_size)
SampleDF$q7_farm <- as.factor(SampleDF$q7_farm)
SampleDF$q7_speci_select <- as.factor(SampleDF$q7_speci_select)
SampleDF$q7_AES <- as.factor(SampleDF$q7_AES)
SampleDF$q5_len_other <- as.numeric(SampleDF$q5_len_other)
#remove columns not needed for regression
summary(TestModel2 <- glm(q1_adopt ~ q3_info
                          + meanDist + q5_len_other , 
                          data = SampleDF, family = binomial("probit")))

#q6_col1 + q6_col2 + q6_col3 +q7_speci_select+q7_age 
#+ q7_size + q7_farm  + + q7_AES


par(mfrow=c(2,2))
plot(TestModel2)
vif(TestModel2)


#Todo
#check what happens with those who überspringen, how can the data.frame look like then?
#remove all [] and '

#for 7th of March
#get PLZ/ county for each farm --> R
#get coordinates for demonatrations farms: https://www.storybench.org/geocode-csv-addresses-r/
#get mapping of adivsory regions --> counties --> Enola?
#check for collinearity













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



