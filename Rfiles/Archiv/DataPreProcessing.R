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
library(tidyr)
library(naniar)
library(spData)
library(vtable)
library(readr)
library(sf)
library(ggplot2)
library(stringr)
library(plyr)
library(tibble)
library(mfx)
library(jtools)
library(ggstance)
library(broom.mixed)
library(RColorBrewer)
library(ggmap)
library(tidygeocoder)
library(gtools)
library(rlist)


###load data
#directly as json
json_file <- "https://fruchtfolge.agp.uni-bonn.de/db/survey_anna/_all_docs?include_docs=true"
json_data <- fromJSON(paste(readLines(json_file), collapse=""))

#as csv
FullSample<- read_csv("Data_raw/survey.csv")#csv
zuordnung_plz_ort <- read_excel("Backgrounddata/Geodata/zuordnung_plz_ort.xls")

#remove most strings
FullSample[] <- lapply(FullSample, function(x) gsub("[]'[()]", "", x)) # check later if this makes sense for all columns

#remove test runs
FullSample <- FullSample %>% 
  filter(!grepl('TEST', q8_comment))

FullSample <- FullSample %>% 
  filter(!grepl('Test', q8_comment))


#change all empty cells to NA
FullSample <- FullSample %>% mutate_all(na_if,"")
FullSample$q2_technique <- str_replace( FullSample$q2_technique,"R昼㹣benhacke","Ruebenhacke")
FullSample$q2_technique <- str_replace( FullSample$q2_technique,"Hackger攼㸴t ","Hackgerät")
#exclude all test runs by AM!!!
#FullSample %>% dplyr::select(FullSample,-)
#get columns for each reason of non-adoption
#create subsamples of adopters and non-adopters fro later analysis
l.ReasonsNo<-str_split(FullSample$q2_alt_selection, ',')
df.ReasonsNo<-ldply(l.ReasonsNo, rbind)
colnames(df.ReasonsNo) <- c("R1","R2","R3","R4","R5","R6","R7","R8","R9","R10")
df.ReasonsNo<- df.ReasonsNo[!is.na(df.ReasonsNo$R1),]
#remove comma
FullSample[] <- lapply(FullSample, function(x) gsub(" ,","", x)) # check later if this makes sense for all columns

#use q_4 cache for those who have removed own selection of fields
FullSample[is.na(FullSample$q4_own)&!is.na(FullSample$q4_cached),]$q4_own <-FullSample[is.na(FullSample$q4_own)&!is.na(FullSample$q4_cached),]$q4_cached 

#create subsample (just for trial purposes) with entries in the coordinate columns
SampleDF_allCoord<- FullSample[!is.na(FullSample$q4_own)&!is.na(FullSample$q5_other),]
SampleDF_NeiCoord<- FullSample[!is.na(FullSample$q5_other)&is.na(FullSample$q4_own),]
SampleDF_NoCoord <- FullSample[is.na(FullSample$q5_other)&is.na(FullSample$q4_own),]
SampleDF_NoCoordNoPLZ <- FullSample[is.na(FullSample$q5_other)&is.na(FullSample$q4_own)&is.na(FullSample$q4_alt),]
SampleDF_onlyPLZ <-FullSample[!is.na(FullSample$q4_alt),]
SampleDF_NoDistNoOwnCoord <-FullSample[is.na(FullSample$q5_alt_dist)&is.na(FullSample$q4_own),] #but others coordinates coudl be used to get PLZ & dist.
sum(is.na(FullSample$q4_cached))

N <-nrow(FullSample)
a<- nrow(SampleDF_allCoord)
b <- nrow(SampleDF_NeiCoord)
c <-nrow(SampleDF_onlyPLZ)
d <- nrow(SampleDF_NoCoordNoPLZ)


FullSample_dist <- FullSample[!is.na(FullSample$q5_alt_dist),]
SampleDF_allCoord_dist<- FullSample[!is.na(FullSample$q4_own)&!is.na(FullSample$q5_alt_dist),]
SampleDF_NeiCoord_dist<- FullSample[!is.na(FullSample$q5_other)&is.na(FullSample$q4_own)&!is.na(FullSample$q5_alt_dist),]
SampleDF_NoCoord_dist <- FullSample[is.na(FullSample$q5_other)&is.na(FullSample$q4_own)&!is.na(FullSample$q5_alt_dist),]
SampleDF_NoCoordNoPLZ_dist <- FullSample[is.na(FullSample$q5_other)&is.na(FullSample$q4_own)&is.na(FullSample$q4_alt)&!is.na(FullSample$q5_alt_dist),]
SampleDF_onlyPLZ_dist <-FullSample[!is.na(FullSample$q4_alt)&!is.na(FullSample$q5_alt_dist),]


N_dist <-nrow(FullSample_dist)
a_dist <- nrow(SampleDF_allCoord_dist)
b_dist <- nrow(SampleDF_NeiCoord_dist)
c_dist<-nrow(SampleDF_onlyPLZ_dist)
d_dist <- nrow(SampleDF_NoCoordNoPLZ_dist)



tab1 <- as.table(rbind(c(N,a,b,c,d),c(N_dist,"will be calculated","will be calculated",c_dist,d_dist))) 
dimnames(tab1) <- list(Frequency = c("#","Thereof WithDist"), 
                       Answers = c("FullSample","own+neib.coordinates",
                                   "only neib.coordinates","onlyPLZ","Neither PLZ nor Coord.")) 


tab1


#create subsamples of adopters and non-adopters fro later analysis
Adopters <-FullSample[(FullSample$q1_adopt == 1),]
NonAdopters <-FullSample[(FullSample$q1_adopt == 0),]

#those who haven't selected any fields should also have no category (small mistake in the beginning of the survey)
FullSample[FullSample$q5_alt_fields == 0&!is.na(FullSample$q5_alt_fields),]$q5_alt_dist <-c("6")

######################
###let's go spatial###
######################

#install.packages('terra', repos='https://rspatial.r-universe.dev')
#if only one plot/marker was selected, we need to copy the coordinates in order to be able to create a 
#SpatialPointsDataFrame, as it shouldn't make any difference, we just copy it for all

#2-copy corrdinates into oroginal column
SampleDF_allCoord$coord_own <- SampleDF_allCoord$q4_own
SampleDF_allCoord$q4_own <- paste(SampleDF_allCoord$q4_own,SampleDF_allCoord$coord_own)
SampleDF_allCoord$coord_nei <- SampleDF_allCoord$q5_other
SampleDF_allCoord$q5_other <- paste(SampleDF_allCoord$q5_other,SampleDF_allCoord$coord_nei)

#same for those who only selected one neighbouring field
SampleDF_NeiCoord$coord_nei <- SampleDF_NeiCoord$q5_other
SampleDF_NeiCoord$q5_other <- paste(SampleDF_NeiCoord$q5_other,SampleDF_NeiCoord$coord_nei)




#manipulating string type, create list with coordinates for own fields
own_fields <- SampleDF_allCoord$q4_own 
own_fields<- chartr("{}lat:lng()'[]","                ", own_fields)
summary(own_fields)
own_fields<-str_split(own_fields, boundary("word"))
str_sub(own_fields[[1]][1])#third coordinate of second farmer (1.coordinate of second field)

#same for neighbours fields
nei_fields <- SampleDF_allCoord$q5_other
nei_fields<- chartr("{}lat:lng()'[]","                ", nei_fields)
nei_fields<-str_split(nei_fields, boundary("word"))


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

#problem: centroids and spatial points are saved in a list in another order than in the origonal data set
#in order to prevent that, the centroids should be saved directly in a alist accoridng to the order

df.centroids <- as.data.frame(list.centroids)
df.centr<- names(df.centroids) #column name data frame
cols.x<-grep(".x",df.centr) 
x.centr <- do.call(paste,df.centroids[cols.x])
x.centr<-str_split(x.centr, boundary("word"))
cols.y<-grep(".y",df.centr) 
y.centr <- do.call(paste,df.centroids[cols.y])
y.centr<-str_split(y.centr, boundary("word"))

df.centroids_clean <- data.frame(x.centr,y.centr)
colnames(df.centroids_clean) <- c("Long_Centroid", "Lat_Centroid")

number <- sort(as.character(1:nrow(df.centroids_clean)))

df.centroids_clean <- cbind(df.centroids_clean, number)#now I know which observation belongs to which centroid




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
df.centroids_clean$meanDist <- 0
for(i in seq_along(list.centroids)){
  df.centroids_clean$meanDist[i] <- mean(pointDistance(list.nei_fields[[i]],list.centroids[[i]], lonlat = T)/1000)
}

#no we have the right distance with the right observation

SampleDF_allCoord$number <- rownames(SampleDF_allCoord)
SampleDF_allCoord <- left_join(SampleDF_allCoord, df.centroids_clean, by = "number")


#check manually
######
#SampleDF_allCoord$meanDist_test <- 0
#SampleDF_allCoord$meanDist_test[1] <- mean(pointDistance(list.nei_fields[[1]],list.centroids[[1]], lonlat = T)/1000)
#SampleDF_allCoord$meanDist_test[2] <- mean(pointDistance(list.nei_fields[[2]],list.centroids[[2]], lonlat = T)/1000)
#SampleDF_allCoord$meanDist_test[3] <- mean(pointDistance(list.nei_fields[[3]],list.centroids[[3]], lonlat = T)/1000)
#SampleDF_allCoord$meanDist_test[4] <- mean(pointDistance(list.nei_fields[[4]],list.centroids[[4]], lonlat = T)/1000)
#SampleDF_allCoord$meanDist_test[5] <- mean(pointDistance(list.nei_fields[[5]],list.centroids[[5]], lonlat = T)/1000)
#SampleDF_allCoord$meanDist_test[6] <- mean(pointDistance(list.nei_fields[[6]],list.centroids[[6]], lonlat = T)/1000)
#####

#get plz from coordinates Old = wrong approach
######
#plz <- st_read("Backgrounddata/Geodata/PLZGEO/plz-5stellig.shp")

## pointsDF: A data.frame whose first column contains longitudes and
##           whose second column contains latitudes.
##
## states:   An sf MULTIPOLYGON object with 50 states plus DC. #plz
##
## name_col: Name of a column in `states` that supplies the states' #note
##           names.
#do for farm-centroids
#CentroidsDF <- as.data.frame(list.centroids)
#cdf<- names(CentroidsDF) #column name data frame
#cols.x<-grep(".x",cdf) 
#x <- do.call(paste,CentroidsDF[cols.x])
#x<-str_split(x, boundary("word"))
#cols.y<-grep(".y",cdf) 
#y <- do.call(paste,CentroidsDF[cols.y])
#y<-str_split(y, boundary("word"))

#pointsDF <- data.frame(x,y)
#colnames(pointsDF) <- c("X", "Y")

#lonlat_to_plz <- function(pointsDF,
#                          states = plz,
 #                         name_col = "NAME") {
  ## Convert points data.frame to an sf POINTS object
 # pts <- st_as_sf(pointsDF, coords = 1:2, crs = 4326)
  
  ## Transform spatial data to some planar coordinate system
  ## (e.g. Web Mercator) as required for geometric operations
  #states <- st_transform(states,crs = 3395)
  #pts <- st_transform(pts, crs = 3395)
  
  ## Find names of state (if any) intersected by each point
  #state_names <- states[["plz"]]
  #ii <- as.integer(st_intersects(pts, states))
  #state_names[ii]
#}

#store respective PLZ in SampleDF_allCoord
#SampleDF_allCoord$plz2 <- lonlat_to_plz(pointsDF) #it seems these plz are not right!
#####

#do the same for those who only chose nei. fields
#first create objects
#same for neighbours fields
nei_fields_only <- SampleDF_NeiCoord$q5_other
nei_fields_only<- chartr("{}lat:lng()'[]","                ", nei_fields_only)
nei_fields_only<-str_split(nei_fields_only, boundary("word"))

#then get centroids
#create df of coordinates for each farmer for neighbouring fields
for(i in seq_along(nei_fields_only)){
 coord.nei_fields_only <- coord.as.df(nei_fields_only)
  coord.nei_fields_only<- as.data.frame(sapply(coord.nei_fields_only,as.numeric))
  nei_fields_only.spat <- SpatialPointsDataFrame(coords = coord.nei_fields_only, data = coord.nei_fields_only)
  assign( paste("nei_fields_only.spat", i, sep = "_") , nei_fields_only.spat)
  centroids.nei_fields_only= gCentroid(nei_fields_only.spat)#get also centrods of neighbouring farms
  assign( paste("nei.spat", i, sep = "_") , centroids.nei_fields_only)
}

#put all nei_farms in a list
list.nei_fields_only <- do.call("list",mget(ls(pattern = "^nei_fields_only.spat_.*")))
list.centroids_nei <- do.call("list",mget(ls(pattern = "^nei.spat_.*")))
#problem: centroids and spatial points are saved in a list in another order than in the origonal data set
#in order to prevent that, the centroids should be saved directly in a alist accoridng to the order

df.centroids_nei <- as.data.frame(list.centroids_nei)
df.centr_nei<- names(df.centroids_nei) #column name data frame
cols.x<-grep(".x",df.centr_nei) 
x.centr <- do.call(paste,df.centroids_nei[cols.x])
x.centr<-str_split(x.centr, boundary("word"))
cols.y<-grep(".y",df.centr_nei) 
y.centr <- do.call(paste,df.centroids_nei[cols.y])
y.centr<-str_split(y.centr, boundary("word"))

df.centroids_nei_clean <- data.frame(x.centr,y.centr)
colnames(df.centroids_nei_clean) <- c("Long_Centroid", "Lat_Centroid")

number <- sort(as.character(1:nrow(df.centroids_nei_clean)))

df.centroids_nei_clean <- cbind(df.centroids_nei_clean, number)#now I know which observation belongs to which centroid

#get also mean distance between those fields selected for neighbours
#loop over list and centroids to get distance between centroid and neighbours fields
df.centroids_nei_clean$meanDist_nei <- 0
for(i in seq_along(list.centroids_nei)){
  df.centroids_nei_clean$meanDist_nei[i] <- mean(pointDistance(list.nei_fields_only[[i]],list.centroids_nei[[i]], lonlat = T)/1000)
}
#no we have the right distance with the right observation

SampleDF_NeiCoord$number <- rownames(SampleDF_NeiCoord)
SampleDF_NeiCoord <- left_join(SampleDF_NeiCoord, df.centroids_nei_clean, by = "number")



######
#CentroidsDF_nei <- as.data.frame(list.centroids_nei)
#cdf_nei<- names(CentroidsDF_nei) #column name data frame
#cols.x_nei<-grep(".x",cdf_nei) 
#x_nei <- do.call(paste,CentroidsDF[cols.x_nei])
#x_nei<-str_split(x_nei, boundary("word"))
#cols.y_nei<-grep(".y",cdf_nei) 
#y_nei <- do.call(paste,CentroidsDF[cols.y_nei])
#y_nei<-str_split(y_nei, boundary("word"))

#pointsDF_nei <- data.frame(x_nei,y_nei)
#colnames(pointsDF_nei) <- c("X", "Y")

#lonlat_to_plz <- function(pointsDF_nei,
 ##                         states = plz,
  #                        name_col = "NAME") {
  ## Convert points data.frame to an sf POINTS object
#  pts_nei <- st_as_sf(pointsDF_nei, coords = 1:2, crs = 4326)
  
  ## Transform spatial data to some planar coordinate system
  ## (e.g. Web Mercator) as required for geometric operations
 # states_nei <- st_transform(states,crs = 3857)
#  pts_nei <- st_transform(pts_nei, crs = 3857)
  
  ## Find names of state (if any) intersected by each point
 # state_names <- states[["plz"]]
  #ii <- as.integer(st_intersects(pts_nei, states_nei))
  #state_names[ii]
#}

#store respective PLZ in SampleDF_allCoord
#SampleDF_NeiCoord$plz2_nei <- lonlat_to_plz(pointsDF_nei)
#####



#####
#as PLZ seems to turn out wrong from the approach chossen above we try another way via ggmap/ google API/ tidygeocoder
#####
#API: AIzaSyCGy4X_aWJabtdrvJmhouJl_royB9Mc-PU
#test
# create a dataframe with addresses
#some_addresses <- tibble::tribble(
#    ~name,                  ~addr,
#    "White House",          "1600 Pennsylvania Ave NW, Washington, DC",
#   "Transamerica Pyramid", "600 Montgomery St, San Francisco, CA 94111",     
#    "Willis Tower",         "233 S Wacker Dr, Chicago, IL 60606")

# geocode the addresses
#lat_longs <- some_addresses %>%
 #   geocode(addr, method = 'osm', lat = latitude , long = longitude)

#reverse the geocoding
#reverse <- lat_longs %>%
 # reverse_geocode(lat = latitude, long = longitude, method = 'osm',
  #                address = address_found, full_results = TRUE)
#####
#identify observations that have no postal code given but coordinates

df.Coord <- FullSample[(is.na(FullSample$q4_alt)),]
df.Coord <- df.Coord[(!is.na(df.Coord$q4_cached))|
                        (!is.na(df.Coord$q4_own))|
                         (!is.na(df.Coord$q5_other)),]

#create column where those who have no own but neighbouring corrdinates are also listed
df.Coord$fields <- coalesce(df.Coord$q4_own, df.Coord$q4_cached, df.Coord$q5_other)

#why not take centroids calculated above???

#now we need to get some centroid for those, either from own fields or from neighb. fields
fields <- df.Coord$fields
fields<- chartr("{}lat:lng()'[]","                ", fields)
summary(fields)
fields<-str_split(fields, boundary("word"))
str_sub(own_fields[[1]][1])#third coor
#coord.as.df <- function(x) cbind.data.frame(split(x[[i]], rep(1:2, times=length(x[[i]])/2)), stringsAsFactors=F) 

#create empty data frames
coord.own_fields2 <- data.frame()
Centroids2 <- data.frame()

#create df of coordinates for each farmer for all his/her fields and calculate centroid = farm location
for(i in seq_along(fields)){
  coord.own_fields2 <- coord.as.df(fields) #need zwischenschritt um cooord. als list zu speiichern
  coord.own_fields2<- mutate_all(coord.own_fields2, function(x) as.numeric(x))
  own_fields.spat2 <- SpatialPointsDataFrame(coords = coord.own_fields2, data = coord.own_fields2)
  centroids.farms2= gCentroid(own_fields.spat2)
  assign( paste("farms.spat2", i, sep = "_") , centroids.farms2)
}
list.centroids2 <- do.call("list",mget(ls(pattern = "^farms.spat2.*")))

#save centroids in a data frame
CentroidsFields <- as.data.frame(list.centroids2)
cdf<- names(CentroidsFields) #column name data frame
cols.x<-grep(".x",cdf) 
x <- do.call(paste,CentroidsFields[cols.x])
x<-str_split(x, boundary("word"))
cols.y<-grep(".y",cdf) 
y <- do.call(paste,CentroidsFields[cols.y])
y<-str_split(y, boundary("word"))

pointsDF <- data.frame(x,y)
colnames(pointsDF) <- c("Long_Centroid", "Lat_Centroid")

#add these centroids to existing data set of those without postal code
#df.Coord <- cbind(df.Coord, pointsDF)

#now get the address by reversing the geocoding



reverse_geocoding <- pointsDF %>%
  reverse_geocode(lat = Lat_Centroid, long = Long_Centroid, method = 'osm',
                  address = "address", full_results = TRUE)

rev_geo <- reverse_geocoding %>% dplyr::select(Long_Centroid, Lat_Centroid,county, state, postcode)

#add info to allcoords df now
df.Coord <- cbind(df.Coord, rev_geo)
df.Coord <- df.Coord %>% dplyr::select(date, Long_Centroid, Lat_Centroid, county, state, postcode)

#store plz in old dataset 
#SampleDF_NeiCoord$plz2_nei <- df.Coord$postcode


##add mean distance calculated from coordinates to full sample an dput it in same categories
MeanDistDF <- SampleDF_allCoord %>% dplyr::select(date,meanDist)
MeanDistDF_nei <- SampleDF_NeiCoord %>% dplyr::select(date,meanDist_nei)
FullSample <- left_join(FullSample,MeanDistDF,by="date", copy = TRUE)
FullSample <- left_join(FullSample,MeanDistDF_nei,by="date", copy = TRUE)
FullSample$meanDist <- coalesce(FullSample$meanDist, FullSample$meanDist_nei)
FullSample$meanDist_cat <- FullSample$meanDist
FullSample$meanDist_cat[FullSample$meanDist_cat >=0 & FullSample$meanDist_cat <= 5] = 0
FullSample$meanDist_cat[FullSample$meanDist_cat >5 & FullSample$meanDist_cat <= 10] = 1
FullSample$meanDist_cat[FullSample$meanDist_cat >10 & FullSample$meanDist_cat <=15] = 2
FullSample$meanDist_cat[FullSample$meanDist_cat >15 & FullSample$meanDist_cat <= 20] = 3
FullSample$meanDist_cat[FullSample$meanDist_cat >20 & FullSample$meanDist_cat <= 30] = 4
FullSample$meanDist_cat[FullSample$meanDist_cat >30] = 5
FullSample$meanDist_cat <- as.character(FullSample$meanDist_cat)
FullSample$FieldDist <- coalesce(FullSample$q5_alt_dist, FullSample$meanDist_cat)

#same for number of fields
#q5_len_other & q5_alt_fields
#those who haven't selected any fields via map get 0
FullSample$q5_len_other <- as.numeric(FullSample$q5_len_other)
FullSample$q5_len_other_cat <- FullSample$q5_len_other
FullSample$q5_len_other_cat[FullSample$q5_len_other_cat ==0] = 0
FullSample$q5_len_other_cat[FullSample$q5_len_other_cat >=1 & FullSample$q5_len_other_cat <= 5] = 1
FullSample$q5_len_other_cat[FullSample$q5_len_other_cat >=6 & FullSample$q5_len_other_cat <=10] = 2
FullSample$q5_len_other_cat[FullSample$q5_len_other_cat >=11 & FullSample$q5_len_other_cat <=15] = 3
FullSample$q5_len_other_cat[FullSample$q5_len_other_cat >= 15 ] = 4
FullSample$q5_len_other_cat <- as.character(FullSample$q5_len_other_cat)
FullSample$NrFields <- coalesce(FullSample$q5_alt_fields, FullSample$q5_len_other_cat)
#those who haven't selected any fields should also have no category (small mistake in the beginning of the survey)
FullSample[FullSample$q5_alt_fields == 0&!is.na(FullSample$q5_alt_fields),]$q5_alt_dist <-c("6")

#map postal code and counties

#1. add information for those who have only coordinates
FullSample <- left_join(FullSample, df.Coord, by = "date")
#2. coalesce info from those who indicated postal code directly in the survey
FullSample$plz <- FullSample$postcode
FullSample$plz <- coalesce(FullSample$plz, FullSample$q4_alt)
#FullSample <- dplyr::rename(FullSample, plz1_own = q4_alt)
#FullSample$plz1_own <- as.numeric(FullSample$plz1_own)
#FullSample$plz2 <- as.numeric(FullSample$plz2)
#FullSample$plz2_nei <- as.numeric(FullSample$plz2_nei)

#first merge all postal codes in one column
#FullSample$plz <- coalesce(FullSample$plz1_own, FullSample$plz2,FullSample$plz2_nei)


plz_lkr <- zuordnung_plz_ort %>% dplyr::select(plz, landkreis,bundesland)
plz_lkr$plz <- as.character(plz_lkr$plz)
FullSample$plz <- as.character(FullSample$plz)
FullSample <- left_join(FullSample,plz_lkr,by = "plz")#here observations in Full sample become irrationally more
FullSample<-FullSample[!duplicated(FullSample), ]

#this observation remained double
FullSample<- FullSample[-c(146),]

#make one final column for county and one for federal state
FullSample$landkreis <- coalesce(FullSample$landkreis, FullSample$county)
FullSample$bundesland <- coalesce(FullSample$bundesland, FullSample$state)


#add manually where landkreis is missing

FullSample$landkreis <- ifelse(FullSample$plz == "38229", "Salzgitter",FullSample$landkreis) 
FullSample$landkreis <- ifelse(FullSample$plz == "51147", "Koeln",FullSample$landkreis) 
FullSample$landkreis <- ifelse(FullSample$plz == "59069", "Hamm",FullSample$landkreis) 
FullSample$landkreis <- ifelse(FullSample$plz == "60431", "Frankfurt am Main",FullSample$landkreis) 
FullSample$landkreis <- ifelse(FullSample$plz == "64291", "Darmstadt",FullSample$landkreis) 
FullSample$landkreis <- ifelse(FullSample$plz == "93055", "Regensburg",FullSample$landkreis) 
FullSample$landkreis <- coalesce(FullSample$landkreis, FullSample$county) #somehow needs to be done again
FullSample[FullSample$date == "2022-04-25 11:44:43",]$landkreis <- "Guetersloh"
FullSample[FullSample$date == "2022-04-25 11:44:43" & FullSample$landkreis == "Guetersloh",]$plz <- "33824"
FullSample[FullSample$date == "2022-04-25 11:44:43" & FullSample$landkreis == "Guetersloh",]$bundesland <- "Nordrhein-Westfalen"

#create binary variables for prelimnary regression
FullSample$farmsize_b <- FullSample$q7_size
FullSample$farmsize_b <- as.numeric(FullSample$farmsize_b)
FullSample$farmsize_b <- ifelse(FullSample$farmsize_b <= 3,0,1)#alle unter 49 = 0

FullSample$age_b <- FullSample$q7_age
FullSample$age_b <-  na_if(FullSample$age_b,"6")
FullSample$age_b <- as.numeric(FullSample$age_b)
FullSample$age_b <- ifelse(FullSample$age_b <= 2,0,1)#alle unter 44 = 0

FullSample$dist_b <- FullSample$FieldDist
FullSample$dist_b <- as.numeric(FullSample$dist_b)
FullSample$dist_b <-  na_if(FullSample$dist_b,"7")
FullSample$dist_b <- ifelse(FullSample$dist_b <= 2,0,1)#0 = 0-15
FullSample$dist_b[is.na(FullSample$dist_b)] <- 2

FullSample$AES_b <- FullSample$q7_AES
FullSample$AES_b <- as.numeric(FullSample$AES_b)
#FullSample$AES_b <-  na_if(FullSample$AES_b,"2")
FullSample$AES_b[FullSample$AES_b == 2] <-0 #not really correct, those who gave no Info are set to "yes"

FullSample$age_b <- as.factor(FullSample$age_b)
FullSample$farmsize_b <- as.factor(FullSample$farmsize_b)
FullSample$AES_b <- as.factor(FullSample$AES_b)
FullSample$dist_b <- as.factor(FullSample$dist_b)


#match öko data from zensus_spi with existing data set
#1.run zensus_api file
#source("C:/Users/Massfeller/sciebo/PhD/Paper/Paper2_socioSpat/SurveyAnalysis/Rfiles/zensus_api.R")
#subset d.full from zensus-api to what we need
Organic_lkr <- d.full[d.full$JAHR == "2016",] %>% dplyr::select(2,5,6)
FullSample <- dplyr::rename(FullSample, Kreis = landkreis)

#need to check for differences, eg. Kreis Höxter vs. Höxter,Kreis
table(FullSample$Kreis %in% Organic_lkr$Kreis)

df.lkr_names <- cbind(FullSample$Kreis, Organic_lkr$Kreis)#ignore warning
FullSample$Kreis <- lapply(FullSample$Kreis, function(x) gsub("Kreis ", "", x))
FullSample$Kreis <- lapply(FullSample$Kreis, function(x) gsub("Landkreis ", "", x))
FullSample$Kreis <- lapply(FullSample$Kreis, function(x) gsub("ö", "oe", x))
FullSample$Kreis <- lapply(FullSample$Kreis, function(x) gsub("ä", "ae", x))
FullSample$Kreis <- lapply(FullSample$Kreis, function(x) gsub("ü", "ue", x))
FullSample$Kreis <- lapply(FullSample$Kreis, function(x) gsub("Staedteregion Aachen", "Staedteregion Aachen (einschl. Stadt Aachen)", x))

Organic_lkr$Kreis <- lapply(Organic_lkr$Kreis, function(x) gsub(", Kreis", "", x))
Organic_lkr$Kreis <- lapply(Organic_lkr$Kreis, function(x) gsub(", Landkreis", "", x))
Organic_lkr$Kreis <- lapply(Organic_lkr$Kreis, function(x) gsub(", kreisfreie Stadt", "", x))
#Organic_lkr$Kreis <- lapply(Organic_lkr$Kreis, function(x) gsub("a.d.", "an der ", x))
Organic_lkr$Kreis <- lapply(Organic_lkr$Kreis, function(x) gsub("ö", "oe", x))
Organic_lkr$Kreis <- lapply(Organic_lkr$Kreis, function(x) gsub("ä", "ae", x))
Organic_lkr$Kreis <- lapply(Organic_lkr$Kreis, function(x) gsub("ü", "ue", x))
Organic_lkr$Kreis <- lapply(Organic_lkr$Kreis, function(x) gsub("Rhein-Kreis Neuss", "Rhein-Neuss", x))
Organic_lkr$Kreis <- lapply(Organic_lkr$Kreis, function(x) gsub("Neustadt a.d.Aisch-Bad Windsheim", "Neustadt an der Aisch-Bad Windsheim", x))
Organic_lkr$Kreis <- lapply(Organic_lkr$Kreis, function(x) gsub("Luebeck, Hansestadt", "Luebeck", x))
Organic_lkr$Kreis <- lapply(Organic_lkr$Kreis, function(x) gsub("Dillingen a.d.Donau", "Dillingen an der Donau", x))
Organic_lkr$Kreis <- lapply(Organic_lkr$Kreis, function(x) gsub("Pfaffenhofen a.d.Ilm", "Pfaffenhofen an der Ilm", x))



table(FullSample$Kreis %in% Organic_lkr$Kreis)

FullSample_org <- left_join(FullSample,Organic_lkr,by="Kreis")



#Todo
#for 30th of May
#get coordinates for demonatrations farms: https://www.storybench.org/geocode-csv-addresses-r/
#get mapping of adivsory regions --> counties --> Enola? #first, use map and 4 large Produzenten
#for those only PLZ: get centtroid of PLZ-area --> check excel file

#identify farms that have postal code but no coordinates







#get shape files for PLZ are to get their centroid
#library(sp)
#library(rgdal)
#library(sf)
#library(sp)
#library(RColorBrewer)
#library(geojsonio)
#gadm36_DEU_2_sp <- readRDS("Data_prep/gadm36_DEU_2_sp.rds")
# get spatial data for Germany on county level
#county <- gadm36_DEU_2_sp
#par(mfrow=c(1,1))
#plot(county, col ='grey')

#get shapes of PLZ area
#ger_plz <- st_read("C:/Users/Massfeller/sciebo/PhD/Paper/Paper2_socioSpat/AnalysisPaper2/Data_prep/PLZGEO/plz-5stellig.shp")
#plot(ger_plz$geometry)
#shp.plz <- geojson_read("C:/Users/Massfeller/sciebo/PhD/Paper/Paper2_socioSpat/AnalysisPaper2/Data_prep/plz-5stellig.geojson",  what = "sp")
#plot(shp.plz)

#access centroid
#shp.plz@polygons[[i]]@labpt
#l.PLZ<-shp.plz@polygons
#df.PLZ <- matrix(NA,2,length(l.PLZ))
#df.PLZ <- as.data.frame((df.PLZ))

#loop to get df
#for(i in seq_along(i:length(l.PLZ))){
#  df.PLZ[,i] <- as.data.frame(l.PLZ[[i]]@labpt)
#}

  
#df.PLZ_t <- t(df.PLZ)
#df.PLZ_t <- as.data.frame(df.PLZ_t)
#colnames(df.PLZ_t) <- c("long", "lat")
#df.PLZ_t <- df.PLZ_t[-8170,]
  
#PLZ.spat <- SpatialPointsDataFrame(coords = df.PLZ_t, data = df.PLZ_t)
#l.PLZ_num<- shp.plz@data$plz

#PLZ_full <-cbind(df.PLZ_t, l.PLZ_num)

#df.centroidsPLZ <- subset(FullSample, select = c(date,plz))
#df.centroidsPLZ <- left_join(df.centroids,PLZ_full, by = c("plz" = "l.PLZ_num"))
#df.centroidsPLZ <- df.centroidsPLZ[!is.na(df.centroidsPLZ$plz),]
#df.centroidsPLZ <- subset(df.centroidsPLZ, select = c(date,plz,long.x,lat.x))

#do the same as for subsets of only nei fields or own fields



#plot(list.nei_fields_only[[2]])
#plot(list.centroids_nei[[2]],add = TRUE)
#plot(PLZ.spat)

