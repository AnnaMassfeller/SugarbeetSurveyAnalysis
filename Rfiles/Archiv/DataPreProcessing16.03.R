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


###load data
#directly as json
json_file <- "https://fruchtfolge.agp.uni-bonn.de/db/survey_anna/_all_docs?include_docs=true"
json_data <- fromJSON(paste(readLines(json_file), collapse=""))

#as csv
setwd("C:/Users/Massfeller/sciebo/PhD/Paper/Paper2_socioSpat/AnalysisPaper2")
FullSample<- read_csv("Data/survey.csv")#csv
zuordnung_plz_ort <- read_excel("Data_prep/zuordnung_plz_ort.xls")

#remove most strings
FullSample[] <- lapply(FullSample, function(x) gsub("[]'[()]", "", x)) # check later if this makes sense for all columns

#change all empty cells to NA
FullSample <- FullSample %>% mutate_all(na_if,"")
FullSample$q2_technique <- str_replace( FullSample$q2_technique,"R昼㹣benhacke","Ruebenhacke")
#exclude all test runs by AM!!!
#FullSample %>% dplyr::select(FullSample,-)
#get columns for each reason of non-adoption
#create subsamples of adopters and non-adopters fro later analysis
l.ReasonsNo<-str_split(FullSample$q2_alt_selection, ',')
df.ReasonsNo<-ldply(l.ReasonsNo, rbind)
colnames(df.ReasonsNo) <- c("R1","R2","R3","R4","R5","R6","R7","R8")
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

#get number of fields choosen --> better take q4_len_own / q5_len_other here
#for own fields
#SampleDF_allCoord$Nr.own_fields <-0
#for(i in seq_along(own_fields)){
# SampleDF_allCoord$Nr.own_fields[i]<-length(own_fields[[i]])
#}
#SampleDF_allCoord$Nr.own_fields <-SampleDF_allCoord$Nr.own_fields/4

#for neighbouring fields
#SampleDF_allCoord$Nr.nei_fields <-0
#for(i in seq_along(nei_fields)){
# SampleDF_allCoord$Nr.nei_fields[i]<-length(nei_fields[[i]])
#}
#SampleDF_allCoord$Nr.nei_fields <-SampleDF_allCoord$Nr.nei_fields/4


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
SampleDF_allCoord$meanDist <- 0
for(i in seq_along(list.centroids)){
  SampleDF_allCoord$meanDist[i] <- mean(pointDistance(list.nei_fields[[i]],list.centroids[[i]], lonlat = T)/1000)
}


#check manually
#SampleDF_allCoord$meanDist_test <- 0
#SampleDF_allCoord$meanDist_test[1] <- mean(pointDistance(list.nei_fields[[1]],list.centroids[[1]], lonlat = T)/1000)
#SampleDF_allCoord$meanDist_test[2] <- mean(pointDistance(list.nei_fields[[2]],list.centroids[[2]], lonlat = T)/1000)
#SampleDF_allCoord$meanDist_test[3] <- mean(pointDistance(list.nei_fields[[3]],list.centroids[[3]], lonlat = T)/1000)
#SampleDF_allCoord$meanDist_test[4] <- mean(pointDistance(list.nei_fields[[4]],list.centroids[[4]], lonlat = T)/1000)
#SampleDF_allCoord$meanDist_test[5] <- mean(pointDistance(list.nei_fields[[5]],list.centroids[[5]], lonlat = T)/1000)
#SampleDF_allCoord$meanDist_test[6] <- mean(pointDistance(list.nei_fields[[6]],list.centroids[[6]], lonlat = T)/1000)
#get plz from coordinates
plz <- st_read("C:/Users/Massfeller/sciebo/PhD/Paper/Paper2_socioSpat/AnalysisPaper2/Data_prep/PLZGEO/plz-5stellig.shp")

## pointsDF: A data.frame whose first column contains longitudes and
##           whose second column contains latitudes.
##
## states:   An sf MULTIPOLYGON object with 50 states plus DC. #plz
##
## name_col: Name of a column in `states` that supplies the states' #note
##           names.
#do for farm-centroids
CentroidsDF <- as.data.frame(list.centroids)
cdf<- names(CentroidsDF) #column name data frame
cols.x<-grep(".x",cdf) 
x <- do.call(paste,CentroidsDF[cols.x])
x<-str_split(x, boundary("word"))
cols.y<-grep(".y",cdf) 
y <- do.call(paste,CentroidsDF[cols.y])
y<-str_split(y, boundary("word"))

pointsDF <- data.frame(x,y)
colnames(pointsDF) <- c("X", "Y")

lonlat_to_plz <- function(pointsDF,
                          states = plz,
                          name_col = "NAME") {
  ## Convert points data.frame to an sf POINTS object
  pts <- st_as_sf(pointsDF, coords = 1:2, crs = 4326)
  
  ## Transform spatial data to some planar coordinate system
  ## (e.g. Web Mercator) as required for geometric operations
  states <- st_transform(states,crs = 3857)
  pts <- st_transform(pts, crs = 3857)
  
  ## Find names of state (if any) intersected by each point
  state_names <- states[["plz"]]
  ii <- as.integer(st_intersects(pts, states))
  state_names[ii]
}

#store respective PLZ in SampleDF_allCoord
SampleDF_allCoord$plz2 <- lonlat_to_plz(pointsDF)

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

CentroidsDF_nei <- as.data.frame(list.centroids_nei)
cdf_nei<- names(CentroidsDF_nei) #column name data frame
cols.x_nei<-grep(".x",cdf_nei) 
x_nei <- do.call(paste,CentroidsDF[cols.x_nei])
x_nei<-str_split(x_nei, boundary("word"))
cols.y_nei<-grep(".y",cdf_nei) 
y_nei <- do.call(paste,CentroidsDF[cols.y_nei])
y_nei<-str_split(y_nei, boundary("word"))

pointsDF_nei <- data.frame(x_nei,y_nei)
colnames(pointsDF_nei) <- c("X", "Y")

lonlat_to_plz <- function(pointsDF_nei,
                          states = plz,
                          name_col = "NAME") {
  ## Convert points data.frame to an sf POINTS object
  pts_nei <- st_as_sf(pointsDF_nei, coords = 1:2, crs = 4326)
  
  ## Transform spatial data to some planar coordinate system
  ## (e.g. Web Mercator) as required for geometric operations
  states_nei <- st_transform(states,crs = 3857)
  pts_nei <- st_transform(pts_nei, crs = 3857)
  
  ## Find names of state (if any) intersected by each point
  state_names <- states[["plz"]]
  ii <- as.integer(st_intersects(pts_nei, states_nei))
  state_names[ii]
}

#store respective PLZ in SampleDF_allCoord
SampleDF_NeiCoord$plz2_nei <- lonlat_to_plz(pointsDF_nei)

#get also mean distance between those fields selected for neighbours
#loop over list and centroids to get distance between centroid and neighbours fields
SampleDF_NeiCoord$meanDist_nei <- 0
for(i in seq_along(list.centroids_nei)){
  SampleDF_NeiCoord$meanDist_nei[i] <- mean(pointDistance(list.nei_fields_only[[i]],list.centroids_nei[[i]], lonlat = T)/1000)
}


##add mean distance calculated from coordinates to full sample an dput it in same categories
MeanDistDF <- SampleDF_allCoord %>% dplyr::select(date,meanDist,plz2)
MeanDistDF_nei <- SampleDF_NeiCoord %>% dplyr::select(date,meanDist_nei,plz2_nei)
FullSample <- left_join(FullSample,MeanDistDF,by="date", copy = TRUE)
FullSample <- left_join(FullSample,MeanDistDF_nei,by="date", copy = TRUE)
FullSample$meanDist <- coalesce(FullSample$meanDist, FullSample$meanDist_nei)
FullSample$meanDist_cat <- FullSample$meanDist
FullSample$meanDist_cat[FullSample$meanDist_cat >=0 & FullSample$meanDist_cat <= 5] = 0
FullSample$meanDist_cat[FullSample$meanDist_cat >=6 & FullSample$meanDist_cat <= 10] = 1
FullSample$meanDist_cat[FullSample$meanDist_cat >=11 & FullSample$meanDist_cat <=15] = 2
FullSample$meanDist_cat[FullSample$meanDist_cat >=16 & FullSample$meanDist_cat <= 20] = 3
FullSample$meanDist_cat[FullSample$meanDist_cat >=21 & FullSample$meanDist_cat <= 30] = 4
FullSample$meanDist_cat[FullSample$meanDist_cat >=30] = 5
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

#map postal code and counties
FullSample <- dplyr::rename(FullSample, plz1_own = q4_alt)
FullSample$plz1_own <- as.numeric(FullSample$plz1_own)
FullSample$plz2 <- as.numeric(FullSample$plz2)
FullSample$plz2_nei <- as.numeric(FullSample$plz2_nei)
#first merge all postal codes in one column
FullSample$plz <- coalesce(FullSample$plz1_own, FullSample$plz2,FullSample$plz2_nei)
plz_lkr <- zuordnung_plz_ort %>% dplyr::select(plz, landkreis,bundesland)
plz_lkr$plz <- as.numeric(plz_lkr$plz)
FullSample$plz <- as.numeric(FullSample$plz)
FullSample <- left_join(FullSample,plz_lkr,by = "plz")#here observations in Full sample become irrationally more
FullSample<-FullSample[!duplicated(FullSample), ]

#those who haven't selected any fields should also have no category (small mistake in the beginning of the survey)
FullSample[FullSample$q5_alt_fields == 0&!is.na(FullSample$q5_alt_fields),]$q5_alt_dist <-c("6")

########################
#get some descriptives##
#######################
summary(FullSample)
str(FullSample)
FullSample$plz <- as.factor(FullSample$plz)
FullSample$q1_adopt <- as.factor(FullSample$q1_adopt)
FullSample$FieldDist <- as.factor(FullSample$FieldDist)
#table(FullSample$q1_adopt)
st(FullSample, vars = c("q1_adopt","q3_info","plz","NrFields","FieldDist","q6_col1","q6_col2","q6_col3","q7_age", "q7_size", "q7_farm","q7_speci_select", "q7_AES"))
#, group = "'q1_adopt")

#stargazer(FullSample[c("q1_adopt","q3_info","plz","q5_len_other","FieldDist","q5_alt_fields","q6_col1","q6_col2","q6_col3","q7_age", "q7_size", "q7_farm","q7_speci_select", "q7_AES")], type = "text", 
#         title="Descriptive statistics/selected variables", digits=1, out="table.txt")

#let's show some graphs
g.adopt <- ggplot(FullSample, aes(q1_adopt))+geom_histogram(binwidth = 5, stat = "count")+labs(x = "Adoption", y = "Count",
        title ="Adoption of mechanical weeding")+scale_x_discrete(labels = c("No", "Yes"))
g.adopt

g.info <- ggplot(FullSample, aes(q3_info))+
  geom_histogram(binwidth = 5, stat = "count")+
  labs(x = "Nr. of adopters known", y = "Count")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_x_discrete(labels = c("0", "1-5","6-10","more than 10"))
g.info

g.NrFields <- ggplot(FullSample, aes(NrFields))+
  geom_histogram(binwidth = 5, stat = "count")+
  labs(x = "Nr. of fields", y = "Count")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_x_discrete(labels = c("0", "1-5","6-10","11-15","more than 15"))
g.NrFields

g.FieldDist <- ggplot(FullSample, aes(FieldDist))+
  geom_histogram(binwidth = 5, stat = "count")+
  labs(x = "Distance in km", y = "Count")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_x_discrete(labels = c("0-5", "6-10","11-15","16-20","21-30","more than 30","no fields observed"))
g.FieldDist

g.Intent.Mech_trad <- ggplot(FullSample, aes(q6_col1))+
  geom_histogram(binwidth = 5, stat = "count")+
  labs(x = "Intention", y = "Count",title ="Intention to use trad. mechanical weeding in the future")+
  scale_x_discrete(labels = c("no Intention", "low","middle","high","already in use"))
g.Intent.Mech_trad

g.Intent.Mech_modern <- ggplot(FullSample, aes(q6_col2))+
  geom_histogram(binwidth = 5, stat = "count")+
  labs(x = "Intention", y = "Count",title ="Intention to use modern mechanical weeding in the future")+
  scale_x_discrete(labels = c("no Intention", "low","middle","high","already in use"))
g.Intent.Mech_modern

g.Intent.Mech_auton <- ggplot(FullSample, aes(q6_col3))+
  geom_histogram(binwidth = 5, stat = "count")+
  labs(x = "Intention", y = "Count",title ="Intention to use autonomous mechanical weeding in the future")+
  scale_x_discrete(labels = c("no Intention", "low","middle","high","already in use"))
g.Intent.Mech_auton

g.Age <- ggplot(FullSample, aes(q7_age))+
  geom_histogram(binwidth = 5, stat = "count")+
  labs(x = "Age in years", y = "Count",title ="Participants' age")+
  scale_x_discrete(labels = c("15-24", "25-34","35-44","45-54","55-64","65 and more","no Info"))
g.Age

g.Farmsize <- ggplot(FullSample, aes(q7_size))+
  geom_histogram(binwidth = 5, stat = "count")+
  labs(x = "Farmsize in hectar", y = "Count",title ="Farm Size")+
  scale_x_discrete(labels = c("less than 5", "5-9","10-19","20-49","50-99","100-199","200-499","500-999","1000 and more", "no Info"))
g.Farmsize

g.Specialization <- ggplot(FullSample, aes(q7_speci_select))+
  geom_histogram(binwidth = 5, stat = "count")+
  labs(x = "Main specialization", y = "Count",title ="Farm Specialization")+
  scale_x_discrete(labels = c("crop production", "livestock","special crops","mixed","no Info","others"))
g.Specialization

g.organic <- ggplot(FullSample, aes(q7_farm))+
  geom_histogram(binwidth = 5, stat = "count")+
  labs(x = "farm type", y = "Count",title ="type")+
  scale_x_discrete(labels = c("conventional", "fully organic","crop production organic"))
g.organic

g.AES <- ggplot(FullSample, aes(q7_AES))+
  geom_histogram(binwidth = 5, stat = "count")+
  labs(x = "participation", y = "Count",title ="AES participation")+
  scale_x_discrete(labels = c("yes", "no","no Info"))
g.AES

g.bundesland <- ggplot(FullSample, aes(bundesland))+
  geom_histogram(binwidth = 5, stat = "count")+
  labs(x = "state", y = "Count",title ="Federal state")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
g.bundesland

g.tech <- ggplot(Adopters, aes(q2_technique))+
  geom_histogram(binwidth = 5, stat = "count")+
  labs(x = "technique", y = "Count",title ="Techniques in use")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
g.tech

g.tech_time <- ggplot(Adopters, aes(q2_timeframe))+
  geom_histogram(binwidth = 5, stat = "count")+
  labs(x = "year of adoption", y = "Count",title ="Techniques in use")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
g.tech_time

g.tech_over_time <- ggplot(Adopters, aes(q2_timeframe, fill = q2_technique))+
  geom_bar(position = "stack")+
  theme_gray()+
  labs(x = "year of adoption", y = "Count")+
  scale_fill_brewer(name = "Technique:", labels = c("Combination hoe-bandsprayer", "Rotorharrow", "Beet hoe", "Coulter hoe"),palette = "Spectral")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "bottom")
g.tech_over_time

#reasons for non-adoption
df.ReasonsNo[is.na(df.ReasonsNo)] <- "99"
df.ReasonsNo[] <- lapply(df.ReasonsNo, function(x) gsub(" ","", x)) 
df.ReasonsNo$R1 <- as.numeric(df.ReasonsNo$R1)
df.ReasonsNo$R2 <- as.numeric(df.ReasonsNo$R2)
df.ReasonsNo$R3 <- as.numeric(df.ReasonsNo$R3)
df.ReasonsNo$R4 <- as.numeric(df.ReasonsNo$R4)
df.ReasonsNo$R5 <- as.numeric(df.ReasonsNo$R5)
df.ReasonsNo$R6 <- as.numeric(df.ReasonsNo$R6)
df.ReasonsNo$R7 <- as.numeric(df.ReasonsNo$R7)
df.ReasonsNo$R8 <- as.numeric(df.ReasonsNo$R8)
#df.ReasonsNo <-  str_replace_all(df.ReasonsNo," ","")
#Freq.ReasonsNo<-as.data.frame(table(df.ReasonsNo$R1))
ldply(df.ReasonsNo, function(c) sum(c =="1"))
lvls <- unique(unlist(df.ReasonsNo))
# apply the summation per value 
freq <- sapply(df.ReasonsNo, 
               function(x) table(factor(x, levels = lvls, 
                                        ordered = TRUE)))
print(freq)
freq.ReasonsNo <- as.data.frame(freq)
freq.ReasonsNo <-  na_if(freq.ReasonsNo,"99")
freq.ReasonsNo$sum <-apply(freq.ReasonsNo,1,sum,na.rm = TRUE)
freq.ReasonsNo$sum <- freq.ReasonsNo$R1+freq.ReasonsNo$R2+freq.ReasonsNo$R3+freq.ReasonsNo$R4+freq.ReasonsNo$R5+freq.ReasonsNo$R6+freq.ReasonsNo$R7+freq.ReasonsNo$R8
freq.ReasonsNo <- tibble::rownames_to_column(freq.ReasonsNo, "Nr.Reason")
freq.ReasonsNo<- freq.ReasonsNo %>% filter(freq.ReasonsNo$Nr.Reason != 99)
freq.ReasonsNo$Nr.Reason <- as.numeric(freq.ReasonsNo$Nr.Reason)

freq.ReasonsNo$Reason <- freq.ReasonsNo$Nr.Reason
freq.ReasonsNo["Reason"][freq.ReasonsNo["Reason"] == "0"] <- "running costs"
freq.ReasonsNo["Reason"][freq.ReasonsNo["Reason"] == "1"] <- "investment costs"
freq.ReasonsNo["Reason"][freq.ReasonsNo["Reason"] == "2"] <- "time constraints"
freq.ReasonsNo["Reason"][freq.ReasonsNo["Reason"] == "3"] <- "low reliability"
freq.ReasonsNo["Reason"][freq.ReasonsNo["Reason"] == "4"] <- "high risk of damaging the crop"
freq.ReasonsNo["Reason"][freq.ReasonsNo["Reason"] == "5"] <- "not possible on my farm "
freq.ReasonsNo["Reason"][freq.ReasonsNo["Reason"] == "6"] <- "I don’t know if the technology works for me"
freq.ReasonsNo["Reason"][freq.ReasonsNo["Reason"] == "7"] <- "I don’t trust the application"
freq.ReasonsNo["Reason"][freq.ReasonsNo["Reason"] == "8"] <- "colleagues'bad experiences"
freq.ReasonsNo["Reason"][freq.ReasonsNo["Reason"] == "9"] <- "I don’t know any colleagues"
freq.ReasonsNo["Reason"][freq.ReasonsNo["Reason"] == "10"] <- "wait until the technology is more mature"
freq.ReasonsNo["Reason"][freq.ReasonsNo["Reason"] == "11"] <- "no reason for me to change cultivation"
#freq.ReasonNo$Reason <-arrange(desc(sum))
g.ReasonsNo <- ggplot(freq.ReasonsNo,aes(x=reorder(factor(Reason),-sum),y=sum))+
  geom_col(color='black',fill='cyan3')+
  labs(x = "Reason", y = "Count",title ="Reasons for Non-adoption")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "bottom")
g.ReasonsNo

#overview Intentions
s <- ggplot(FullSample, aes(q6_col1, fill = q6_col2))+ 
  geom_bar(position = "dodge")
s
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

#Run test regression
FullSample$q1_adopt <- as.factor(FullSample$q1_adopt)
FullSample$q3_info <- as.factor(FullSample$q3_info)
FullSample$FieldDist <- as.factor(FullSample$FieldDist)
FullSample$q6_col1 <- as.factor(FullSample$q6_col1)
FullSample$q6_col2 <- as.factor(FullSample$q6_col2)
FullSample$q6_col3 <- as.factor(FullSample$q6_col3)
FullSample$q7_age <- as.factor(FullSample$q7_age)
FullSample$q7_size <- as.factor(FullSample$q7_size)
FullSample$q7_farm <- as.factor(FullSample$q7_farm)
FullSample$q7_speci_select <- as.factor(FullSample$q7_speci_select)
FullSample$q7_AES <- as.factor(FullSample$q7_AES)
FullSample$q5_len_other <- as.numeric(FullSample$q5_len_other)

#remove columns not needed for regression
summary(m.FieldPeer <- glm(q1_adopt ~ q3_info + NrFields , 
                          data = FullSample, family = binomial("probit")))
stargazer(m.FieldPeer, type = "text", out = "FieldPeer.doc", star.char = c("*", "**", "***"), star.cutoffs = c(0.1, 0.05, 0.01),
          dep.var.caption  = "Mechanical weeding yes/no",
            covariate.labels = c("1-5 adopters known","6-10 adopters known","more than 10 adopters known",
                                 "1-5 fields observed","6-10 fields observed","11-15 fields observed","more than 15 fields observed"),
            dep.var.labels   = "", single.row=TRUE)
m.FieldPeer_mfx <- probitmfx(m.FieldPeer, data = FullSample)
m.FieldPeer_mfx

summary(m.NrFields <- glm(q1_adopt ~  NrFields, 
                          data = FullSample, family = binomial("probit"))) #ref.category: 0 = 0 fields
m.NrFields_mfx <-probitmfx(m.NrFields, data = FullSample) 
m.NrFields_mfx

summary(m.NrPeers <- glm(q1_adopt ~  q3_info, 
                          data = FullSample, family = binomial("probit")))#ref-category:0 = 0 peers

m.NrPeers_mfx <-probitmfx(m.NrPeers, data = FullSample)
m.NrPeers_mfx

summary(m.Full <- glm(q1_adopt ~  q3_info + NrFields + age_b + farmsize_b + AES_b, 
                         data = FullSample, family = binomial("probit")))#ref-category:0 = 0 peers

m.Full_mfx <-probitmfx(m.Full, data = FullSample)
m.Full_mfx

m.Full.table <- stargazer(m.Full, m.Full_mfx$fit,
                        coef = list(NULL, m.Full_mfx$mfxest[,1]),
                        se = list(NULL, m.Full_mfx$mfxest[,2]),
                        type = "html", out = "m.Full.html", star.char = c("*", "**", "***"), star.cutoffs = c(0.1, 0.05, 0.01),
                        dep.var.caption  = "Mechanical weeding yes/no",
                        covariate.labels = c("1-5 adopters known","6-10 adopters known","more than 10 adopters known",
                                             "1-5 fields observed","6-10 fields observed","11-15 fields observed","more than 15 fields observed",
                                             "older than 45 years", "farm size > 50 ha", "AES participation"),
                        dep.var.labels   = "", single.row=FALSE)


#+ q7_age + q7_speci_select+ q7_size + q7_AES
#q6_col1 + q6_col2 + q6_col3 +q7_speci_select+q7_age 
#+ q7_size + q7_farm  + + q7_AES

par(mfrow=c(2,2))
plot(m.FieldPeer)
vif(m.FieldPeer)

#correlation between peer und field?
fisher.test(table(FullSample$NrFields,FullSample$q3_info))#no dependence

#univariate checks
#adoption&field
fisher.test(table(FullSample$NrFields,FullSample$q1_adopt))#no

#adopt&peers
fisher.test(table(FullSample$q3_info,FullSample$q1_adopt))#no

#adoption & distFields
fisher.test(table(FullSample$FieldDist,FullSample$q1_adopt))#no

#adopt&AES
fisher.test(table(FullSample$q7_AES,FullSample$q1_adopt))#no

#adopt&Intention trad.mech.weeding
fisher.test(table(FullSample$q6_col1,FullSample$q1_adopt))#yes

#adopt&Intention modern.mech.weeding
fisher.test(table(FullSample$q6_col2,FullSample$q1_adopt))#yes

#adopt$auton.mech.weeding
fisher.test(table(FullSample$q6_col3,FullSample$q1_adopt))#no

#Todo
#for 20th of March
#get coordinates for demonatrations farms: https://www.storybench.org/geocode-csv-addresses-r/
#get mapping of adivsory regions --> counties --> Enola? #first, use map and 4 large Produzenten
#for those only PLZ: get centtroid of PLZ-area
#check for collinearity



#match öko data from zensus_spi with existing data set
#1.run zensus_api file

#subset d.full from zensus-api to what we need
Organic_lkr <- d.full %>% dplyr::select(2,6,13)
FullSample <- rename(FullSample, Kreis = landkreis)

FullSample <- left_join(FullSample,Organic_lkr,by="Kreis")

#need to check for differences, eg. Kreis Höxter vs. Höxter,Kreis



##mapping centroid to county(Landkreis)--> Seperate file togetehr with Dario
library(sp)
library(rgdal)
library(sf)
library(sp)
library(RColorBrewer)
library(geojsonio)
gadm36_DEU_2_sp <- readRDS("Data_prep/gadm36_DEU_2_sp.rds")
# get spatial data for Germany on county level
county <- gadm36_DEU_2_sp
par(mfrow=c(1,1))
plot(county, col ='grey')

#get shapes of PLZ area
ger_plz <- st_read("C:/Users/Massfeller/sciebo/PhD/Paper/Paper2_socioSpat/AnalysisPaper2/Data_prep/PLZGEO/plz-5stellig.shp")
plot(ger_plz$geometry)
shp.plz <- geojson_read("C:/Users/Massfeller/sciebo/PhD/Paper/Paper2_socioSpat/AnalysisPaper2/Data_prep/plz-5stellig.geojson",  what = "sp")
plot(shp.plz)

