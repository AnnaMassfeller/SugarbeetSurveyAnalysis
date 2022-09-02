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
library(dplyr)
library(naniar)
library(spData)
library(vtable)

###load data
#directly as json
json_file <- "https://fruchtfolge.agp.uni-bonn.de/db/survey_anna/_all_docs?include_docs=true"
json_data <- fromJSON(paste(readLines(json_file), collapse=""))

#as csv
setwd("C:/Users/Massfeller/sciebo/PhD/Paper/Paper2_socioSpat/AnalysisPaper2")
FullSample<- read_csv("Data/survey.csv")#csv
zuordnung_plz_ort <- read_excel("Data_prep/zuordnung_plz_ort.xls")

#remove strings
FullSample[] <- lapply(FullSample, function(x) gsub("[]'[(),]", "", x)) # check later if this makes sense for all columns

#change all empty cells to NA
FullSample <- FullSample %>% mutate_all(na_if,"")

#exclude all test runs by AM!!!
#FullSample %>% dplyr::select(FullSample,-)

#create subsample (just for trial purposes) with entries in the coordinate columns
SampleDF_allCoord<- FullSample[!is.na(FullSample$q4_own),]
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



tab1 <- as.table(rbind(c(N,a,b,c,d),c(N_dist,"will be calculated",b_dist,c_dist,d_dist))) 
dimnames(tab1) <- list(Frequency = c("#","Thereof WithDist"), 
                      Answers = c("FullSample","own+neib.coordinates",
                                    "only neib.coordinates","onlyPLZ","Neither PLZ nor Coord.")) 
                      

tab1


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
FullSample <- rename(FullSample, plz1 = q4_alt)
#first merge all postal codes in one column
FullSample$plz <- coalesce(FullSample$plz1, FullSample$plz2,FullSample$plz2_nei)
plz_lkr <- zuordnung_plz_ort %>% dplyr::select(plz, landkreis,bundesland)
plz_lkr$plz <- as.numeric(plz_lkr$plz)
FullSample$plz <- as.numeric(FullSample$plz)
FullSample <- merge(FullSample,plz_lkr,by="plz")#here observations in Full sample become irrationally more


#get some descriptives
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
g.adopt <- ggplot(FullSample, aes(q1_adopt))+geom_histogram(binwidth = 5, stat = "count")
g.adopt

g.info <- ggplot(FullSample, aes(q3_info))+geom_histogram(binwidth = 5, stat = "count")
g.info

g.NrFields <- ggplot(FullSample, aes(NrFields))+geom_histogram(binwidth = 5, stat = "count")
g.NrFields

g.FieldDist <- ggplot(FullSample, aes(FieldDist))+geom_histogram(binwidth = 5, stat = "count")
g.FieldDist

g.Intent.Mech_trad <- ggplot(FullSample, aes(q6_col1))+geom_histogram(binwidth = 5, stat = "count")
g.Intent.Mech_trad

g.Intent.Mech_modern <- ggplot(FullSample, aes(q6_col2))+geom_histogram(binwidth = 5, stat = "count")
g.Intent.Mech_modern

g.Intent.Mech_auton <- ggplot(FullSample, aes(q6_col3))+geom_histogram(binwidth = 5, stat = "count")
g.Intent.Mech_auton

g.Age <- ggplot(FullSample, aes(q7_age))+geom_histogram(binwidth = 5, stat = "count")
g.Age

g.Farmsize <- ggplot(FullSample, aes(q7_size))+geom_histogram(binwidth = 5, stat = "count")
g.Farmsize

g.Specialization <- ggplot(FullSample, aes(q7_speci_select))+geom_histogram(binwidth = 5, stat = "count")
g.Specialization

g.AES <- ggplot(FullSample, aes(q7_AES))+geom_histogram(binwidth = 5, stat = "count")
g.AES

g.bundesland <- ggplot(FullSample, aes(bundesland))+geom_histogram(binwidth = 5, stat = "count")
g.bundesland

g.PLZ <- ggplot(FullSample, aes(plz))+geom_histogram(binwidth = 5, stat = "count")
g.PLZ

table(FullSample$q1_adopt, FullSample$q7_AES)


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
summary(TestModel2 <- glm(q1_adopt ~ q3_info
                          + FieldDist + NrFields + q7_age + q7_speci_select
                          + q7_size + q7_AES, 
                          data = FullSample, family = binomial("probit")))

#q6_col1 + q6_col2 + q6_col3 +q7_speci_select+q7_age 
#+ q7_size + q7_farm  + + q7_AES

##why more observations after running???

par(mfrow=c(2,2))
plot(TestModel2)
vif(TestModel2)


#Todo
#check what happens with those who überspringen, how can the data.frame look like then?
#remove all [] and '

#for 7th of March
#get coordinates for demonatrations farms: https://www.storybench.org/geocode-csv-addresses-r/
#get mapping of adivsory regions --> counties --> Enola?
#first, use map and 4 large Produzenten
#check for collinearity
#get plz fro others' fields for those who haven't selected own fields
#setr distance for those who haven't selected any fields to infinity

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
gadm36_DEU_2_sp <- readRDS("Data_prep/gadm36_DEU_2_sp.rds")
# get spatial data for Germany on county level
county <- gadm36_DEU_2_sp
plot(county, col ='grey')



