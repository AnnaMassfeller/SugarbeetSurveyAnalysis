############################
#  Data Pre-processing    #
###########################


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
SampleDF <- read_excel("Data_prep/sample.xlsx")#excel
SampleDFcsv <- read.csv("Data_prep/sample.csv")#csv
#directly as json
json_file <- "https://fruchtfolge.agp.uni-bonn.de/db/survey_anna/_all_docs?include_docs=true"
json_data <- fromJSON(paste(readLines(json_file), collapse=""))



###set up models and see what we would need to fill them easily

#Model 1 to answer "How do information exchange and field observation influence adoption?" 
# Adopt ~ Info + Field + Field*FieldDist + Field*FieldDist^2 + Adv + DemoDist + DemoDist^2 + Control + Tech



#with TestData
summary(TestModel1 <- glm(Adopt ~ Info + NeiField_Nr + NeiField_Rad + Intent_trad + 
                    Intent_mod + Intent_auton + Age + FarmSize + ProdType + 
                    FarmSpec + AES, data = TestData, na.action = na.exclude, family = binomial("probit")))





#With SampleDF
#adjustment of variables
SampleDF$q1 <- ifelse(SampleDF$q1=="Ja",1,0)
SampleDF$q7_env <- ifelse(SampleDF$q7_env=="Ja",1,0)
SampleDF$q3 <- as.factor(SampleDF$q3)
SampleDF$q6_col1 <- as.factor(SampleDF$q6_col1)
SampleDF$q6_col2 <- as.factor(SampleDF$q6_col2)
SampleDF$q6_col3 <- as.factor(SampleDF$q6_col3)
SampleDF$q7_age <- as.factor(SampleDF$q7_age)
SampleDF$q7_size <- as.factor(SampleDF$q7_size)
SampleDF$q7_farm <- as.factor(SampleDF$q7_farm)
SampleDF$q7_orient_select <- as.factor(SampleDF$q7_orient_select)
SampleDF$q7_env <- as.factor(SampleDF$q7_env)


summary(TestModel2 <- glm(q1 ~ q3 + q6_col1 + q6_col2 + q6_col3 + q7_age 
                            + q7_size + q7_farm +q7_orient_select + q7_env, 
                            data = SampleDF, family = binomial("probit")))





#We would need: Distance between own and other plots, Number of plots for those who indicated it on the map
###Descrptive analysis of techniques/ status quo (Table from question2)




######################
###let's go spatial###
######################

#install.packages('terra', repos='https://rspatial.r-universe.dev')


#lodges <- readOGR("Zambezi.Accomodation.shp")

##########
#manually#
##########
#create own example data how it should look like
lon <- c(12.235344194080837, 12.572512715956744,12.679533938952855,12.442329243421555)
lat <- c(52.09892870683502, 52.12022357889193, 52.67480458454271, 52.39298085647458)
xy <- cbind(lon, lat)
xy <- as.data.frame(xy)

fields <- SpatialPointsDataFrame(coords = xy, data = xy) 

lon2 <- c(8.349344194080837, 13.671512715956744,8.578533938952855,10.332329243421555)
lat2 <- c(52.09592870683502, 53.02022357889193, 52.68480458454271, 51.09298085647458)
xy2 <- cbind(lon2, lat2)
xy2 <- as.data.frame(xy2)

farms <- SpatialPointsDataFrame(coords = xy2, data = xy2)

# gives distances of one farm to one field
dist.field.farm <- pointDistance(fields,farms, lonlat = T) #in luftlinie in m
#divide by 1000 to get km
dist.field.farm <- dist.field.farm/1000

#dist.field.farm <- as.data.frame(dist.field.farm)
# Gives the minimum value and multiplies it by 111, to get km
#dist.field.farm <- apply(dist.field.farm, 1, FUN=min)*111
#xy ist ein dataframe der nur die xy coordinates von d enthält, also long lat


#get centroid of own fields = approx. location of the farm
#centroid = gCentroid(spdf,byid=TRUE)

#example gCentroid
#assume one farmer selected the following fields
#summary(SampleDF$q4_shape_own)

lon3 <- c(12.349344194080837, 12.671512715956744,12.578533938952855,12.332329243421555)
lat3 <- c(52.09592870683502, 53.02022357889193, 52.68480458454271, 52.09298085647458)
xy3 <- cbind(lon3, lat3)
xy3 <- as.data.frame(xy3)

farms_centr <- SpatialPointsDataFrame(coords = xy3, data = xy3)

centroid = gCentroid(farms_centr)
plot(farms_centr)
plot(centroid,col='red',add=TRUE)
plot(fields,col='blue',add=TRUE)

#now calculate the distance of this centroid (farm) to all fields and calculate the average
# gives distances of one farm to one field
dist.field.centroid <- pointDistance(fields,centroid, lonlat = T) #in luftlinie in m
#as df
#dist.field.centroid <- as.data.frame(dist.field.centroid)
#divide by 1000 to get km and get mean
#dist.field.centroid <- apply(dist.field.centroid, 1, FUN=mean)/1000--> too few observations
dist.field.centroid <- dist.field.centroid/1000
mean(dist.field.centroid)
#####
#############################
#####use the csv.file data###
#############################

#manipulating string type
ownfields_spt <- SampleDF$q4_shape_own
#matrix(as.numeric(strsplit(chartr('([)]','    ',ownfields_spt),",")[[1]]),ncol=2,byrow=TRUE)
ownfields_spt<- chartr("{}lat:lng()'[]","                ", ownfields_spt)
#ownfields_spt<- str_replace_all(ownfields_spt, "[[:][{}]]", " ")
#ownfields_spt<- str_replace_all(ownfields_spt, "[[lat' lng]]", " ")
summary(ownfields_spt)
#m_fields <- matrix(as.numeric(strsplit(ownfields_spt,",")[[1]]),ncol=4,byrow=TRUE)
ownfields_spt<-str_split(ownfields_spt, boundary("word"))
#ownfields_spt<- as.vector(ownfields_spt)
str_sub(ownfields_spt[[1]][1])

#same for neighbours fields
neifields_spt <- SampleDF$q5_shape_other
neifields_spt<- chartr("{}lat:lng()'[]","                ", neifields_spt)
neifields_spt<-str_split(neifields_spt, boundary("word"))

#own fields and farm
lon4 <- as.numeric(c(ownfields_spt[[1]][1],ownfields_spt[[1]][3]))
lat4 <- as.numeric(c(ownfields_spt[[1]][2],ownfields_spt[[1]][4]))
xy4 <- cbind(lon4, lat4)
xy4 <- as.data.frame(xy4)

farms_centr4 <- SpatialPointsDataFrame(coords = xy4, data = xy4)#has to be saved in the to go data set then
centroid4 = gCentroid(farms_centr4)#has to be saved in the to go dataset then


#others fields
lon5 <- as.numeric(c(neifields_spt[[1]][1],neifields_spt[[1]][3]))
lat5 <- as.numeric(c(neifields_spt[[1]][2],neifields_spt[[1]][4]))
xy5 <- cbind(lon5, lat5)
xy5 <- as.data.frame(xy5)

neifields <- SpatialPointsDataFrame(coords = xy5, data = xy5)

#distance between centroid and neighbours fields
dist.neifields.centroid4 <- pointDistance(neifields,centroid4, lonlat = T) #in luftlinie in m
dist.neifields.centroid4 <- dist.neifields.centroid4/1000
mean(dist.neifields.centroid4)#save in to go data frame


plot(farms_centr4)
plot(centroid4,col='red',add=TRUE)
plot(neifields,col='blue')  

######################################
##loop over all elements of the list##
######################################

#get every item
for(i in ownfields_spt)
{print(i)}

#get every element
for(i in ownfields_spt)
{
  for(j in i)
  {print(j)}
}





















##mapping centroid to county(Landkreis)
library(sp)
library(rgdal)
library(sf)
library(sp)
library(RColorBrewer)
gadm36_DEU_2_sp <- readRDS("Data/gadm36_DEU_2_sp.rds")
# get spatial data for Germany on county level
county <- gadm36_DEU_2_sp
plot(farms_centr)
plot(centroid,col='red',add=TRUE)
plot(fields,col='blue',add=TRUE)
plot(county, col ='grey', add = TRUE)




