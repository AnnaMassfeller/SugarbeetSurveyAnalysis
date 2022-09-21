############################
#  Data Pre-processing    #
###########################

#rm(list = ls())
#load packages
library(readxl)
library(plyr)
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
library(writexl)
library(splitstackshape)
library(dplyr)

###load data
#directly as json
#json_file <- "https://fruchtfolge.agp.uni-bonn.de/db/survey_anna/_all_docs?include_docs=true"
#json_data <- fromJSON(paste(readLines(json_file), collapse=""))

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
SampleDF_onlyPLZ <-FullSample[!is.na(FullSample$q4_alt)&is.na(FullSample$q5_other)&is.na(FullSample$q4_own),]
SampleDF_NoDistNoOwnCoord <-FullSample[is.na(FullSample$q5_alt_dist)&is.na(FullSample$q4_own),] #but others coordinates coudl be used to get PLZ & dist.

#is there someone giving own but not others coordinates?
SampleDF_onlyOwn <- FullSample[!is.na(FullSample$q4_own)&is.na(FullSample$q5_other),] 

N <-nrow(FullSample)
a<- nrow(SampleDF_allCoord)
b <- nrow(SampleDF_NeiCoord)
c <-nrow(SampleDF_onlyPLZ)
d <- nrow(SampleDF_NoCoordNoPLZ)
e <- nrow(SampleDF_onlyOwn)

FullSample_dist <- FullSample[!is.na(FullSample$q5_alt_dist),]
SampleDF_allCoord_dist<- FullSample[!is.na(FullSample$q4_own)&!is.na(FullSample$q5_alt_dist),]
SampleDF_NeiCoord_dist<- FullSample[!is.na(FullSample$q5_other)&is.na(FullSample$q4_own)&!is.na(FullSample$q5_alt_dist),]
SampleDF_NoCoord_dist <- FullSample[is.na(FullSample$q5_other)&is.na(FullSample$q4_own)&!is.na(FullSample$q5_alt_dist),]
SampleDF_NoCoordNoPLZ_dist <- FullSample[is.na(FullSample$q5_other)&is.na(FullSample$q4_own)&is.na(FullSample$q4_alt)&!is.na(FullSample$q5_alt_dist),]
SampleDF_onlyPLZ_dist <-FullSample[!is.na(FullSample$q4_alt)&!is.na(FullSample$q5_alt_dist),]
SampleDF_onlyOwn_dist <- SampleDF_onlyOwn[!is.na(SampleDF_onlyOwn$q5_alt_dist),] 

N_dist <-nrow(FullSample_dist)
a_dist <- nrow(SampleDF_allCoord_dist)
b_dist <- nrow(SampleDF_NeiCoord_dist)
c_dist<-nrow(SampleDF_onlyPLZ_dist)
d_dist <- nrow(SampleDF_NoCoordNoPLZ_dist)
e_dist <- nrow(SampleDF_onlyOwn_dist)


tab1 <- as.table(rbind(c(N,a,b,c,d,e),c(N_dist,"will be calculated","will be calculated",c_dist,d_dist, e_dist))) 
dimnames(tab1) <- list(Frequency = c("#","Thereof WithDist"), 
                       Answers = c("FullSample","own+neib.coordinates",
                                   "only neib.coordinates","onlyPLZ","Neither PLZ nor Coord.", "only own")) 


tab1


#create subsamples of adopters and non-adopters fro later analysis
Adopters <-FullSample[(FullSample$q1_adopt == 1),]
NonAdopters <-FullSample[(FullSample$q1_adopt == 0),]

#those who haven't selected any fields should also have no category (small mistake in the beginning of the survey)
FullSample[FullSample$q5_alt_fields == 0&!is.na(FullSample$q5_alt_fields),]$q5_alt_dist <-c("6")


#####Centroids
#get centroid for those who have some coordinate (own or neighbours), 10.05: 101+14+69 = 184
#first copy corrdinates into original column, as sometimes one field is not enough
SampleDF_allCoord$coord_own <- SampleDF_allCoord$q4_own
SampleDF_allCoord$q4_own <- paste(SampleDF_allCoord$q4_own,SampleDF_allCoord$coord_own)
SampleDF_allCoord$coord_nei <- SampleDF_allCoord$q5_other
SampleDF_allCoord$q5_other <- paste(SampleDF_allCoord$q5_other,SampleDF_allCoord$coord_nei)

#same for those who only selected one neighbouring field
#SampleDF_NeiCoord$coord_nei <- SampleDF_NeiCoord$q5_other
#SampleDF_NeiCoord$q5_other <- paste(SampleDF_NeiCoord$q5_other,SampleDF_NeiCoord$coord_nei)
SampleDF_allNei <- FullSample[!is.na(FullSample$q5_other),]
SampleDF_allNei$coord_nei <- SampleDF_allNei$q5_other
SampleDF_allNei$q5_other <- paste(SampleDF_allNei$q5_other,SampleDF_allNei$coord_nei)
#identify who has only coordinates 
df.Coord <- FullSample[(!is.na(FullSample$q4_own))|
                       (!is.na(FullSample$q5_other)),]
#exclude thoe who have PLZ (e.g. those who mainly selected others fields but not own)
#df.Coord <- df.Coord[(is.na(df.Coord$q4_alt)),]

#create column where those who have no own but neighbouring corrdinates are also listed
df.Coord$fields <- coalesce(df.Coord$q4_own, df.Coord$q4_cached, df.Coord$q5_other)

#now we need to get some centroid for those, either from own fields or from neighb. fields
fields <- df.Coord$fields
fields<- chartr("{}lat:lng()'[]","                ", fields)
summary(fields)
fields<-str_split(fields, boundary("word"))
str_sub(fields[[1]][1])#third coor
#coord.as.df <- function(x) cbind.data.frame(split(x[[i]], rep(1:2, times=length(x[[i]])/2)), stringsAsFactors=F) 

#create empty data frames
coord.fields <- data.frame()
Centroids <- data.frame()

#create function to get every item
coord.as.df <- function(x) cbind.data.frame(split(x[[i]], rep(1:2, times=length(x[[i]])/2)), stringsAsFactors=F) 


#create df of coordinates for each farmer for all his/her fields and calculate centroid = farm location
for(i in seq_along(fields)){
  coord.fields <- coord.as.df(fields) #need zwischenschritt um cooord. als list zu speiichern
  coord.fields<- mutate_all(coord.fields, function(x) as.numeric(x))
  fields.spat <- SpatialPointsDataFrame(coords = coord.fields, data = coord.fields)
  centroids.farms= gCentroid(fields.spat)
  assign( paste("farms.spat", i, sep = "_") , centroids.farms)
  assign( paste("ownfields.spat", i, sep = "_") , fields.spat)#save coordinates of own fields
}
list.centroids <- do.call("list",mget(ls(pattern = "^farms.spat.*")))
list.ownfields.spat <- do.call("list",mget(ls(pattern = "^ownfields.spat.*")))

#save centroids in a data frame
CentroidsFields <- as.data.frame(list.centroids)
cdf<- names(CentroidsFields) #column name data frame
cols.x<-grep(".x",cdf) 
x <- do.call(paste,CentroidsFields[cols.x])
x<-str_split(x, boundary("word"))
cols.y<-grep(".y",cdf) 
y <- do.call(paste,CentroidsFields[cols.y])
y<-str_split(y, boundary("word"))

df.Centroids <- data.frame(x,y)
colnames(df.Centroids) <- c("Long_Centroid", "Lat_Centroid")

#assign right number
number <- sort(as.character(1:nrow(df.Centroids)))
df.Centroids <- cbind(df.Centroids, number)#now I know which observation belongs to which centroid

#now add the centroids to the dataset which contains all that have coordinates
df.Coord$number <- rownames(df.Coord)
df.Coord <- left_join(df.Coord, df.Centroids, by = "number")

#create mapping of numbers in lists and date/ id
mapping.date_number <- df.Coord %>% dplyr::select(date, number)



#we now have centroids for each observation that has coordinates
#based on that we now get the postalcode etc.
#reverse_geocoding <- df.Centroids %>%
 #reverse_geocode(lat = Lat_Centroid, long = Long_Centroid, method = 'osm',
  #          address = "address", full_results = TRUE)

#rev_geo <- reverse_geocoding %>% dplyr::select(Long_Centroid, Lat_Centroid,county, state, postcode)
#write_xlsx(rev_geo,"Processed/rev_geo.xlsx")
rev_geo<-read_xlsx("Processed/rev_geo.xlsx")
#add manually where missing
rev_geo$postcode <-ifelse(rev_geo$county == "Wittenberg", "06925",rev_geo$postcode)


#add info to allcoords df now
df.Coord <- left_join(df.Coord, rev_geo, by = "Lat_Centroid")
df.Coord<- df.Coord[ -c(45)]
names(df.Coord)[names(df.Coord) == "Long_Centroid.x"] <- "Long_Centroid"
df.Coord<-df.Coord[!duplicated(df.Coord$date), ]
df.Coord <- df.Coord %>% dplyr::select(date, Long_Centroid, Lat_Centroid, county, state, postcode)
df.Coord$postcode <-ifelse(df.Coord$date == "2022-03-02 12:04:44", "38229",df.Coord$postcode) #Salzgitter
df.Coord$postcode <-ifelse(df.Coord$date == "2022-03-09 19:24:00", "38229",df.Coord$postcode)
df.Coord$postcode <-ifelse(df.Coord$date == "2022-03-17 20:06:56", "59069",df.Coord$postcode)#Hamm
df.Coord$postcode <-ifelse(df.Coord$date == "2022-03-19 07:38:43", "04626",df.Coord$postcode)
df.Coord$postcode <-ifelse(df.Coord$date == "2022-03-31 10:17:58", "37589",df.Coord$postcode)
df.Coord$postcode <-ifelse(df.Coord$date == "2022-04-04 08:38:50", "93055",df.Coord$postcode)#kreisfreie Stadt Regensburg
df.Coord$postcode <-ifelse(df.Coord$date == "2022-04-08 08:18:39", "97320",df.Coord$postcode)
df.Coord$postcode <-ifelse(df.Coord$date == "2022-04-20 11:06:02", "23570",df.Coord$postcode) #kreisfreie Stadt Lübeck

df.Coord$county <-ifelse(df.Coord$date == "2022-03-02 12:04:44", "Salzgitter",df.Coord$county)
df.Coord$county <-ifelse(df.Coord$date == "2022-03-09 19:24:00", "Salzgitter",df.Coord$county)
df.Coord$county <-ifelse(df.Coord$date == "2022-03-17 20:06:56", "Hamm",df.Coord$county)
df.Coord$county <-ifelse(df.Coord$date == "2022-04-04 08:38:50", "Landkreis Regensburg",df.Coord$county)
df.Coord$county <-ifelse(df.Coord$date == "2022-04-20 11:06:02", "Lübeck",df.Coord$county)

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
FullSample$landkreis <- ifelse(FullSample$plz == "38259", "Salzgitter",FullSample$landkreis) 
FullSample$landkreis <- ifelse(FullSample$plz == "51147", "Köln",FullSample$landkreis) 
FullSample$landkreis <- ifelse(FullSample$plz == "59069", "Hamm",FullSample$landkreis) 
FullSample$landkreis <- ifelse(FullSample$plz == "60431", "Frankfurt am Main",FullSample$landkreis) 
FullSample$landkreis <- ifelse(FullSample$plz == "64291", "Darmstadt",FullSample$landkreis) 
FullSample$landkreis <- ifelse(FullSample$plz == "93055", "Regensburg",FullSample$landkreis)
FullSample$landkreis <- ifelse(FullSample$plz == "23570", "Lübeck",FullSample$landkreis)

FullSample$landkreis <- coalesce(FullSample$landkreis, FullSample$county) #somehow needs to be done again
FullSample[FullSample$date == "2022-04-25 11:44:43",]$landkreis <- "Guetersloh"
FullSample[FullSample$date == "2022-04-25 11:44:43" & FullSample$landkreis == "Guetersloh",]$plz <- "33824"
FullSample[FullSample$date == "2022-04-25 11:44:43" & FullSample$landkreis == "Guetersloh",]$bundesland <- "Nordrhein-Westfalen"


#those who only sleected others fields get centroid of PLz as own centroid: SampleDF_allNei
#add PLZ centroids
df.PLZ_centroids_full <- read_xlsx("Backgrounddata/Geodata/zipcodecentroid.XLSX")
df.PLZ_centroids<- df.PLZ_centroids_full %>% dplyr::select(PLZ, lat, lon) %>% dplyr::rename(plz = PLZ,PLZ_lat = lat,PLZ_lon = lon)
df.PLZ_centroids$plz <- as.factor(df.PLZ_centroids$plz)
FullSample <- left_join(FullSample, df.PLZ_centroids, by = "plz")
FullSample<-FullSample[!duplicated(FullSample$date), ]

df.plz_date <- FullSample %>% dplyr::select(date, plz)

#map everything to subset
SampleDF_NeiCoord<-left_join(SampleDF_NeiCoord,df.plz_date,by = "date")
SampleDF_NeiCoord <- left_join(SampleDF_NeiCoord, df.PLZ_centroids, by = "plz")
SampleDF_NeiCoord<-SampleDF_NeiCoord[!duplicated(SampleDF_NeiCoord$date), ]

#those who only selected own now get centroid from PLZ
table(SampleDF_NeiCoord$date %in% df.Coord$date)

SampleDF_NeiCoord_latlon <- SampleDF_NeiCoord %>% dplyr::select(date, PLZ_lat, PLZ_lon)

df.Coord<- left_join(df.Coord,SampleDF_NeiCoord_latlon, by = "date")

df.Coord$Long_Centroid<-ifelse(!is.na(df.Coord$PLZ_lat) == TRUE, df.Coord$PLZ_lat, df.Coord$Long_Centroid)
df.Coord$Lat_Centroid<-ifelse(!is.na(df.Coord$PLZ_lon) == TRUE, df.Coord$PLZ_lon, df.Coord$Lat_Centroid)


#match öko data from zensus_api with existing data set
#1.run zensus_api file
#source("C:/Users/Massfeller/sciebo/PhD/Paper/Paper2_socioSpat/SurveyAnalysis/Rfiles/zensus_api.R")
#subset d.full from zensus-api to what we need
Organic_lkr <- d.full[d.full$JAHR == "2016",]# %>% dplyr::select(1,4,5,7,12, 41,42,43,44,67,71)

df.MeanFarmSize <- read_xlsx("Backgrounddata/durchschnittlicheBetriebsgröße.XLSX")#data from 2020
df.MeanFarmSize <- dplyr::rename(df.MeanFarmSize, "KREISE" = 1)
df.MeanFarmSize <- dplyr::rename(df.MeanFarmSize, "Kreise" = 2)
df.MeanFarmSize<-df.MeanFarmSize[!(df.MeanFarmSize$KREISE == "03152"),]
df.MeanFarmSize$KREISE <-ifelse(df.MeanFarmSize$Kreise == "Göttingen, Landkreis","03152",df.MeanFarmSize$KREISE) 
Organic_lkr <- left_join(Organic_lkr, df.MeanFarmSize, by = "KREISE")

#change order of string
Organic_lkr$Kreis <- sub('^(.*), Landkreis', 'Landkreis \\1', Organic_lkr$Kreis)
Organic_lkr$Kreis <- sub('^(.*), kreisfreie Stadt', 'kreisfreie Stadt \\1', Organic_lkr$Kreis)
Organic_lkr$Kreis <- sub('^(.*), Kreis', 'Kreis \\1', Organic_lkr$Kreis) 


#now add to full sample
FullSample <- dplyr::rename(FullSample, Kreis = landkreis)

#need to check for differences, eg. Kreis Höxter vs. Höxter,Kreis
table(FullSample$Kreis %in% Organic_lkr$Kreis)

#df.lkr_names <- cbind(FullSample$Kreis, Organic_lkr$Kreis)#ignore warning
Organic_lkr$Kreis <- lapply(Organic_lkr$Kreis, function(x) gsub("Landkreis Neustadt a.d.Aisch-Bad Windsheim", "Landkreis Neustadt an der Aisch-Bad Windsheim", x))
Organic_lkr$Kreis <- lapply(Organic_lkr$Kreis, function(x) gsub("Landkreis Dillingen a.d.Donau", "Landkreis Dillingen an der Donau", x))
Organic_lkr$Kreis <- lapply(Organic_lkr$Kreis, function(x) gsub("Landkreis Pfaffenhofen a.d.Ilm", "Landkreis Pfaffenhofen an der Ilm", x))
Organic_lkr$Kreis <- lapply(Organic_lkr$Kreis, function(x) gsub(" Vorpommern-Greifswald, Landkreis", "Vorpommern-Greifswald, Landkreis", x))
Organic_lkr$Kreis <- lapply(Organic_lkr$Kreis, function(x) gsub("Landkreis Region Hannover", "Region Hannover", x))
Organic_lkr$Kreis <- lapply(Organic_lkr$Kreis, function(x) gsub("Landkreis Main-Tauber-Kreis", "Main-Tauber-Kreis", x))
Organic_lkr$Kreis <- lapply(Organic_lkr$Kreis, function(x) gsub("Landkreis Schleswig-Flensburg", "Kreis Schleswig-Flensburg", x))
Organic_lkr$Kreis <- lapply(Organic_lkr$Kreis, function(x) gsub("Kreis Altenburger Land", "Altenburger Land", x))
Organic_lkr$Kreis <- lapply(Organic_lkr$Kreis, function(x) gsub("Landkreis Bergstraße", "Kreis Bergstraße", x))
#Organic_lkr$Kreis <- lapply(Organic_lkr$Kreis, function(x) gsub("Landkreis Nienburg (Weser)", "Landkreis Nienburg/Weser", x))
Organic_lkr$Kreis <- lapply(Organic_lkr$Kreis, function(x) gsub("kreisfreie Stadt Darmstadt", "Darmstadt", x))
Organic_lkr$Kreis <- lapply(Organic_lkr$Kreis, function(x) gsub("kreisfreie Stadt Köln", "Köln", x))
Organic_lkr$Kreis <- lapply(Organic_lkr$Kreis, function(x) gsub("kreisfreie Stadt Hamm", "Hamm", x))
Organic_lkr$Kreis <- lapply(Organic_lkr$Kreis, function(x) gsub("kreisfreie Stadt Frankfurt am Main", "Frankfurt am Main", x))
Organic_lkr$Kreis <- lapply(Organic_lkr$Kreis, function(x) gsub("Landkreis Hohenlohekreis", "Hohenlohekreis", x))
Organic_lkr$Kreis <- lapply(Organic_lkr$Kreis, function(x) gsub("Landkreis Rhein-Neckar-Kreis", "Rhein-Neckar-Kreis", x))
FullSample$Kreis <- lapply(FullSample$Kreis, function(x) gsub("Guetersloh", "Kreis Gütersloh", x))
FullSample$Kreis <- lapply(FullSample$Kreis, function(x) gsub("Landkreis Altenburger Land", "Altenburger Land", x))
FullSample$Kreis <- lapply(FullSample$Kreis, function(x) gsub("Landkreis Nienburg/Weser", "Landkreis Nienburg (Weser)", x))
FullSample$Kreis <- lapply(FullSample$Kreis, function(x) gsub("Städteregion Aachen", "Städteregion Aachen (einschl. Stadt Aachen)", x))
Organic_lkr$Kreis <- lapply(Organic_lkr$Kreis, function(x) gsub("kreisfreie Stadt Salzgitter", "Salzgitter", x))
Organic_lkr$Kreis <- lapply(Organic_lkr$Kreis, function(x) gsub("kreisfreie Stadt Lübeck, Hansestadt", "Lübeck", x))
Organic_lkr$Kreis <- lapply(Organic_lkr$Kreis, function(x) gsub("Landkreis Wittenberg", "Wittenberg", x))
FullSample$Kreis <- lapply(FullSample$Kreis, function(x) gsub("Landkreis Wittenberg", "Wittenberg", x))
Organic_lkr$Kreis <- lapply(Organic_lkr$Kreis, function(x) gsub("Landkreis Rendsburg-Eckernförde", "Kreis Rendsburg-Eckernförde", x))
table(FullSample$Kreis %in% Organic_lkr$Kreis)

#FullSample <- dplyr::select(FullSample, - c("BTR030","FLC048"))
FullSample <- left_join(FullSample,Organic_lkr,by="Kreis")


FullSample<-FullSample[!duplicated(FullSample$date), ]

#getoverview
st(FullSample, vars = c("q1_adopt","q3_info","plz","NrFields","FieldDist","q6_col1","q6_col2","q6_col3","q7_age", "q7_size", "q7_farm","q7_speci_select", "q7_AES"))



#add distance to demonstration farm
#get geo-data from address
# Read in the CSV data and store it in a variable 
#set google API
register_google(key = "AIzaSyCGy4X_aWJabtdrvJmhouJl_royB9Mc-PU", write = TRUE)

df.Modell_Organic <-read_xlsx("Backgrounddata/Modellbetriebe/ModellbetriebeÖkolandbau.xlsx")
df.Modell_Organic <- as.data.frame(df.Modell_Organic)
Modell_organic_addresses <- df.Modell_Organic$addresses
Modell_organic_addresses<- as.data.frame(Modell_organic_addresses)
Modell_organic_addresses <- dplyr::rename(Modell_organic_addresses, addresses = Modell_organic_addresses)

#geocode addresses
#DemoOrganic_coord<-mutate_geocode(Modell_organic_addresses, addresses)
#write_xlsx(DemoOrganic_coord,"Processed/DemoOrganic_coord.xlsx")
DemoOrganic_coord<-read_xlsx("Processed/DemoOrganic_coord.xlsx")
DemoOrganic_coord <- dplyr::rename(DemoOrganic_coord,DemoOrg_lon = lon, DemoOrg_lat = lat)
Demo_coord <- dplyr::select(DemoOrganic_coord,DemoOrg_lon, DemoOrg_lat)

#combine lat, lon column to one
DemoOrganic_coord$location_d <- paste(DemoOrganic_coord$DemoOrg_lon,",", DemoOrganic_coord$DemoOrg_lat)
demo_coord_m <- cbind(Demo_coord$DemoOrg_lat, Demo_coord$DemoOrg_lon)

#now calculate distance to demonstration farms
#1.save all demo-coordinates in a sp.dataframe
#sp.demo <- SpatialPointsDataFrame(coords = Demo_coord, data = Demo_coord,
 #                              proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

#2.save all centroids in a sp.dataframe

FullSample$Lat_Centroid <- as.numeric(FullSample$Lat_Centroid)
FullSample$Long_Centroid <- as.numeric(FullSample$Long_Centroid)

FullSample$Lat_Centroid<- coalesce(FullSample$Lat_Centroid, FullSample$PLZ_lat)
FullSample$Long_Centroid<- coalesce(FullSample$Long_Centroid, FullSample$PLZ_lon)


farms_coord <- FullSample %>% dplyr::select(date,Lat_Centroid, Long_Centroid)
farms_coord <- farms_coord[!is.na(farms_coord$Lat_Centroid),]
farms_coord$location_f <- paste(farms_coord$Long_Centroid,",", farms_coord$Lat_Centroid)
farms_coord_m <- cbind(farms_coord$Lat_Centroid, farms_coord$Long_Centroid)
farms_coord_pure <- dplyr::select(farms_coord, Lat_Centroid, Long_Centroid)
farms_coord_pure$Lat_Centroid <- as.numeric(farms_coord_pure$Lat_Centroid)
farms_coord_pure$Long_Centroid <- as.numeric(farms_coord_pure$Long_Centroid)

sp.farms <- SpatialPointsDataFrame(coords = farms_coord_pure, data = farms_coord_pure,
                                  proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))


#3.calculate distance, take mean

farm_demo<-as.data.frame(pointDistance(farms_coord_m, demo_coord_m,lonlat = TRUE)/1000)

farm_demo$meanDist_demo <- rowMeans(farm_demo)
farm_demo$minDist_demo <- do.call(pmin, farm_demo)

#add number to dataset such that we can add it to the original one
farm_demo$number <- rownames(farm_demo)
farm_demo_clean <- farm_demo %>% dplyr::select(number,minDist_demo, meanDist_demo)

farms_coord$number <- rownames(farms_coord)

farms_coord <- left_join(farms_coord, farm_demo_clean, by = "number") 
farms_coord_done <- dplyr::select(farms_coord,date,minDist_demo,meanDist_demo)
FullSample <- left_join(FullSample, farms_coord_done, by = "date")

#add belonging to advisory region

#redo calculation of distance to others' farmers fields
#1.we have farm_demo as centroids of farms(either own centroids or PLZ centroids)
#we need centroids of other farmers fields: list.nei_fields-->no, cannot take tht approach, biases our calculation of distance to other farmers' fields
#let#s do it step by step manually first

#what's teh data set we look at? those who have selected other farmers' fields via map
#SampleDF_allNei
#first get centroids only for the farms in this data set
AllNei_coord <- farms_coord[farms_coord$date %in% SampleDF_allNei$date == TRUE,]
df.coord_nei <- SampleDF_allNei %>% dplyr::select(date, coord_nei)
AllNei_coord <- left_join(AllNei_coord, df.coord_nei, by = "date")
#make matrix for them
AllNei_coord_m <- cbind(AllNei_coord$Lat_Centroid, AllNei_coord$Long_Centroid)

#go thorugh every neighbouring field now
AllNei_coord$meanDist_Nei <- 0

for(i in 1:137){
  coord_nei.m <-as.vector(AllNei_coord[i,]$coord_nei)
  coord_nei.m <-strsplit(coord_nei.m,",", split = ",")
  coord_nei.m <- lapply(coord_nei.m, function(coord_nei.m) gsub(" ", "", coord_nei.m))
  coord_nei.m <- unlist(coord_nei.m)
  res <- split(coord_nei.m, rep(1:2,times=length(coord_nei.m)/2))
  df<- do.call(rbind.data.frame,res)
  df<-t(df)
  rownames(df) <- 1:nrow(df)
  df<-as.data.frame(df)
  df <- dplyr::rename(df, Lon = V1, Lat = V2)
  df$Lat <- as.numeric(df$Lat)
  df$Lon <- as.numeric(df$Lon)
  farm_nei_coord_m <- cbind( df$Lat,df$Lon)
  farm_nei<-as.data.frame(pointDistance(AllNei_coord_m[i,], farm_nei_coord_m,lonlat = TRUE)/1000)
  AllNei_coord[i,]$meanDist_Nei <- colMeans(farm_nei)
    }
#get subset to merg it then
AllNei_coord_small <- AllNei_coord %>% dplyr::select(date, meanDist_Nei)

#merge these new distances to full sample and recalculate categories
FullSample<- left_join(FullSample, AllNei_coord_small, by = "date")


##
#no add mean distance to full sample
#categorize into multiple choice answer options
#MeanDistDF_nei <- SampleDF_allNei %>% dplyr::select(date,meanDist_nei)
#FullSample <- left_join(FullSample,MeanDistDF_nei,by="date", copy = TRUE)

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
#FullSample[FullSample$q5_alt_fields == 0&!is.na(FullSample$q5_alt_fields),]$q5_alt_dist <-c("0")

FullSample$fields_b<-ifelse(FullSample$NrFields == "0",0,1)
FullSample$fields_b <- as.factor(FullSample$fields_b)

FullSample$meanDist <- FullSample$meanDist_Nei
FullSample$meanDist_cat <- FullSample$meanDist

#subset those who have observed fields by map
#FullSample
FullSample$meanDist_cat[FullSample$meanDist <= 5] = 0
FullSample$meanDist_cat[FullSample$meanDist >5] = 1
FullSample$meanDist_cat[FullSample$meanDist >10] = 2
FullSample$meanDist_cat[FullSample$meanDist >15] = 3
FullSample$meanDist_cat[FullSample$meanDist >20] = 4
FullSample$meanDist_cat[FullSample$meanDist>30] = 5#7
FullSample$meanDist_cat[is.na(FullSample$meanDist)] <- 6
FullSample$meanDist_cat <- as.character(FullSample$meanDist_cat)
FullSample$FieldDist <- coalesce(FullSample$q5_alt_dist, FullSample$meanDist_cat)

#create binary variables for prelimnary regression
FullSample$farmsize_b <- FullSample$q7_size
FullSample$farmsize_b <- as.numeric(FullSample$farmsize_b)
FullSample$farmsize_b <- ifelse(FullSample$farmsize_b <= 3,0,1)#alle unter 49 = 0

FullSample$age_b <- FullSample$q7_age
#FullSample$age_b <-  na_if(FullSample$age_b,"6")#assume those wo didn't want to give any information are older than 45
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

#add 3postal codes manually

FullSample[FullSample$date == "2022-03-19 07:38:43",]$plz <- "04626" 
FullSample[FullSample$date == "2022-03-28 20:32:35",]$plz <- "35794"
FullSample[FullSample$date == "2022-04-08 08:18:39",]$plz <- "97320"


#create prelimnary mapping for advisory region
##FullSample$advisory <- 0
#simply assume the following:

#Niedersachsen:Nordzucker
#Bayern: Südzucker
#Ba-Wü: Südzucker
#Rh-Pf: Südzucker
#Brandeburg: Südzucker
#Hessen: Südzucker
#schleswig-h: Nordzucker
#MV: Suiker

#problematisch:
#SachsenAnhalt: Nordzucker, P&L, Südzucker
#NRW: eigentlich P&L, bis auf Warburg

#FullSample$advisory <- ifelse(FullSample$bundesland == "Niedersachsen","Nordzucker",FullSample$advisory)
#FullSample$advisory <- ifelse(FullSample$bundesland == "Bayern","Südzucker",FullSample$advisory)
#FullSample$advisory <- ifelse(FullSample$bundesland == "Baden-Württemberg","Südzucker",FullSample$advisory)
#FullSample$advisory <- ifelse(FullSample$bundesland == "Brandenburg","Süddzucker",FullSample$advisory)
#FullSample$advisory <- ifelse(FullSample$bundesland == "Hessen","Südzucker",FullSample$advisory)
#FullSample$advisory <- ifelse(FullSample$bundesland == "Rheinland-Pfalz","Südzucker",FullSample$advisory)
#FullSample$advisory <- ifelse(FullSample$bundesland == "Schleswig-Holstein","Nordzucker",FullSample$advisory)
#FullSample$advisory <- ifelse(FullSample$bundesland == "Mecklenburg-Vorpommern","Cosun",FullSample$advisory)#currently only one from MV, simply add to nordzucker?
#FullSample$advisory <- ifelse(FullSample$bundesland == "Nordrhein-Westfalen","PfeifferLangen",FullSample$advisory)
#for NRW put those to Südzucker that are landkreise close to WAR
#FullSample$advisory <- ifelse(FullSample$Kreis == "Lippe","Südzucker",FullSample$advisory)
#FullSample$advisory <- ifelse(FullSample$Kreis == "Höxter","Südzucker",FullSample$advisory)
#FullSample$advisory <- ifelse(FullSample$Kreis == "Hochsauerlandkreis","Südzucker",FullSample$advisory)
#FullSample$advisory <- ifelse(FullSample$Kreis == "Paderborn","Südzucker",FullSample$advisory)
#FullSample$advisory <- ifelse(FullSample$Kreis == "Soest","Südzucker",FullSample$advisory)

#FullSample$advisory <- ifelse(FullSample$bundesland == "Sachsen-Anhalt","Nordzucker",FullSample$advisory)#currently 3 participants from S-A that are all close to farbik WZL
#one participant from S-A that belongs to Könnern:
#FullSample$advisory <- ifelse(FullSample$Kreis == "Mansfeld-Suedharz","PfeifferLangen",FullSample$advisory)
#FullSample$advisory <- ifelse(FullSample$bundesland == "Thüringen","Südzucker",FullSample$advisory)
#FullSample$advisory <- ifelse(FullSample$bundesland == "Sachsen","Südzucker",FullSample$advisory)

#FullSample$advisory <- as.factor(FullSample$advisory)


#those who indciated distance to fields in the alternative question now get mean for numeric meanDist

FullSample$meanDist<- ifelse(FullSample$q5_alt_dist == 0,2.5,FullSample$meanDist)
FullSample$meanDist<- ifelse(FullSample$q5_alt_dist == 1,7.5,FullSample$meanDist)
FullSample$meanDist<- ifelse(FullSample$q5_alt_dist == 2,12.5,FullSample$meanDist)
FullSample$meanDist<- ifelse(FullSample$q5_alt_dist == 3,17.5,FullSample$meanDist)
FullSample$meanDist<- ifelse(FullSample$q5_alt_dist == 4,25.5,FullSample$meanDist)
FullSample$meanDist<- ifelse(FullSample$q5_alt_dist == 5,35,FullSample$meanDist)
FullSample$meanDist<- ifelse(FullSample$q5_alt_dist == 6,50,FullSample$meanDist)#those who didn't observe any fields get 50 as distance
FullSample$meanDist<- coalesce(FullSample$meanDist, FullSample$meanDist_Nei)

#49 didn't indicate any fields
table(is.na(FullSample$meanDist_Nei)) #137 indicated fields over map
table(FullSample$q5_alt_dist) #127 indicated fields via multiple choice
#49+137+127 = 313

#as we loose many observations for th IV approach due to missing data in FLC048 and BTR030, need to find solution
#e.g. check in manual sheet
FullSample$BTR030 <- as.numeric(FullSample$BTR030)
FullSample$FLC047 <- as.numeric(FullSample$FLC047)

organic_man <- read_excel("Backgrounddata/Öko-Betriebe/Kreisebene/ÖkoforR.xlsx")
#identify who ha sno entry for lwBetrOrganic_Anzahl
FullSample_noFLC047 <- FullSample[is.na(FullSample$FLC047),]


FullSample$FLC047 <-ifelse(FullSample$Kreis == "Landkreis Kassel",3106,FullSample$FLC047)


FullSample$FLC047 <-ifelse(FullSample$Kreis == "Landkreis Limburg-Weilburg",2681,FullSample$FLC047)
FullSample$FLC047 <-ifelse(FullSample$Kreis == "Rhein-Kreis Neuss",190,FullSample$FLC047)


FullSample$FLC047 <-ifelse(FullSample$Kreis == "Main-Tauber-Kreis",5380,FullSample$FLC047)


FullSample$FLC047 <-ifelse(FullSample$Kreis == "Rhein-Neckar-Kreis",1899,FullSample$FLC047)

FullSample$FLC047 <-ifelse(FullSample$Kreis == "Landkreis Gießen",5900,FullSample$FLC047)

#no data for peine, salzgitter
#calculate manually as share of all farms/area
6/401 #0.01496259 percent of farms organicall
35763 *0.01496259 #percent of area organically
FullSample$FLC047 <-ifelse(FullSample$Kreis == "Landkreis Peine",535,FullSample$FLC047)

#recherhce for slazgitter: 0% öko
#https://www.paz-online.de/lokales/peine-lk/peine/oekolandbau-boomt-aber-nicht-im-landkreis-peine-DRWGDNIMWODBBQ2MQ4RVDSV7OE.html

FullSample$FLC047 <-ifelse(FullSample$Kreis == "Salzgitter",0,FullSample$FLC047)
FullSample$BTR030 <-ifelse(FullSample$Kreis == "Salzgitter",0,FullSample$BTR030) 

#same for Köln
1/365
17257*0.002739726
FullSample$FLC047 <-ifelse(FullSample$Kreis == "Köln",47,FullSample$FLC047)

#for "Landkreis südliche Weinstraße" take FLC048
FullSample$FLC047 <-ifelse(FullSample$Kreis == "Landkreis Südliche Weinstraße",1588,FullSample$FLC047)
#1875
#same for Landkreis Mayen Koblenz 6 bad Dürkheim
FullSample$FLC047 <-ifelse(FullSample$Kreis == "Landkreis Mayen-Koblenz",1875,FullSample$FLC047)

FullSample$FLC047 <-ifelse(FullSample$Kreis == "Landkreis Bad Dürkheim",1302,FullSample$FLC047)

#get öko-data fro rheinland fpalz
df.organic_rhlpflz <- read_xlsx("Backgrounddata/Organic_rhlpfz.xlsx")

#first map county id
#add name of landkreis
mapping.CountyID_lkrname <- Organic_lkr %>% dplyr::select(KREISE,Kreis)
#calculate mean farm size for landkreise where info is missing
mapping.CountyID_lkrname$Kreis <- as.character(mapping.CountyID_lkrname$Kreis)
df.organic_rhlpflz <- left_join(df.organic_rhlpflz, mapping.CountyID_lkrname, by = "Kreis")

#add data for donnersbergkreis manually
FullSample$FLC047 <-ifelse(FullSample$Kreis == "Donnersbergkreis",1909,FullSample$FLC047)
FullSample$BTR030 <-ifelse(FullSample$Kreis == "Donnersbergkreis",31,FullSample$BTR030)

#for eckernförde: https://www.statistik-nord.de/fileadmin/Dokumente/Faltbl%C3%A4tter/Faltbl%C3%A4tter_Landwirtschaft/Faltblatt_Landwirtschaft_RD-ECK.pdf
FullSample$FLC047 <-ifelse(FullSample$Kreis == "Kreis Rendsburg-Eckernförde",3596,FullSample$FLC047)
FullSample$BTR030 <-ifelse(FullSample$Kreis == "Kreis Rendsburg-Eckernförde",65,FullSample$BTR030)

#Wittenberg:https://www.bauernzeitung.de/news/sachsen-anhalt/oekologischer-landbau-im-aufwind/
FullSample$FLC047 <-ifelse(FullSample$Kreis == "Wittenberg",17404,FullSample$FLC047)
FullSample$BTR030 <-ifelse(FullSample$Kreis == "Wittenberg",98,FullSample$BTR030)

st(FullSample, vars = c("q1_adopt","q3_info","plz","NrFields","FieldDist","q6_col1","q6_col2","q6_col3","q7_age", "q7_size", "q7_farm","q7_speci_select", "q7_AES"))



#give good column names
FullSample <- dplyr::rename(FullSample, "lwBetr_Anzahl" = BTR010)
FullSample <- dplyr::rename(FullSample, "lwBetrOrganic_Anzahl" = BTR030)
FullSample <- dplyr::rename(FullSample, "lwBetrUnter5_Anzahl" = BTR010_FLCHA000B050)
FullSample <- dplyr::rename(FullSample, "lwBetr5b10_Anzahl" = BTR010_FLCHA050B100)
FullSample <- dplyr::rename(FullSample, "UAA" = FLC017)
FullSample <- dplyr::rename(FullSample, "UAA_Organic" = FLC047)
FullSample <- dplyr::rename(FullSample, "UAA_unter5" = FLC017_FLCHA000B050)
FullSample <- dplyr::rename(FullSample, "UAA_5b10" = FLC017_FLCHA050B100)
FullSample <- dplyr::rename(FullSample, "UAA_Sugarbeet" = `FLC004_BNZAT-2132`)
FullSample <- dplyr::rename(FullSample, "UAA_arable" = `FLC004_BNZAT-21`)


#add UAA and lw. betreibe allgmeien for eckernförde
FullSample$lwBetr_Anzahl <-ifelse(FullSample$Kreis == "Kreis Rendsburg-Eckernförde",1697,FullSample$lwBetr_Anzahl)
FullSample$UAA <-ifelse(FullSample$Kreis == "Kreis Rendsburg-Eckernförde",138682,FullSample$UAA)


#create columns for share of organic farms and orgnaic area
FullSample$ShareOrgFarms <- (FullSample$lwBetrOrganic_Anzahl/ FullSample$lwBetr_Anzahl)
FullSample$ShareOrgArea <- (FullSample$UAA_Organic/FullSample$UAA)

#add manually for wittenberg
FullSample$ShareOrgArea <-ifelse(FullSample$Kreis == "Wittenberg",0.25,FullSample$ShareOrgArea)
FullSample$ShareOrgFarms <-ifelse(FullSample$Kreis == "Wittenberg",0.32,FullSample$ShareOrgFarms)

#calculate abse don shares UAA and lw. betriebe
FullSample$lwBetr_Anzahl <-ifelse(FullSample$Kreis == "Wittenberg",306,FullSample$lwBetr_Anzahl)
FullSample$UAA <-ifelse(FullSample$Kreis == "Wittenberg",69616,FullSample$UAA)

#add manually where is missing
FullSample$UAA_Sugarbeet <-ifelse(FullSample$KREISE == "03152",2900,FullSample$UAA_Sugarbeet)
FullSample$UAA_Sugarbeet <-ifelse(FullSample$KREISE == "06535",96,FullSample$UAA_Sugarbeet)
FullSample$UAA_Sugarbeet <-ifelse(FullSample$KREISE == "01003",102,FullSample$UAA_Sugarbeet)
FullSample$UAA_Sugarbeet <-ifelse(FullSample$KREISE == "06411",41,FullSample$UAA_Sugarbeet)
FullSample$UAA_Sugarbeet <-ifelse(FullSample$KREISE == "06532",8,FullSample$UAA_Sugarbeet)

#get dichte land. betriebe
df.counties <-read_xlsx("Backgrounddata/kreise.xlsx")
df.counties <- dplyr::rename(df.counties, "Kreis" = "Kreisfreie Stadt")
df.counties <- dplyr::rename(df.counties, "Area" = 5)
df.counties <- dplyr::rename(df.counties, "Bevölkerung" = 6)
df.counties <- dplyr::rename(df.counties, "Bevölkerungsdichte" = 9)
df.counties$`Schlüssel-nummer` <- ifelse(df.counties$Kreis == "Göttingen","03152",df.counties$`Schlüssel-nummer`)
FullSample <- dplyr::rename(FullSample, "CountyID" = "KREISE")
df.counties <- dplyr::rename(df.counties, "CountyID" = "Schlüssel-nummer")
df.counties <- df.counties %>% dplyr::select(1,4,5,6,9)
#FullSample$Kreis <- as.character(FullSample$Kreis)
df.counties <- df.counties[-c(1,2,3),]
df.counties <- df.counties[!is.na(df.counties$CountyID),]
#FullSample$CountyID <- ifelse(FullSample$CountyID == "03522", "03159",df.counties$CountyID)
#give correct CountyID to Göttingen manually


#FullSample$county
FullSample[FullSample$date == "2022-06-26 08:59:31",]$CountyID <- "15091" #set manually for wittenberg

FullSample <- left_join(FullSample, df.counties, by = "CountyID")
FullSample<-FullSample[!duplicated(FullSample$date), ]


FullSample$Bevölkerung <- as.numeric(FullSample$Bevölkerung)
FullSample$Bevölkerungsdichte <- as.numeric(FullSample$Bevölkerungsdichte)


#some are not matched correctly!



#03152

#get farms/county area (Anzahl Betriebe/ County Area)
FullSample$farmDens <- FullSample$lwBetr_Anzahl/FullSample$Area
FullSample$farmDens <-as.numeric(FullSample$farmDens)
#get farm area/ county area
FullSample$UAA <- FullSample$UAA*0.01 #make it into hectar
FullSample$areaDens <- FullSample$UAA/FullSample$Area
FullSample$areaDens <-as.numeric(FullSample$areaDens)
#sugar beet area per arable area
FullSample$ShareSB<-FullSample$UAA_Sugarbeet/FullSample$UAA_arable
FullSample$ShareSB<-as.numeric(FullSample$ShareSB)



#share of farms <10ha
FullSample$ShareSmallFarms<- (FullSample$lwBetrUnter5_Anzahl + FullSample$lwBetr5b10_Anzahl) /FullSample$lwBetr_Anzahl
FullSample$ShareSmallFarms<-as.numeric(FullSample$ShareSmallFarms)

#share of area of small farm sin total UAA
FullSample$ShareSmallArea<- (sum(FullSample$UAA_unter5,FullSample$UAA_5b10,na.rm = TRUE)) /FullSample$UAA
FullSample$ShareSmallArea<-as.numeric(FullSample$ShareSmallArea)

#share of arable area in UAA
FullSample$ShareArableUAA<- (FullSample$UAA_arable) /FullSample$UAA
FullSample$ShareArableUAA<-as.numeric(FullSample$ShareArableUAA)

#share of arable UAA in total county area
FullSample$ShareArableInTotalArea<- (FullSample$UAA_arable) /FullSample$Area
FullSample$ShareArableInTotalArea<-as.numeric(FullSample$ShareArableInTotalArea)


FullSample$`Durchschnittliche Betriebsgröße`<-as.numeric(FullSample$`Durchschnittliche Betriebsgröße`)

FullSample$meanFarmSize <- FullSample$`Durchschnittliche Betriebsgröße`

#Coordinates of farms/fields
df.Coordinates<- FullSample %>% dplyr::select(q1_adopt,Long_Centroid, Lat_Centroid,q2_technique, q2_timeframe)
df.Coordinates<-df.Coordinates[!is.na(df.Coordinates$Long_Centroid),]

#write_xlsx(df.Coordinates,"df.Coordinates.xlsx")

#get soil and altitude data from josef
#read soil data on nuts3 ebene von josef
#df.NUTS3 <-read_csv("Backgrounddata/DE_NUTS3_geodata.csv")
#mapping_plz_NUTS<- read.csv("Backgrounddata/pc2020_DE_NUTS-2021_v3.0.csv", sep = ";")
#mapping_plz_NUTS[] <- lapply(mapping_plz_NUTS, function(x) gsub("[]'[()]", "", x))
#join PLZ and NUTS3
#df.NUTS3 <- left_join(df.NUTS3, mapping_plz_NUTS, by = c("NUTS_ID"="NUTS3"))
#df.Nuts3_forAnalysis <- df.NUTS3%>% dplyr::select(NUTS_ID,elev_mean, sand_content, clay_content, PLZ)
df.Nuts3_forAnalysis <- read_excel("Backgrounddata/df.NUTS3_forAnalysis_withslope.xlsx")
df.Nuts3_forAnalysis$PLZ <- as.character(df.Nuts3_forAnalysis$PLZ)
#write_xlsx(df.Nuts3_forAnalysis,"Processed/df.NUTS3_forAnalysis.xlsx")

FullSample<-left_join(FullSample, df.Nuts3_forAnalysis, by = c("NUTS3"="NUTS_ID"))


#get soil and slope data at coordinate level
df.SoilSlope <-read_xlsx("Backgrounddata/df_coordinates_field_JB.xlsx")
df.SoilSlope<- df.SoilSlope %>% dplyr::select(-c(Lat, Long, LatLong))

#need to calculate mean for each observation

#df.Soil_fieldlevel<-df.SoilSlope %>%
 #dplyr::group_by(date) %>%
  #dplyr::summarise_at(vars(1:4), list(mean = mean))

#write_xlsx(df.Soil_fieldlevel,"Processed/df.Soil_fieldlevel.xlsx")
df.Soil_fieldlevel<-read_xlsx("Processed/df.Soil_fieldlevel.xlsx")


#match soil and slope dara to Sample IV
FullSample<- left_join(FullSample,df.Soil_fieldlevel, by = "date")

#remove duplciates
FullSample <-FullSample[!duplicated(FullSample$date), ]

#for those hwre we have only PLZ as sptial data, we take soil data from NUTS3 level
FullSample$slope_in_degrees_mean <- coalesce(FullSample$slope_in_degrees_mean, FullSample$median_slope_in_percent) #slope missing at NUTS level
FullSample$sand_content_percent_mean <- coalesce(FullSample$sand_content_percent_mean, FullSample$sand_content)
FullSample$clay_content_percent_mean <- coalesce(FullSample$clay_content_percent_mean, FullSample$clay_content)
FullSample$elevation_in_m_mean <- coalesce(FullSample$elevation_in_m_mean, FullSample$elev_mean)


#make column for "specialzied in arable farming or not"

FullSample$mainly_crop<- ifelse(FullSample$q7_speci_select == 0,1,0) #crop farmes = 1

#make one column for "sugarbeet region" e.g. SB area > 10% arable area
FullSample$SB_region <- ifelse(FullSample$ShareSB >= 0.1,1,0)#SB area = 1

#create df. of coordinates for SB factories
df.SB_fabriken <-read_xlsx("Backgrounddata/ZR_Fabriken_Coordinaten.xlsx")
#create vector to calculate distance
SBFabrics_coord_m <- cbind(df.SB_fabriken$Lat, df.SB_fabriken$Long)


sf.ZR_fabriken<- st_as_sf(df.SB_fabriken, coords = c("Long", "Lat"), crs = 4326)

#rather sort by region than by ZR fabrik?
#to compare adopters and non-adopters create barplot

#get overview: 
#how many landkreise do we have?
#FullSample$Kreis<- as.factor(FullSample$Kreis)
#levels(FullSample$Kreis) # 87
#table((FullSample[!is.na(FullSample$q4_own),]$Kreis))#18 without coordinates

#calculate distance from each centroid to sugar beet fabric

farm_SugarbeetFabric<-as.data.frame(pointDistance(farms_coord_m, SBFabrics_coord_m,lonlat = TRUE)/1000)
#get minimal distance
farm_SugarbeetFabric$closest_SBfabric = colnames(farm_SugarbeetFabric)[apply(farm_SugarbeetFabric, 1, which.min)]
farm_SugarbeetFabric$minDist_SBfabric <- do.call(pmin, farm_SugarbeetFabric[,1:19])

#add number to dataset such that we can add it to the original one
farm_SugarbeetFabric$number <- rownames(farm_SugarbeetFabric)

farms_coord$number <- rownames(farms_coord)
farm_SugarbeetFabric_clean <- farm_SugarbeetFabric %>% dplyr::select(number,closest_SBfabric, minDist_SBfabric)
#match fabric name to V-number
farm_SugarbeetFabric_clean<-left_join(farm_SugarbeetFabric_clean, df.SB_fabriken, by = "closest_SBfabric" ) 
farms_coord <- left_join(farms_coord, farm_SugarbeetFabric_clean, by = "number") 
farms_coord_done <- dplyr::select(farms_coord,date,Fabrikstandort, advisory,minDist_SBfabric)
FullSample <- left_join(FullSample, farms_coord_done, by = "date")

#create data frame for Josef with all field coordinates in it
#first own fields
#df.coordinates_forJosef <- FullSample[!is.na(FullSample$q4_own),]#|!is.na(FullSample$q5_other),]
#df.coordinates_forJosef<- df.coordinates_forJosef %>%  dplyr::select(date, q4_own)#, q5_other)
#df2 <- cSplit(df.coordinates_forJosef, "q4_own", sep = ",", direction = "long")
#df2b<-as.data.frame(matrix(df2$q4_own, ncol = 2, byrow = TRUE))
#df2<- left_join(df2,df2b, by= c("q4_own" = "V1"))
#df2<-df2[!is.na(df2$V2),]
#df2 <- dplyr::rename(df2, "Long" = 2)
#df2 <- dplyr::rename(df2, "Lat" = 3)

#then nei_fields fields
#df.coordinates_forJosef <- FullSample[!is.na(FullSample$q5_other),]#|!is.na(FullSample$q5_other),]
#df.coordinates_forJosef<- df.coordinates_forJosef %>%  dplyr::select(date, q5_other)#, q5_other)
#df3 <- cSplit(df.coordinates_forJosef, "q5_other", sep = ",", direction = "long")
#df3b<-as.data.frame(matrix(df3$q5_other, ncol = 2, byrow = TRUE))
#df3<- left_join(df3,df3b, by= c("q5_other" = "V1"))
#df3<-df3[!is.na(df3$V2),]
#df3 <- dplyr::rename(df3, "Long" = 2)
#df3 <- dplyr::rename(df3, "Lat" = 3)

#df.coordinates_forJosef_final<- rbind(df2,df3)

#write_xlsx(df.coordinates_forJosef_final,"df.Coordinates14.06b.xlsx")
#get NUTS regions right
#table(FullSample$NUTS3 %in% FullSample$NUTS_ID)
#table(FullSample$NUTS_ID %in% FullSample$NUTS3)
FullSample$NUTS_ID <- FullSample$NUTS3
#table(is.na(FullSample$NUTS_ID))


#get share of adopters per county
#table(FullSample$q1_adopt, FullSample$Kreis)

df.Adopters_County <- FullSample %>% dplyr::select(q1_adopt, Kreis,NUTS_ID)
df.Adopters_County<-df.Adopters_County %>% dplyr::group_by(Kreis) %>% dplyr::mutate(count = n())
df.Adopters_County2<-df.Adopters_County %>% dplyr::group_by(q1_adopt, Kreis) %>% dplyr::mutate(count = n())
df.Adopters_County<- dplyr::rename(df.Adopters_County, "NrObs" = 4)
df.Adopters_County2<- dplyr::rename(df.Adopters_County2, "NrAdopters" = 4)

#remove nonadopters from adopters list
df.Adopters_County2 <- df.Adopters_County2[df.Adopters_County2$q1_adopt == 1,]

#remove duplicates and first row from nrobs list
df.Adopters_County <-df.Adopters_County[!duplicated(df.Adopters_County$Kreis), ]
df.Adopters_County <- df.Adopters_County %>% dplyr::select(Kreis,NUTS_ID, NrObs)
df.Adopters_County2 <-df.Adopters_County2[!duplicated(df.Adopters_County2$Kreis), ]
df.Adopters_County2 <- df.Adopters_County2 %>% dplyr::select(Kreis, NrAdopters)
df.Adopters_County <- left_join(df.Adopters_County, df.Adopters_County2, by = "Kreis")
df.Adopters_County <- df.Adopters_County %>% dplyr::select(Kreis,NUTS_ID, NrObs, NrAdopters)

#those who have no adopters get 0
df.Adopters_County<-df.Adopters_County %>% mutate_at(4, ~replace_na(.,0))

#calculate share of adopters
df.Adopters_County$ShareAdopters <- df.Adopters_County$NrAdopters/ df.Adopters_County$NrObs
df.NUTS_shareAdopters <- df.Adopters_County %>% dplyr::select(NUTS_ID, ShareAdopters)
df.NUTS_shareAdopters <- df.NUTS_shareAdopters[!is.na(df.NUTS_shareAdopters$NUTS_ID),]

#join share of adopters to sampleIV
FullSample <- left_join(FullSample, df.NUTS_shareAdopters, by = c("NUTS_ID"= "NUTS_ID"))


#create necessary variables for models
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
FullSample$info_b<-ifelse(FullSample$q3_info == "0",0,1)

FullSample$info_b <- as.factor(FullSample$info_b)
FullSample$fields_b <- as.numeric(as.character(FullSample$fields_b))
FullSample$fields_dist <- FullSample$fields_b*FullSample$meanDist #0 for those who don't observe fields
FullSample$sq.meanDist <- FullSample$meanDist*FullSample$meanDist
FullSample$fields_b <- as.factor(FullSample$fields_b )
FullSample$sq.fields_dist <- FullSample$fields_dist*FullSample$fields_dist
FullSample$sq.demodist <- FullSample$minDist_demo*FullSample$minDist_demo
FullSample<-FullSample[!duplicated(FullSample$date), ]

FullSample$Peers_cont <- as.numeric(FullSample$q3_info)
FullSample$Fields_cont <- as.numeric(FullSample$NrFields)


#share sb is weird, check in d.full
#d.full$shareSB_check <- d.full$UAA_Sugarbeet/d.full$UAA_arable

#get free answers for enola
#df.comments <- FullSample %>% dplyr::select(date,q2_alt_comment,q8_comment)
#df.comments<- (df.comments[!(is.na(df.comments$q8_comment)&is.na(df.comments$q2_alt_comment)),])
#df.comments$q2_alt_comment <- as.factor(df.comments$q2_alt_comment)
#df.comments$q8_comment <- as.factor(df.comments$q8_comment)
#library(xlsx)
#write.xlsx(df.comments,"df.Freitextantworten.xlsx")



#create variable for if farm is organic or not
FullSample$farm_organic <- ifelse(FullSample$q7_farm == 0,0,1)
FullSample$farm_organic <- as.factor(FullSample$farm_organic )

#create column for choice of distance 
#0 = map, 1= mc
FullSample$selection_distance <- ifelse(FullSample$q5_alt_dist== 6,2,1)
FullSample$selection_distance <- ifelse(is.na(FullSample$meanDist_Nei)== FALSE, 0,FullSample$selection_distance)
FullSample$selection_distance <-as.factor(FullSample$selection_distance )

#read beratungsregionen from enola
df.Beratungsregionen <- read_xlsx("Backgrounddata/MappingBeratungsregionen.XLSX")
df.Beratungsregionen <- df.Beratungsregionen %>% dplyr::select(NR, Verband)

FullSample <- left_join(FullSample, df.Beratungsregionen, by = c("CountyID"= "NR"))
FullSample$Verband <- as.character(FullSample$Verband)

#create mapping of country ID and NUTS nummer

mapping.CountyID_NUTS <- df.counties %>% dplyr::select(CountyID, NUTS3)
mapping.CountyID_NUTS <- mapping.CountyID_NUTS %>% drop_na()
df.Beratungsregionen <- left_join(df.Beratungsregionen, mapping.CountyID_NUTS, by= c("NR" = "CountyID"))


#create squared terms of soil variables
FullSample$sq.sand_content_percent_mean <- FullSample$sand_content_percent_mean*FullSample$sand_content_percent_mean
FullSample$sq.clay_content_percent_mean <- FullSample$clay_content_percent_mean*FullSample$clay_content_percent_mean
FullSample$sq.elevation_in_m_mean <- FullSample$elevation_in_m_mean*FullSample$elevation_in_m_mean
FullSample$sq.slope_in_degrees_mean <- FullSample$slope_in_degrees_mean*FullSample$slope_in_degrees_mean

#minimize cell size of Verband and Fabrikstandort by combining regions close to each other
FullSample$Fabrikstandort_agg <- FullSample$Fabrikstandort
#West = Appeldorn, Euskirchen, Jülich
FullSample$Fabrikstandort_agg <- ifelse(FullSample$Fabrikstandort_agg == "Appeldorn", "West",FullSample$Fabrikstandort_agg)
FullSample$Fabrikstandort_agg <- ifelse(FullSample$Fabrikstandort_agg == "Euskirchen", "West",FullSample$Fabrikstandort_agg)
FullSample$Fabrikstandort_agg <- ifelse(FullSample$Fabrikstandort_agg == "Jülich", "West",FullSample$Fabrikstandort_agg)
#SachsenAnhalt = KleinWanzleben, Könnern, Zeitz
FullSample$Fabrikstandort_agg <- ifelse(FullSample$Fabrikstandort_agg == "Klein Wanzleben", "SachsenAnhalt",FullSample$Fabrikstandort_agg)
FullSample$Fabrikstandort_agg <- ifelse(FullSample$Fabrikstandort_agg == "Könnern", "SachsenAnhalt",FullSample$Fabrikstandort_agg)
FullSample$Fabrikstandort_agg <- ifelse(FullSample$Fabrikstandort_agg == "Zeitz", "SachsenAnhalt",FullSample$Fabrikstandort_agg)

#lage + Nordstemmen = LageNordst.
FullSample$Fabrikstandort_agg <- ifelse(FullSample$Fabrikstandort_agg == "Lage", "LageNordst",FullSample$Fabrikstandort_agg)
FullSample$Fabrikstandort_agg <- ifelse(FullSample$Fabrikstandort_agg == "Nordstemmen", "LageNordst",FullSample$Fabrikstandort_agg)


#same for Verbände
FullSample$Verband_agg <- FullSample$Verband
#Güstzrow + Schleswig-Holstein = GüstrowSH
FullSample$Verband_agg <- ifelse(FullSample$Verband_agg == "Güstrower Rübenanbauerverband", "GüstrowSH",FullSample$Verband_agg)
FullSample$Verband_agg <- ifelse(FullSample$Verband_agg == "Zuckerrübenanbauerverband Schleswig-Holstein", "GüstrowSH",FullSample$Verband_agg)
#Hessisch-Pfälz + Wetterauer + Kassel = HessPflzKassel
FullSample$Verband_agg <- ifelse(FullSample$Verband_agg == "Verband der Hessisch-Pfälzischen Zuckerrübenanbauer", "HessPflzKassel",FullSample$Verband_agg)
FullSample$Verband_agg <- ifelse(FullSample$Verband_agg == "Verband Wetterauer Zuckerrübenanbauer", "HessPflzKassel",FullSample$Verband_agg)
FullSample$Verband_agg <- ifelse(FullSample$Verband_agg == "Verband der Zuckerrübenanbauer Kassel", "HessPflzKassel",FullSample$Verband_agg)
#hunte-weser + lippe-weser = Weser
FullSample$Verband_agg <- ifelse(FullSample$Verband_agg == "Verband der Zuckerrübenanbauer im Hunte-Wese-Gebiet", "Weser",FullSample$Verband_agg)
FullSample$Verband_agg <- ifelse(FullSample$Verband_agg == "Verband der Rübenbauer im Lippe-Weser-Raum", "Weser",FullSample$Verband_agg)
#südniedersachsen + niedersachsenmitte = NiedersachsenMitteSüd
FullSample$Verband_agg <- ifelse(FullSample$Verband_agg == "Zuckerrübenanbauerverband Südniedersachsen", "NiedersachsenMitteSüd",FullSample$Verband_agg)
FullSample$Verband_agg <- ifelse(FullSample$Verband_agg == "Zuckerrübenanbauerverband Niedersachsen-Mitte", "NiedersachsenMitteSüd",FullSample$Verband_agg)
#Ost = Sächsisch-Thüringen, Könnern, Magdeburg
FullSample$Verband_agg <- ifelse(FullSample$Verband_agg == "Zuckerrüben Anbauerverband Könnern", "Ost",FullSample$Verband_agg)
FullSample$Verband_agg <- ifelse(FullSample$Verband_agg == "Zuckerrübenanbauerverband Magdeburg", "Ost",FullSample$Verband_agg)
FullSample$Verband_agg <- ifelse(FullSample$Verband_agg == "Verband Sächsisch-Thüringischer Zuckerrübenanbauer", "Ost",FullSample$Verband_agg)

#get mean distance
mean(FullSample$fields_dist)
#mean(SampleIV$fields_dist)
#mean(AdoptersIV$fields_dist)
#mean(NonAdoptersIV$fields_dist)
#t.test(AdoptersIV$fields_dist,NonAdoptersIV$fields_dist)
#mean(FullSample$meanDist)
FullSample$lwBetr_Anzahl <- as.numeric(FullSample$lwBetr_Anzahl)
FullSample$UAA_unter5 <- as.numeric(FullSample$UAA_unter5)
FullSample$Fabrikstandort_agg <- as.factor(FullSample$Fabrikstandort_agg)
FullSample$Verband_agg <- as.factor(FullSample$Verband_agg)
#calulcat emean farm size from existing data
FullSample$meanFarmSize2 <- (FullSample$UAA/FullSample$lwBetr_Anzahl)*100

#get names right
FullSample <- dplyr::rename(FullSample, population = "Bevölkerung" ,populationdensity = "Bevölkerungsdichte")
FullSample$population <- as.numeric(FullSample$population)
FullSample$populationdensity <- as.numeric(FullSample$populationdensity)


####create distance between own fields

for (i in 1:233){
  ownfields_dist<-as.data.frame(pointDistance(list.centroids[[i]], list.ownfields.spat[[i]]@coords,lonlat = TRUE)/1000)
  ownfields_dist$mean_ownfields_dist <- colMeans(ownfields_dist)
  assign( paste("ownfields_dist",i, sep = "_") ,ownfields_dist)
}
lst.ownfields_dist<- do.call("list",mget(ls(pattern = "ownfields_dist_.*")))

#save all ownfields distances as dataframe
for (i in 1:233){
mean_ownfields_dist<-lst.ownfields_dist[[i]][["mean_ownfields_dist"]][1]
assign( paste("mean_ownfields_dist",i, sep = "_") ,mean_ownfields_dist)
}
lst.mean_ownfields_dist<- do.call("list",mget(ls(pattern = "mean_ownfields_dist_.*")))

#create df
df.ownfields_dist <- data.frame(ID = rep(names(lst.mean_ownfields_dist), sapply(lst.mean_ownfields_dist, length)),
                                      Mean_ownfield_dist = unlist(lst.mean_ownfields_dist))


#now calculated distances have to be merged with sample
#first get rid of unneccessary characters in the ID column

df.ownfields_dist$ID<- gsub("[^0-9.-]", "", df.ownfields_dist$ID)

#get date based on number
df.ownfields_dist <- left_join(df.ownfields_dist, mapping.date_number, by = c("ID"="number"))

#jpin to full sample
FullSample <- left_join(FullSample, df.ownfields_dist, by = "date")

###################################################
##############  SAMPLE IV ########################
##################################################

#create sampel for IV/for models with complete observations
#remove those for which we have no spatial data
SampleIV<-FullSample[!is.na(FullSample$Kreis.x),]
#remove the one delivering to Cosun
SampleIV<-SampleIV[(!SampleIV$advisory == "Cosun"),]
#exxclude those who have NA for age
SampleIV<-SampleIV[!is.na(SampleIV$age_b),]

#drop unnecessary levels fo verband anf fabrikstandort as no cosun
SampleIV$Fabrikstandort_agg <- droplevels(SampleIV$Fabrikstandort_agg )
SampleIV$Verband_agg <- droplevels(SampleIV$Verband_agg)

vtable(SampleIV,missing = TRUE )
write_xlsx(SampleIV,"Processed/SampleIV.xlsx")
SampleIV<-read_xlsx("Processed/SampleIV.xlsx")
#create dataframe with all info we have at Kreislevel to then identify outliers

###########################################################
###### df.Kreise                                      ####
##########################################################

df.Kreise <- d.full[d.full$JAHR == 2016,]
df.Kreise <- left_join(df.Kreise, mapping.CountyID_NUTS, by = c("KREISE" = "CountyID"))
df.Kreise <- left_join(df.Kreise, df.Nuts3_forAnalysis, by = c("NUTS3" = "NUTS_ID") )
df.Kreise<-df.Kreise[!duplicated(df.Kreise$KREISE), ]
df.Kreise <- left_join(df.Kreise, df.MeanFarmSize, by = "KREISE")
df.Kreise <- dplyr::rename(df.Kreise, "lwBetr_Anzahl" = BTR010)
df.Kreise <- dplyr::rename(df.Kreise, "lwBetrOrganic_Anzahl" = BTR030)
df.Kreise <- dplyr::rename(df.Kreise, "lwBetrUnter5_Anzahl" = BTR010_FLCHA000B050)
df.Kreise <- dplyr::rename(df.Kreise, "lwBetr5b10_Anzahl" = BTR010_FLCHA050B100)
df.Kreise <- dplyr::rename(df.Kreise, "UAA" = FLC017)
df.Kreise <- dplyr::rename(df.Kreise, "UAA_Organic" = FLC047)
df.Kreise <- dplyr::rename(df.Kreise, "UAA_unter5" = FLC017_FLCHA000B050)
df.Kreise <- dplyr::rename(df.Kreise, "UAA_5b10" = FLC017_FLCHA050B100)
df.Kreise <- dplyr::rename(df.Kreise, "UAA_Sugarbeet" = `FLC004_BNZAT-2132`)
df.Kreise <- dplyr::rename(df.Kreise, "UAA_arable" = `FLC004_BNZAT-21`)
df.Kreise <- dplyr::rename(df.Kreise, "meanFarmSize" = `Durchschnittliche Betriebsgröße`)
df.Kreise$ShareOrgFarms <- (df.Kreise$lwBetrOrganic_Anzahl/ df.Kreise$lwBetr_Anzahl)
df.Kreise$ShareOrgArea <- (df.Kreise$UAA_Organic/df.Kreise$UAA)
df.Kreise <- left_join(df.Kreise, df.counties, by = "NUTS3")
df.Kreise$farmDens <- df.Kreise$lwBetr_Anzahl/df.Kreise$Area
df.Kreise$farmDens <-as.numeric(df.Kreise$farmDens)


#get farm area/ county area
df.Kreise$UAA <- df.Kreise$UAA*0.01 #make it into hectar
df.Kreise$areaDens <- df.Kreise$UAA/df.Kreise$Area
df.Kreise$areaDens <-as.numeric(df.Kreise$areaDens)
#sugar beet area per arable area

#df.Kreise$ShareSB<-as.numeric(df.Kreise$ShareSB)
df.Kreise$ShareSmallFarms<- (df.Kreise$lwBetrUnter5_Anzahl + df.Kreise$lwBetr5b10_Anzahl) /df.Kreise$lwBetr_Anzahl
df.Kreise$ShareSmallFarms<-as.numeric(df.Kreise$ShareSmallFarms)


df.Kreise$ShareSmallArea<- (df.Kreise$UAA_unter5 + df.Kreise$UAA_5b10 ) /df.Kreise$UAA
df.Kreise$ShareSmallArea<-as.numeric(df.Kreise$ShareSmallArea)

df.Kreise$sq.elev_mean <- df.Kreise$elev_mean^2
df.Kreise$sq.sand_content <- df.Kreise$sand_content^2
df.Kreise$sq.clay_content <- df.Kreise$clay_content^2
df.Kreise$ShareSB<-df.Kreise$UAA_Sugarbeet/df.Kreise$UAA_arable

df.Kreise$meanFarmSize2 <- (df.Kreise$UAA/df.Kreise$lwBetr_Anzahl)*100



#merge rhlplz data to df.kreise
df.Kreise <- left_join(df.Kreise, df.organic_rhlpflz, by = "Kreis")
df.Kreise$lwBetrOrganic_Anzahl<- ifelse(!is.na(df.Kreise$org_anzahl),df.Kreise$org_anzahl,df.Kreise$lwBetrOrganic_Anzahl )
df.Kreise$UAA_Organic <- ifelse(!is.na(df.Kreise$org_area),df.Kreise$org_area,df.Kreise$UAA_Organic)
df.Kreise$ShareOrgFarms<- ifelse(!is.na(df.Kreise$org_anzahl_percent),df.Kreise$org_anzahl_percent,df.Kreise$ShareOrgArea )
df.Kreise$ShareOrgArea<- ifelse(!is.na(df.Kreise$org_area_percent),df.Kreise$org_area_percent,df.Kreise$ShareOrgArea )

#add data manually
df.Kreise$UAA_Organic <-ifelse(df.Kreise$Kreis == "Donnersbergkreis",1909,df.Kreise$UAA_Organic)
df.Kreise$lwBetrOrganic_Anzahl <-ifelse(df.Kreise$Kreis == "Donnersbergkreis",31,df.Kreise$lwBetrOrganic_Anzahl)

#for eckernförde: https://www.statistik-nord.de/fileadmin/Dokumente/Faltbl%C3%A4tter/Faltbl%C3%A4tter_Landwirtschaft/Faltblatt_Landwirtschaft_RD-ECK.pdf
df.Kreise$UAA_Organic <-ifelse(df.Kreise$Kreis == "Kreis Rendsburg-Eckernförde",3596,df.Kreise$UAA_Organic)
df.Kreise$lwBetrOrganic_Anzahl <-ifelse(df.Kreise$Kreis == "Kreis Rendsburg-Eckernförde",65,df.Kreise$lwBetrOrganic_Anzahl)

#Wittenberg:https://www.bauernzeitung.de/news/sachsen-anhalt/oekologischer-landbau-im-aufwind/
df.Kreise$UAA_Organic <-ifelse(df.Kreise$Kreis == "Wittenberg",17404,df.Kreise$UAA_Organic)
df.Kreise$lwBetrOrganic_Anzahl <-ifelse(df.Kreise$Kreis == "Wittenberg",98,df.Kreise$lwBetrOrganic_Anzahl)

#add UAA and lw. betreibe allgmeien for eckernförde
df.Kreise$lwBetr_Anzahl <-ifelse(df.Kreise$Kreis == "Kreis Rendsburg-Eckernförde",1697,df.Kreise$lwBetr_Anzahl)
df.Kreise$UAA <-ifelse(df.Kreise$Kreis == "Kreis Rendsburg-Eckernförde",138682,df.Kreise$UAA)


#add manually for wittenberg
df.Kreise$ShareOrgArea <-ifelse(df.Kreise$Kreis == "Wittenberg",0.25,df.Kreise$ShareOrgArea)
df.Kreise$ShareOrgFarms <-ifelse(df.Kreise$Kreis == "Wittenberg",0.32,df.Kreise$ShareOrgFarms)

#calculate abse don shares UAA and lw. betriebe
df.Kreise$lwBetr_Anzahl <-ifelse(df.Kreise$Kreis == "Wittenberg",306,df.Kreise$lwBetr_Anzahl)
df.Kreise$UAA <-ifelse(df.Kreise$Kreis == "Wittenberg",69616,df.Kreise$UAA)



#create df
df.Kreise_Lasso <-df.Kreise %>% 
  dplyr::select(KREISE.x,Kreis,lwBetr_Anzahl,lwBetrOrganic_Anzahl,lwBetrUnter5_Anzahl,lwBetr5b10_Anzahl,UAA_unter5,UAA_5b10,
                         UAA,UAA_Organic,UAA_Sugarbeet,UAA_arable,meanFarmSize2,ShareOrgFarms,ShareOrgArea,Area,Bevölkerung,Bevölkerungsdichte,farmDens,areaDens,ShareSB,ShareSmallFarms,
                         elev_mean,sand_content,clay_content,sq.elev_mean,sq.sand_content,sq.clay_content)
df.Kreise_Lasso <- dplyr::rename(df.Kreise_Lasso, population = "Bevölkerung" ,populationdensity = "Bevölkerungsdichte")



df.Kreise_Lasso$meanFarmSize2 <- as.numeric(df.Kreise_Lasso$meanFarmSize2)
df.Kreise_Lasso$population <- as.numeric(df.Kreise_Lasso$population)
df.Kreise_Lasso$populationdensity <- as.numeric(df.Kreise_Lasso$populationdensity)
df.Kreise_Lasso<-df.Kreise_Lasso[!duplicated(df.Kreise_Lasso$KREISE.x), ]
vtable(df.Kreise_Lasso,missing = TRUE )

write_xlsx(df.Kreise_Lasso,"Processed/df.Kreise_lasso.xlsx")
df.Kreise_Lasso<-read_xlsx("Processed/df.Kreise_lasso.xlsx")
#tabelle for Enola missing UAA unter5
df.missingUAAunter5 <- FullSample %>% dplyr::select(Kreis.x, UAA_unter5)
df.missingUAAunter5 <-df.missingUAAunter5[is.na(df.missingUAAunter5$UAA_unter5),]
df.missingUAAunter5<-df.missingUAAunter5[!duplicated(df.missingUAAunter5$Kreis.x), ]
#write_xlsx(df.missingUAAunter5,"df.missingUAAunter5.xlsx")


#get data for landkreise where share of sugabreet is missing
df.missingShareSB <- FullSample %>% dplyr::select(Kreis.x, ShareSB)
df.missingShareSB <-df.missingShareSB[is.na(df.missingShareSB$ShareSB),]
df.missingShareSB<-df.missingShareSB[!duplicated(df.missingShareSB$Kreis.x), ]
#write_xlsx(df.missingShareSB,"df.missingShareSB.xlsx")

#no sb area:
SampleIV[is.na(SampleIV$elevation_in_m_mean),]$county
