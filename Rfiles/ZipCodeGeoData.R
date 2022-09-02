#make sure the workspace is in pristine condition
rm(list=ls(all=TRUE))
#shapefile from https://datahub.io/de/dataset/postal-codes-de
#http://www.suche-postleitzahl.org/downloads?download=zuordnung_plz_ort.csv
#post questions here: http://gis.stackexchange.com/
library(choroplethr)
library(dplyr)
library(ggplot2)
library(rgdal)
library(maptools)
library(gpclib)
library(readr)
library(R6)
library(sf)

ger_plz <- st_read("C:/Users/Massfeller/sciebo/PhD/Paper/Paper2_socioSpat/AnalysisPaper2/Data_prep/PLZGEO/plz-5stellig.shp")


