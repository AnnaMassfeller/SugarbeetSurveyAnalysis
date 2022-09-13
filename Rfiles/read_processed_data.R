##################################
###file to read in processed data#
##################################
library(readxl)
#########################
####from zensus_api######
#########################


d.full<-read_xlsx("Processed/d.full.xlsx")
# merge with spatial data

# set worknig directory
#setwd("C:/Users/schulz/sciebo/PhenoRob/grassland_biodiversity")

# locate shapefile
library(sf)
shp_path <- "C:/Users/Massfeller/sciebo/PhD/Paper/Paper2_socioSpat/SurveyAnalysis_new/SurveyAnalysis_new/Backgrounddata/Geodata/Kreisgrenzen_2017_mit_Einwohnerzahl"
spdf <- read_sf(dsn = shp_path, layer = "Kreisgrenzen_2017_mit_Einwohnerzahl")

#make göttingen right
spdf$RS <- ifelse(spdf$GEN == "Göttingen","03152",spdf$RS)
spdf$NUTS <- ifelse(spdf$GEN == "Göttingen","DE91C",spdf$NUTS)

d.full$KREISE <- as.character(d.full$KREISE)
#create column for SB area
#d.full$ShareSB<-d.full$`FLC004_BNZAT-2132`/d.full$`FLC004_BNZAT-21`
#d.full$ShareSB<-as.numeric(d.full$ShareSB)
# merge data for year 2020
spdf <- left_join(spdf, d.full %>% filter(JAHR==2016), by=c("RS"="KREISE"))
#spdf <- left_join(spdf, df.NUTS3, by = c("NUTS"="NUTS_ID"))
spdf<-spdf[!duplicated(spdf$RS), ]

######################
##from processing_new#
######################

rev_geo<-read_xlsx("Processed/rev_geo.xlsx")
df.Kreise_lasso<-read_xlsx("Processed/df.Kreise_lasso.xlsx")
DemoOrganic_coord<-read_xlsx("Processed/DemoOrganic_coord.xlsx")
SampleIV<-read_xlsx("Processed/SampleIV.xlsx")

SampleIV$q1_adopt <- as.factor(SampleIV$q1_adopt)
SampleIV$info_b <- as.factor(SampleIV$info_b)
SampleIV$fields_b <- as.factor(SampleIV$fields_b)
SampleIV$age_b <- as.factor(SampleIV$age_b)
SampleIV$AES_b <- as.factor(SampleIV$AES_b)
SampleIV$farm_organic <- as.factor(SampleIV$farm_organic)
SampleIV$mainly_crop <- as.factor(SampleIV$mainly_crop)
SampleIV$Fabrikstandort_agg <- as.factor(SampleIV$Fabrikstandort_agg)
SampleIV$Verband_agg <- as.factor(SampleIV$Verband_agg )
SampleIV$q6_col1 <- as.factor(SampleIV$q6_col1)
SampleIV$q6_col2 <- as.factor(SampleIV$q6_col2)
SampleIV$q6_col3 <- as.factor(SampleIV$q6_col3)





