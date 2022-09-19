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

SampleIV <- apply_labels(SampleIV,q1_adopt="Adoption mechanical weeding binary",
                         fields_b="observing fields binary",
                         info_b="knowing adopters binary",
                         minDist_demo="minimal distance to demonstration farm",
                         sq.demodist="squared minimal distance to demonstration farm",
                         Fabrikstandort_agg="sugar factory location aggregated",
                         farmsize_b="farm size in ha over 50 binary",
                         AES_b="participation in AES binary",
                         age_b="farmer age over 45 binary",
                         farm_organic="production type binary",
                         meanFarmSize2="mean farm size at county level",
                         UAA_unter5="UAA in farms with less than 5ha at county level",
                         UAA_Sugarbeet="UAA where sugarbeet is grown at county level",
                         SB_region="share of SB in UAA larger 10% binary",
                         ShareSB="Share of UAA for Sugarbeets in arable UAA at county level",
                         lwBetr_Anzahl="number of farms in county",
                         lwBetrOrganic_Anzahl="number of farms with organic agriculture at county level",
                         lwBetrUnter5_Anzahl="number of farms with less than 5ha at county level",
                         lwBetr5b10_Anzahl="number of farms with 5-10ha at county level",
                         UAA_5b10="UAA in farms with 5- 10ha at county level",
                         UAA="UAA at county level",
                         UAA_Organic="organic UAA at county level",
                         UAA_arable="arable UAA at county level",
                         ShareOrgFarms="Share of organic farms in all farms at county level",
                         ShareOrgArea="Share of organic area in UAA at county level",
                         Area="total area county",
                         population="Number of habitants at county level",
                         populationdensity="habitants per sq.km",
                         farmDens="farms per sq.km",
                         areaDens="UAA per total county area",
                         ShareSmallFarms="Share of small farms (< 10ha) in all farms at county level",
                         elevation_in_m_mean="mean elevation at county or field level",
                         sand_content_percent_mean="mean sand content in soil at county or field level",
                         clay_content_percent_mean="mean clay content in soil at county or field level",
                         sq.elevation_in_m_mean="sq.mean elevation at county or field level",
                         sq.sand_content_percent_mean="sq.mean sand content in soil at county or field level",
                         sq.clay_content_percent_mean="sq.mean clay content in soil at county or field level",
                         mainly_crop="farm specialized in crops/ arable farming binary",
                         Verband_agg = "producer associations aggregated",
                         meanDist = "mean distance to other farmers' fields, set to 50 for NA",
                         sq.meanDist ="squared mean distance to other farmers' fields",
                         fields_dist ="interaction of fields_b*meanDist =0 for non-observators",
                         sq.fields_dist = "squared distance to other farmers' fields with 0 for non-observators",
                         ShareSmallArea= "share of area of farms wih 5 to 10 ha in total UAA",
                         ShareArableUAA = "share of arable area in total UAA", 
                         ShareArableInTotalArea = "share of arable area in total county area")



