#install.packages("wiesbaden")

#rm(list=ls())
library(tidyverse)
library(wiesbaden)
library(janitor) # clean_names
library(Hmisc) # match column names

#save_credentials(db="regio",user="", password = "") #own username and password needed
test_login(genesis=c(db='regio'))

# retrieve list of tables
tl <- retrieve_datalist(tableseries="41141*", genesis=c(db='regio')) %>%
  filter(str_detect(description, "Kreise") ) 



prepper = function(tablename){
  # download table
  df <- retrieve_data(tablename=tablename, genesis=c(db='regio'))
  
  # set missing data to NA
  quality_columns <- names(df) %>% 
    subset(grepl("_qual", .))
  
  if(length(quality_columns)>0){
    for (i in 1:length(quality_columns)) {
      value_column <- paste0(gsub("qual", "", quality_columns[i]),"val")
      # set erroneous value to NA
      df[value_column][df[quality_columns[i]]!="e"] <- NA
    }
  }
  
  # remove error info etc.
  df <- df[,colnames(df)[!grepl("qual|lock|err|id41141", colnames(df))]]
  
  # clean column names
  colnames(df) <- gsub("_val", "", colnames(df))
  
  # Stichtag -> Jahr
  if(any(names(df)=="STAG")){
    df$JAHR <- as.numeric(substr(df$STAG, 7,10) )
    df$STAG <- NULL
  }
  
  # get meta data
  md <- data.frame(retrieve_metadata(tablename=tablename, 
                                     genesis=c(db='regio'))) %>%
    mutate(var_name = paste(description, unit)) %>%
    dplyr::select(all_of(c("name", "var_name")))
  
  
  
  #md1 <- md$var_name
  #names(md1) <- md$name

  #colnames(df)[4:ncol(df)] <- make_clean_names(md$var_name)
  label(df) = as.list(md$var_name[match(names(df), md$name)])
  label(df$JAHR) <- "Jahr"
  return(df)
}

dl <- lapply(tl$tablename,prepper)

# some tables are partially in long format
# 1) recognize factor columns
# 2) retrieve factor metadata
# 3) transform into wide format preserving column names
no_labs <- c("KREISE", "JAHR")

ll <- lapply(dl, names) %>% unlist() %>%
  # remove unnecessary ID-columns
  subset(!. %in% no_labs) %>%
  # retrieve value labels
  lapply(., function(x){retrieve_valuelabel(x, 
                               genesis=c(db='regio'))})
  
# subset columns with actual values
ll1 <- ll[ll %>% lapply(.,nrow) %>% 
            lapply(.,is.null) %>% 
            lapply(., isFALSE) %>% 
            unlist()]

# define columns of interest (for long -> wide conversion)
CoI <- lapply(ll1, '[',1) %>% lapply(., names) %>% unlist()

# conditional long to wide
for (i in 1:length(dl)) {
  
  if(any(names(dl[[i]]) %in% CoI)){
    
    VoI <- names(dl[[i]])[names(dl[[i]]) %in% CoI]
    
    dl[[i]] <- reshape(dl[[i]], 
                               idvar = c("KREISE","JAHR"), 
                               timevar = VoI, 
                               direction = "wide",
                               sep = "_")
  }
}


# merge all tables
d.full <- dl %>% reduce(left_join, by = c("KREISE","JAHR"))



lab_dict <- data.frame(var=lapply(ll1, '[[', 1) %>% unlist(),
                       lab=lapply(ll1, '[[', 'description') %>% unlist()) %>%
  filter(!duplicated(.))


for (i in 1:length(names(d.full))) {
  raw_name <- names(d.full)[i]
  new_name <- strsplit(raw_name, "_")[[1]][2]
  raw_lab <- label(d.full)[i]
  # check whether specification needed
  if(new_name %in% lab_dict$var){
    append_lab <- lab_dict$lab[which(lab_dict$var==new_name)]
    new_lab <- paste0(raw_lab," (",append_lab ,")")
    Hmisc::label(d.full[,i]) <- new_lab
    #cat(paste("Found", new_name, "=", append_lab, "\n"))
  }
}



# finally add location names
kreis_namen <- retrieve_valuelabel("KREISE", genesis=c(db='regio'))

label(kreis_namen$KREISE) <- "Kreise und kreisfreie Städte "
names(kreis_namen) <- c("KREISE", "Kreis")

d.full <- left_join(d.full, kreis_namen, by="KREISE")
# reorder
d.full <- d.full[c(1, ncol(d.full), 2:(ncol(d.full)-1))]
View(d.full)


label(d.full)
#write_xlsx(d.full,"Processed/d.full.xlsx")
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
spdf <- left_join(spdf, df.NUTS3, by = c("NUTS"="NUTS_ID"))
spdf<-spdf[!duplicated(spdf$RS), ]
#spdf <- left_join(spdf, df.NUTS_shareAdopters, by = c("NUTS"="NUTS_ID"))
#spdf <- left_join(spdf, df.Beratungsregionen, by = c("NUTS"="NUTS3"))
#remove columns that are not needed in spdf

#spdf <- spdf %>% dplyr::select(BEZ, geometry.x, Kreis.x,elev_mean, sand_content,clay_content,ShareAdopters, Verband, ShareSB)
#write_xlsx(spdf,"Processed/spdf.xlsx")
#spdf<-read_xlsx("Processed/spdf.xlsx")

mapview::mapview(spdf, zcol="FLC047")


# apparently 2020 data for Rheinland Pfalz is not available yet

