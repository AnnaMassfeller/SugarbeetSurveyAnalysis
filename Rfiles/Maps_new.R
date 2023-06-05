##create new maps NOT useing mapview

pacman::p_load("scales","plyr","dplyr", "tidyverse", "readxl", "stringr", "scales", "RColorBrewer", "rprojroot", "lazyeval", "skimr","ggpubr","ggplot2")
pacman::p_load("microbenchmark","rdhs","geosphere")
pacman::p_load("OpenStreetMap", "DT", "RColorBrewer", "mapproj", "sf", "RgoogleMaps",
       "scales", "rworldmap", "maps", "tidyverse", "rnaturalearth",
       "rnaturalearthdata", "rgeos", "ggspatial", "maptools", "leaflet", "sf",
       "tmap", "here", "rgdal", "scales","ggmap","gtsummary","gt","sjPlot")
pacman::p_load("ggrepel","tidyverse","rnaturalearth","rnaturalearthdata","hutilscpp","ggalt","ggforce","concaveman")
#https://github.com/r-spatial/sf/issues/1856 ##needed as otherwise we cannot use coord_sf after package update


#original code from cata

#plot datapoints
# load packages
pacman::p_load("tidyverse","rnaturalearth","rnaturalearthdata","hutilscpp")
# load data
#world <- ne_countries(scale = "medium", returnclass = "sf")
#plot(world)
#create file that contains alle info we want to plot
spdf_formaps <- spdf
#soildata, taked ata from df.Kreise_lasso
#NR = "Kreise

#first remove unnecessary columns
spdf_formaps <- spdf_formaps[ -c(3,4,7:15,17:21)]

#add missing NUTS_IDS for Göttingen, Rhein-Hunsrück-Kreis and Cochem_zell
spdf_formaps$RS <- ifelse(spdf_formaps$GEN == "Göttingen","03152",spdf_formaps$RS)
spdf_formaps$NUTS <- ifelse(spdf_formaps$GEN == "Göttingen","DE91C",spdf_formaps$NUTS)
spdf_formaps$NUTS <- ifelse(spdf_formaps$GEN == "Rhein-Hunsrück-Kreis","DEB1D",spdf_formaps$NUTS)
spdf_formaps$NUTS <- ifelse(spdf_formaps$GEN == "Cochem-Zell","DEB1C",spdf_formaps$NUTS)


#add all data on kreislevel
spdf_formaps <- left_join(spdf_formaps, df.Kreise_Lasso, by = c("RS" = "KREISE.x"))
spdf_formaps <- left_join(spdf_formaps, df.NUTS_shareAdopters, by = c("NUTS"="NUTS_ID"))
spdf_formaps <- left_join(spdf_formaps, df.Beratungsregionen, by = c("NUTS"="NUTS3"))

#set vrbände for hamburg and Belrin to those sourrounding
spdf_formaps$Verband <- ifelse(spdf_formaps$GEN == "Hamburg","Güstrower Rübenanbauerverband",spdf_formaps$Verband)
spdf_formaps$Verband <- ifelse(spdf_formaps$GEN == "Berlin","Zuckerrüben Anbauerverband Könnern",spdf_formaps$Verband)


#aggregate verbände
spdf_formaps$Verband_agg <- spdf_formaps$Verband
#Güstzrow + Schleswig-Holstein = GüstrowSH
spdf_formaps$Verband_agg <- ifelse(spdf_formaps$Verband_agg == "Güstrower Rübenanbauerverband", "GüstrowSH",spdf_formaps$Verband_agg)
spdf_formaps$Verband_agg <- ifelse(spdf_formaps$Verband_agg == "Zuckerrübenanbauerverband Schleswig-Holstein", "GüstrowSH",spdf_formaps$Verband_agg)
#Hessisch-Pfälz + Wetterauer + Kassel = HessPflzKassel
spdf_formaps$Verband_agg <- ifelse(spdf_formaps$Verband_agg == "Verband der Hessisch-Pfälzischen Zuckerrübenanbauer", "HessPflzKassel",spdf_formaps$Verband_agg)
spdf_formaps$Verband_agg <- ifelse(spdf_formaps$Verband_agg == "Verband Wetterauer Zuckerrübenanbauer", "HessPflzKassel",spdf_formaps$Verband_agg)
spdf_formaps$Verband_agg <- ifelse(spdf_formaps$Verband_agg == "Verband der Zuckerrübenanbauer Kassel", "HessPflzKassel",spdf_formaps$Verband_agg)
#hunte-weser + lippe-weser = Weser
spdf_formaps$Verband_agg <- ifelse(spdf_formaps$Verband_agg == "Verband der Zuckerrübenanbauer im Hunte-Wese-Gebiet", "Weser",spdf_formaps$Verband_agg)
spdf_formaps$Verband_agg <- ifelse(spdf_formaps$Verband_agg == "Verband der Rübenbauer im Lippe-Weser-Raum", "Weser",spdf_formaps$Verband_agg)
#südniedersachsen + niedersachsenmitte = NiedersachsenMitteSüd
spdf_formaps$Verband_agg <- ifelse(spdf_formaps$Verband_agg == "Zuckerrübenanbauerverband Südniedersachsen", "NiedersachsenMitteSüd",spdf_formaps$Verband_agg)
spdf_formaps$Verband_agg <- ifelse(spdf_formaps$Verband_agg == "Zuckerrübenanbauerverband Niedersachsen-Mitte", "NiedersachsenMitteSüd",spdf_formaps$Verband_agg)
#Ost = Sächsisch-Thüringen, Könnern, Magdeburg
spdf_formaps$Verband_agg <- ifelse(spdf_formaps$Verband_agg == "Zuckerrüben Anbauerverband Könnern", "Ost",spdf_formaps$Verband_agg)
spdf_formaps$Verband_agg <- ifelse(spdf_formaps$Verband_agg == "Zuckerrübenanbauerverband Magdeburg", "Ost",spdf_formaps$Verband_agg)
spdf_formaps$Verband_agg <- ifelse(spdf_formaps$Verband_agg == "Verband Sächsisch-Thüringischer Zuckerrübenanbauer", "Ost",spdf_formaps$Verband_agg)

spdf_formaps <-spdf_formaps[!duplicated(spdf_formaps$RS), ]
#check where share adopters is missing although we have observations there
#SampleIV[is.na(SampleIV$ShareAdopters == TRUE),]$Kreis.x

#delete dublicated col-names
#spdf_formaps <- spdf_formaps[!duplicated(as.list(spdf_formaps))]
#spdf_formaps <- spdf_formaps[-c(120)]

#create shares on county ebene
#first change column names
#spdf_formaps <- dplyr::rename(spdf_formaps, "lwBetr_Anzahl" = BTR010)
#spdf_formaps <- dplyr::rename(spdf_formaps, "lwBetrOrganic_Anzahl" = BTR030)
#spdf_formaps <- dplyr::rename(spdf_formaps, "lwBetrUnter5_Anzahl" = BTR010_FLCHA000B050)
#spdf_formaps <- dplyr::rename(spdf_formaps, "lwBetr5b10_Anzahl" = BTR010_FLCHA050B100)
#spdf_formaps <- dplyr::rename(spdf_formaps, "UAA" = FLC017)
#spdf_formaps <- dplyr::rename(spdf_formaps, "UAA_Organic" = FLC047)
#spdf_formaps <- dplyr::rename(spdf_formaps, "UAA_unter5" = FLC017_FLCHA000B050)
#spdf_formaps <- dplyr::rename(spdf_formaps, "UAA_5b10" = FLC017_FLCHA050B100)
#spdf_formaps <- dplyr::rename(spdf_formaps, "UAA_Sugarbeet" = `FLC004_BNZAT-2132`)
#spdf_formaps <- dplyr::rename(spdf_formaps, "UAA_arable" = `FLC004_BNZAT-21`)
#mean farm size in the county
spdf_formaps$meanFarmSize2 <- (spdf_formaps$UAA/spdf_formaps$BTR010)*100

#get share of UAA in total area and farm density
spdf_formaps$UAA <- spdf_formaps$UAA*0.01 #make it into hectar
spdf_formaps$areaDens <- spdf_formaps$UAA/spdf_formaps$Area
spdf_formaps$areaDens <-as.numeric(spdf_formaps$areaDens)

spdf_formaps$farmDens <- spdf_formaps$lwBetr_Anzahl/spdf_formaps$Area
spdf_formaps$farmDens <-as.numeric(spdf_formaps$farmDens)

spdf_formaps$lwBetrOrganic_Anzahl <- as.numeric(spdf_formaps$lwBetrOrganic_Anzahl)
spdf_formaps$lwBetr_Anzahl <- as.numeric(spdf_formaps$lwBetr_Anzahl)
spdf_formaps$UAA_Organic <- as.numeric(spdf_formaps$UAA_Organic)
spdf_formaps$UAA <- as.numeric(spdf_formaps$UAA)

spdf_formaps$ShareOrgFarms <- (spdf_formaps$lwBetrOrganic_Anzahl/ spdf_formaps$lwBetr_Anzahl)
spdf_formaps$ShareOrgArea <- (spdf_formaps$UAA_Organic/spdf_formaps$UAA)

spdf_formaps$ShareOrgArea <- as.numeric(spdf_formaps$ShareOrgArea)
spdf_formaps$ShareOrgFarms <- as.numeric(spdf_formaps$ShareOrgFarms)

#scale all share sbetween 0 and 1
#spdf_formaps$ShareOrgArea <- rescale(spdf_formaps$ShareOrgArea) 
#spdf_formaps$ShareOrgFarms <- rescale(spdf_formaps$ShareOrgFarms) 

#share sb
spdf_formaps$ShareSB<-spdf_formaps$`FLC004_BNZAT-2132`/spdf_formaps$`FLC004_BNZAT-21`
spdf_formaps$ShareSB<-as.numeric(spdf_formaps$ShareSB)
#set to 0 for those where we have no data
spdf_formaps$ShareSB <- ifelse(is.na(spdf_formaps$ShareSB)== TRUE, 0,spdf_formaps$ShareSB)

#write_xlsx(spdf_formaps,"Processed/spdf_formaps.xlsx")
#spdf_formaps<-read_xlsx("Processed/spdf_formaps.xlsx")

###create map
# Germany map
options(ggrepel.max.overlaps = Inf)

#share of small farms
SmallFarms_plot<-ggplot(data = spdf_formaps) +
  #add filling of landkreise accoridng to variable of choice
  geom_sf(aes(fill = ShareSmallFarms ))+#,size = 0.01, alpha = 1)+
  scale_fill_distiller("Share of small farms",palette = "Greens", direction = 1)+
  #now add SB fabrics and farms that participated in the survey
 # farms
# geom_point(data = df.Coordinates,
 #            aes(x = Long_Centroid, y = Lat_Centroid,colour = factor(q1_adopt)),
  #           alpha=4,size = 2)+
  #scale_color_manual("Mechanical weeding",values = c("0" = "orange", "1" = "darkgreen"),labels = c("No", "Yes"))+
 #general make up of map
   theme_bw()+
  labs(x = "", y = "") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks=element_blank(),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"))+
  coord_sf()+
  #sb fabrics
  geom_point(data = df.SB_fabriken,
             aes(x = Long, y = Lat),
             alpha=1,shape = 23,size = 2, fill = "darkred")+
  geom_label_repel(data=df.SB_fabriken, aes(Long, Lat, label=Fabrikstandort), size = 4,
                   min.segment.length = 0,box.padding = 0.5)#+
 #theme(legend.position="none")
#ggsave(SmallFarms_plot,file="Output/map_sharesmallfarms_fabrics.tiff",dpi = 300,unit="cm",height=35,width=50)

#share organic farms
OrgFarms_plot<-ggplot(data = spdf_formaps) +
  #add filling of landkreise accoridng to variable of choice
  geom_sf(aes(geometry = geometry,fill = ShareOrgFarms ))+#,size = 0.01, alpha = 1)+
  scale_fill_distiller("Share of organic farms",palette = "Greens", direction = 1)+
  #now add SB fabrics and farms that participated in the survey
  # farms
  # geom_point(data = df.Coordinates,
  #            aes(x = Long_Centroid, y = Lat_Centroid,colour = factor(q1_adopt)),
  #           alpha=4,size = 2)+
  #scale_color_manual("Mechanical weeding",values = c("0" = "orange", "1" = "darkgreen"),labels = c("No", "Yes"))+
  #general make up of map
  theme_bw()+
  labs(x = "", y = "") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks=element_blank(),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"))+
  coord_sf()+
  #sb fabrics
  geom_point(data = df.SB_fabriken,
             aes(x = Long, y = Lat),
             alpha=1,shape = 23,size = 2, fill = "darkred")+
  geom_label_repel(data=df.SB_fabriken, aes(Long, Lat, label=Fabrikstandort), size = 4,
                   min.segment.length = 0,box.padding = 0.5)#+
#theme(legend.position="none")
ggsave(OrgFarms_plot,file="Output/map_shareorgfarms_fabrics.tiff",dpi = 300,unit="cm",height=35,width=50)

#share organic area
OrgArea_plot<-ggplot(data = spdf_formaps) +
  #add filling of landkreise accoridng to variable of choice
  geom_sf(aes(fill = ShareOrgArea))+#,size = 0.01, alpha = 1)+
  scale_fill_distiller("Share of organic area in total UAA",palette = "Greens", direction = 1)+
  #now add SB fabrics and farms that participated in the survey
  # farms
  # geom_point(data = df.Coordinates,
  #            aes(x = Long_Centroid, y = Lat_Centroid,colour = factor(q1_adopt)),
  #           alpha=4,size = 2)+
  #scale_color_manual("Mechanical weeding",values = c("0" = "orange", "1" = "darkgreen"),labels = c("No", "Yes"))+
  #general make up of map
  theme_bw()+
  labs(x = "", y = "") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks=element_blank(),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"))+
  coord_sf()+
  #sb fabrics
  geom_point(data = df.SB_fabriken,
             aes(x = Long, y = Lat),
             alpha=1,shape = 23,size = 2, fill = "darkred")+
  geom_label_repel(data=df.SB_fabriken, aes(Long, Lat, label=Fabrikstandort), size = 4,
                   min.segment.length = 0,box.padding = 0.5)#+
#theme(legend.position="none")
ggsave(OrgArea_plot,file="Output/map_shareorgarea_fabrics.tiff",dpi = 300,unit="cm",height=35,width=50)



#share of small farms
ShareAdopters_plot<-ggplot(data = spdf_formaps) +
  #add filling of landkreise accoridng to variable of choice
  geom_sf(aes(fill = ShareAdopters ))+#,size = 0.01, alpha = 1)+
  scale_fill_distiller("Share of adopters",palette = "Greens", direction = 1,na.value="white")+
  #now add SB fabrics and farms that participated in the survey
  # farms
 geom_point(data = df.Coordinates,
              aes(x = Long_Centroid, y = Lat_Centroid,colour = factor(q1_adopt)),
            alpha=4,size = 3)+
scale_color_manual("Mechanical weeding",values = c("0" = "orange", "1" = "darkblue"),labels = c("No", "Yes"))+
  #general make up of map
  theme_bw()+
  labs(x = "", y = "") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks=element_blank(),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"))+
  coord_sf()+
  #sb fabrics
  #geom_point(data = df.SB_fabriken,
   #          aes(x = Long, y = Lat),
    #         alpha=1,shape = 23,size = 3, fill = "darkred")+
  #geom_label_repel(data=df.SB_fabriken, aes(Long, Lat, label=Fabrikstandort), size = 5,
   #                min.segment.length = 0,box.padding = 0.5)+
theme(legend.position="none")
ggsave(ShareAdopters_plot,file="map_shareadopters_fabrics_formagazines.tiff",dpi = 300,unit="cm",height=35,width=50)

#farm density
FarmDensity_plot<-ggplot(data = spdf_formaps) +
  #add filling of landkreise accoridng to variable of choice
  geom_sf(aes(fill = farmDens ))+#,size = 0.01, alpha = 1)+
  scale_fill_distiller("Farm density",palette = "Greens", direction = 1)+
  #now add SB fabrics and farms that participated in the survey
  # farms
  # geom_point(data = df.Coordinates,
  #            aes(x = Long_Centroid, y = Lat_Centroid,colour = factor(q1_adopt)),
  #           alpha=4,size = 2)+
  #scale_color_manual("Mechanical weeding",values = c("0" = "orange", "1" = "darkgreen"),labels = c("No", "Yes"))+
  #general make up of map
  theme_bw()+
  labs(x = "", y = "") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks=element_blank(),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"))+
  coord_sf()+
  #sb fabrics
  geom_point(data = df.SB_fabriken,
             aes(x = Long, y = Lat),
             alpha=1,shape = 23,size = 2, fill = "darkred")+
  geom_label_repel(data=df.SB_fabriken, aes(Long, Lat, label=Fabrikstandort), size = 4,
                   min.segment.length = 0,box.padding = 0.5)#+
#theme(legend.position="none")
ggsave(FarmDensity_plot,file="Output/map_farmdensity_fabrics.tiff",dpi = 300,unit="cm",height=35,width=50)


#farm density
AreaDensity_plot<-ggplot(data = spdf_formaps) +
  #add filling of landkreise accoridng to variable of choice
  geom_sf(aes(fill = areaDens ))+#,size = 0.01, alpha = 1)+
  scale_fill_distiller("UAA density",palette = "Greens", direction = 1)+
  #now add SB fabrics and farms that participated in the survey
  # farms
  # geom_point(data = df.Coordinates,
  #            aes(x = Long_Centroid, y = Lat_Centroid,colour = factor(q1_adopt)),
  #           alpha=4,size = 2)+
  #scale_color_manual("Mechanical weeding",values = c("0" = "orange", "1" = "darkgreen"),labels = c("No", "Yes"))+
  #general make up of map
  theme_bw()+
  labs(x = "", y = "") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks=element_blank(),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"))+
  coord_sf()+
  #sb fabrics
  geom_point(data = df.SB_fabriken,
             aes(x = Long, y = Lat),
             alpha=1,shape = 23,size = 2, fill = "darkred")+
  geom_label_repel(data=df.SB_fabriken, aes(Long, Lat, label=Fabrikstandort), size = 4,
                   min.segment.length = 0,box.padding = 0.5)#+
#theme(legend.position="none")
ggsave(AreaDensity_plot,file="Output/map_areadensity_fabrics.tiff",dpi = 300,unit="cm",height=35,width=50)

#clay
clay_plot<-ggplot(data = spdf_formaps) +
  #add filling of landkreise accoridng to variable of choice
  geom_sf(aes(fill =clay_content.y))+#,size = 0.01, alpha = 1)+
  scale_fill_distiller("Share of clay",palette = "Greens", direction = 1)+
  #now add SB fabrics and farms that participated in the survey
  # farms
  # geom_point(data = df.Coordinates,
  #            aes(x = Long_Centroid, y = Lat_Centroid,colour = factor(q1_adopt)),
  #           alpha=4,size = 2)+
  #scale_color_manual("Mechanical weeding",values = c("0" = "orange", "1" = "darkgreen"),labels = c("No", "Yes"))+
  #general make up of map
  theme_bw()+
  labs(x = "", y = "") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks=element_blank(),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"))+
  coord_sf()+
  #sb fabrics
  geom_point(data = df.SB_fabriken,
             aes(x = Long, y = Lat),
             alpha=1,shape = 23,size = 2, fill = "darkred")+
  geom_label_repel(data=df.SB_fabriken, aes(Long, Lat, label=Fabrikstandort), size = 4,
                   min.segment.length = 0,box.padding = 0.5)#+
#theme(legend.position="none")
ggsave(clay_plot,file="Output/map_clay_fabrics.tiff",dpi = 300,unit="cm",height=35,width=50)

#clay
sand_plot<-ggplot(data = spdf_formaps) +
  #add filling of landkreise accoridng to variable of choice
  geom_sf(aes(fill =sand_content.y))+#,size = 0.01, alpha = 1)+
  scale_fill_distiller("Share of sand",palette = "Greens", direction = 1)+
  #now add SB fabrics and farms that participated in the survey
  # farms
  # geom_point(data = df.Coordinates,
  #            aes(x = Long_Centroid, y = Lat_Centroid,colour = factor(q1_adopt)),
  #           alpha=4,size = 2)+
  #scale_color_manual("Mechanical weeding",values = c("0" = "orange", "1" = "darkgreen"),labels = c("No", "Yes"))+
  #general make up of map
  theme_bw()+
  labs(x = "", y = "") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks=element_blank(),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"))+
  coord_sf()+
  #sb fabrics
  geom_point(data = df.SB_fabriken,
             aes(x = Long, y = Lat),
             alpha=1,shape = 23,size = 2, fill = "darkred")+
  geom_label_repel(data=df.SB_fabriken, aes(Long, Lat, label=Fabrikstandort), size = 4,
                   min.segment.length = 0,box.padding = 0.5)#+
#theme(legend.position="none")
ggsave(sand_plot,file="Output/map_sand_fabrics.tiff",dpi = 300,unit="cm",height=35,width=50)


#elevation
elev_plot<-ggplot(data = spdf_formaps) +
  #add filling of landkreise accoridng to variable of choice
  geom_sf(aes(fill =elev_mean.y))+#,size = 0.01, alpha = 1)+
  scale_fill_distiller("mean elevation in m",palette = "Greens", direction = 1)+
  #now add SB fabrics and farms that participated in the survey
  # farms
  # geom_point(data = df.Coordinates,
  #            aes(x = Long_Centroid, y = Lat_Centroid,colour = factor(q1_adopt)),
  #           alpha=4,size = 2)+
  #scale_color_manual("Mechanical weeding",values = c("0" = "orange", "1" = "darkgreen"),labels = c("No", "Yes"))+
  #general make up of map
  theme_bw()+
  labs(x = "", y = "") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks=element_blank(),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"))+
  coord_sf()+
  #sb fabrics
  geom_point(data = df.SB_fabriken,
             aes(x = Long, y = Lat),
             alpha=1,shape = 23,size = 2, fill = "darkred")+
  geom_label_repel(data=df.SB_fabriken, aes(Long, Lat, label=Fabrikstandort), size = 4,
                   min.segment.length = 0,box.padding = 0.5)#+
#theme(legend.position="none")
ggsave(elev_plot,file="Output/map_elevation_fabrics.tiff",dpi = 300,unit="cm",height=35,width=50)


verband_plot<-ggplot(data = spdf_formaps) +
  #add filling of landkreise accoridng to variable of choice
  geom_sf(aes(fill = Verband_agg))+#,size = 0.01, alpha = 1)+
 # scale_fill_distiller("Verbände",palette = "Greens", direction = 1)+
  #now add SB fabrics and farms that participated in the survey
  # farms
  # geom_point(data = df.Coordinates,
  #            aes(x = Long_Centroid, y = Lat_Centroid,colour = factor(q1_adopt)),
  #           alpha=4,size = 2)+
  #scale_color_manual("Mechanical weeding",values = c("0" = "orange", "1" = "darkgreen"),labels = c("No", "Yes"))+
  #general make up of map
  theme_bw()+
  labs(x = "", y = "") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks=element_blank(),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"))+
  coord_sf()+
  #sb fabrics
  geom_point(data = df.SB_fabriken,
             aes(x = Long, y = Lat),
             alpha=1,shape = 23,size = 2, fill = "darkred")+
  geom_label_repel(data=df.SB_fabriken, aes(Long, Lat, label=Fabrikstandort), size = 6,
                   min.segment.length = 0,box.padding = 0.5)#+
#theme(legend.position="none")
ggsave(verband_plot,file="map_verband_fabrics.tiff",dpi = 300,unit="cm",height=35,width=50)


farmsize_plot<-ggplot(data = spdf_formaps) +
  #add filling of landkreise accoridng to variable of choice
  geom_sf(aes(fill = meanFarmSize2))+#,size = 0.01, alpha = 1)+
  scale_fill_distiller("mean farm size in ha",palette = "Greens", direction = 1)+
  #now add SB fabrics and farms that participated in the survey
  # farms
  # geom_point(data = df.Coordinates,
  #            aes(x = Long_Centroid, y = Lat_Centroid,colour = factor(q1_adopt)),
  #           alpha=4,size = 2)+
  #scale_color_manual("Mechanical weeding",values = c("0" = "orange", "1" = "darkgreen"),labels = c("No", "Yes"))+
  #general make up of map
  theme_bw()+
  labs(x = "", y = "") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks=element_blank(),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"))+
  coord_sf()+
  #sb fabrics
  geom_point(data = df.SB_fabriken,
             aes(x = Long, y = Lat),
             alpha=1,shape = 23,size = 2, fill = "darkred")+
  geom_label_repel(data=df.SB_fabriken, aes(Long, Lat, label=Fabrikstandort), size = 4,
                   min.segment.length = 0,box.padding = 0.5)#+
#theme(legend.position="none")
ggsave(farmsize_plot,file="Output/map_farmsize_fabrics.tiff",dpi = 300,unit="cm",height=35,width=50)

SB_plot<-ggplot(data = spdf_formaps) +
  #add filling of landkreise accoridng to variable of choice
  geom_sf(aes(fill = ShareSB))+#,size = 0.01, alpha = 1)+
  scale_fill_distiller("Share of Sugar beet",palette = "Greens", direction = 1)+
  #now add SB fabrics and farms that participated in the survey
  # farms
  # geom_point(data = df.Coordinates,
  #            aes(x = Long_Centroid, y = Lat_Centroid,colour = factor(q1_adopt)),
  #           alpha=4,size = 2)+
  #scale_color_manual("Mechanical weeding",values = c("0" = "orange", "1" = "darkgreen"),labels = c("No", "Yes"))+
  #general make up of map
  theme_bw()+
  labs(x = "", y = "") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks=element_blank(),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"))+
  coord_sf()+
  #sb fabrics
#  geom_point(data = df.SB_fabriken,
 #            aes(x = Long, y = Lat),
  #           alpha=1,shape = 23,size = 2, fill = "darkred")+
  #geom_label_repel(data=df.SB_fabriken, aes(Long, Lat, label=Fabrikstandort), size = 4,
   #                min.segment.length = 0,box.padding = 0.5)#+
theme(legend.position="none")
ggsave(SB_plot,file="Output/map_SB_fabrics.tiff",dpi = 300,unit="cm",height=35,width=50)


SB_farms_plot<-ggplot(data = spdf_formaps) +
  #add filling of landkreise accoridng to variable of choice
  geom_sf(aes(fill = ShareSB))+#,size = 0.01, alpha = 1)+
  scale_fill_distiller("Share of Sugar beet",palette = "Greens", direction = 1)+
  #now add SB fabrics and farms that participated in the survey
  # farms
   geom_point(data = df.Coordinates,
              aes(x = Long_Centroid, y = Lat_Centroid,colour = factor(q1_adopt)),
             alpha=4,size = 2)+
  scale_color_manual("Mechanical weeding",values = c("0" = "orange", "1" = "darkblue"),labels = c("No", "Yes"))+
  #general make up of map
  theme_bw()+
  labs(x = "", y = "") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks=element_blank(),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"))+
  coord_sf()+
  #sb fabrics
  geom_point(data = df.SB_fabriken,
             aes(x = Long, y = Lat),
             alpha=1,shape = 23,size = 2, fill = "darkred")+
  geom_label_repel(data=df.SB_fabriken, aes(Long, Lat, label=Fabrikstandort), size = 4,
                   min.segment.length = 0,box.padding = 0.5)#+
#theme(legend.position="none")
ggsave(SB_farms_plot,file="Output/map_SB_farms_fabrics.tiff",dpi = 300,unit="cm",height=35,width=50)

#add demonstration farms
Demo_coord_map <- Demo_coord
Demo_coord_map['type'] = 'demonstration farm'

SB_farms_demo_plot<-ggplot(data = spdf_formaps) +
  #add filling of landkreise accoridng to variable of choice
  geom_sf(aes(fill = ShareSB))+#,size = 0.01, alpha = 1)+
  scale_fill_distiller("Share of Sugar beet",palette = "Greys", direction = 1)+
  #now add demo farms, SB fabrics and farms that participated in the survey
  #demo farms
  geom_point(data = Demo_coord_map,
             aes(x = DemoOrg_lon, y = DemoOrg_lat),
             alpha=4,size = 2, shape = 8, colour = "orange", show.legend = TRUE)+
  # farms
  geom_point(data = df.Coordinates,
             aes(x = Long_Centroid, y = Lat_Centroid,colour = factor(q1_adopt)),
             alpha=4,size = 2, shape = 16)+
  scale_color_manual("Mechanical weeding",values = c("0" = "hotpink3", "1" = "royalblue2"),labels = c("No", "Yes"))+
  #general make up of map
  theme_bw()+
  labs(x = "", y = "") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks=element_blank(),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"))+
  coord_sf()+
  #sb fabrics
  geom_point(data = df.SB_fabriken,
             aes(x = Long, y = Lat),
             alpha=1,shape = 23,size = 2, fill = "darkred")+
  geom_label_repel(data=df.SB_fabriken, aes(Long, Lat, label=Fabrikstandort), size = 2,
                   min.segment.length = 0,box.padding = 0.5)+
  theme(legend.position="none")
ggsave(SB_farms_demo_plot,file="Output/map_SB_farms_demo_fabrics.tiff")#,dpi = 800,unit="cm",height=35,width=50)


#share adopters for magazones
ShareAdopters2_plot<-ggplot(data = spdf_formaps) +
  #add filling of landkreise accoridng to variable of choice
  geom_sf(aes(fill = ShareAdopters ))+#,size = 0.01, alpha = 1)+
  scale_fill_distiller("Share of adopters",palette = "Greens", direction = 1,na.value="white")+
  #sb fabrics
  geom_point(data = df.SB_fabriken,
             aes(x = Long, y = Lat),
             alpha=1,shape = 23,size = 1, fill = "darkred")+
  geom_label_repel(data=df.SB_fabriken, aes(Long, Lat, label=Fabrikstandort), size = 3,
                   min.segment.length = 0,box.padding = 0.5)+
  theme_bw()+
  labs(x = "", y = "") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks=element_blank(),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"))+
  coord_sf()+
  #sb fabrics
  #geom_point(data = df.SB_fabriken,
  #          aes(x = Long, y = Lat),
  #         alpha=1,shape = 23,size = 3, fill = "darkred")+
  #geom_label_repel(data=df.SB_fabriken, aes(Long, Lat, label=Fabrikstandort), size = 5,
  #                min.segment.length = 0,box.padding = 0.5)+
  theme(legend.position="none")
ggsave(ShareAdopters2_plot,file="map_shareadopters_fabrics_formagazines2.tiff",dpi = 300,unit="cm",height=20,width=12)



