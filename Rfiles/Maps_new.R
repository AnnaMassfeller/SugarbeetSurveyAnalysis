##create new maps NOT useing mapview

pacman::p_load("plyr","dplyr", "tidyverse", "readxl", "stringr", "scales", "RColorBrewer", "rprojroot", "lazyeval", "skimr","ggpubr","ggplot2")
pacman::p_load("microbenchmark","rdhs","geosphere")
pacman::p_load("OpenStreetMap", "DT", "RColorBrewer", "mapproj", "sf", "RgoogleMaps",
       "scales", "rworldmap", "maps", "tidyverse", "rnaturalearth",
       "rnaturalearthdata", "rgeos", "ggspatial", "maptools", "leaflet", "sf",
       "tmap", "here", "rgdal", "scales","ggmap","gtsummary","gt","sjPlot")
pacman::p_load("tidyverse","rnaturalearth","rnaturalearthdata","hutilscpp","ggalt","ggforce","concaveman")
#https://github.com/r-spatial/sf/issues/1856 ##needed as otherwise we cannot use coord_sf after package update


#original code from cata

#plot datapoints
# load packages
pacman::p_load("tidyverse","rnaturalearth","rnaturalearthdata","hutilscpp")
# load data
#world <- ne_countries(scale = "medium", returnclass = "sf")
#plot(world)
#create file that contains alle info we want to plot
#verband_agg, soildata, taked ata from df.Kreise_lasso

spdf_formaps <- spdf
spdf_formaps <- left_join(spdf_formaps, df.NUTS3, by = c("NUTS"="NUTS_ID"))
spdf_formaps <- left_join(spdf_formaps, df.NUTS_shareAdopters, by = c("NUTS"="NUTS_ID"))
spdf_formaps <- left_join(spdf_formaps, df.Beratungsregionen, by = c("NUTS"="NUTS3"))

# Germany map
Germany_plot<-ggplot(data = spdf_formaps) +
  geom_sf() +#mapping = aes(fill = ShareAdopters)
#  labs( x = "Longitude", y = "Latitude") +
 # ggtitle("Household and market data in the DHS survey year"
          #, subtitle = paste0("(", length(unique(world$admin)), " countries)")
 # )+
  #now add SB fabrics and farms that participate din the survey
  #SB fabrics
  geom_point(data = df.SB_fabriken,
             aes(x = Long, y = Lat,col = "Sugar beet farbric"),
             size = 5,alpha=1)+
  #farms
  geom_point(data = df.Coordinates,
             aes(x = Long_Centroid, y = Lat_Centroid,colour = factor(q1_adopt)),
             size = .75)+
  theme_bw()+
  coord_sf()
Germany_plot
ggsave(Germany_plot,file="Output/map_farms_and_fabrics.tiff",dpi = 300,unit="cm",height=35,width=50)








