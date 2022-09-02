#Maps

library(ggpubr)
library(mapview)
library(sf)
library(leaflet)
library(tidyverse)
library(leafem)
library(webshot)
library(htmlwidgets)
library(farver)

#save all participants centroid as sf
sf.Coordinates<- st_as_sf(df.Coordinates, coords = c("Long_Centroid", "Lat_Centroid"), crs = 4326)
plot(sf.Coordinates)
sf.coo<-mapview::mapview(sf.Coordinates, cex = 3,zcol= "q1_adopt", col.regions = c("#66A61E","#E6AB02"))
sp.techniques<- mapview::mapview(sf.Coordinates,zcol= "q2_timeframe", cex = 4)
#mapview::mapview(spdf, zcol="ShareSB",col.regions = brewer.pal(19, "Greens"))+sf.coo
#add sugar beet factories
sf.ZR_fabriken<- st_as_sf(df.SB_fabriken, coords = c("Long", "Lat"), crs = 4326)
zr_fabriken<- mapview::mapview(sf.ZR_fabriken,zcol= "Fabrikstandort", col.regions=c("darkred"), cex = 4)
zr_fabriken <- zr_fabriken %>%
  leafem::addStaticLabels(label = sf.ZR_fabriken$Fabrikstandort,
                          noHide = FALSE,
                          direction = 'left',
                          offset = c(20,10),
                          textOnly = TRUE,
                          textsize = "10px",
                          opacity = 10,
                          data = sf.ZR_fabriken)


saveWidget(zr_fabriken, file = paste0(getwd(), "/map.zr_fabriken.html"))
webshot::webshot(url = paste0(getwd(), "/map.zr_fabriken.html"), file = paste0(getwd(),"/map.zr_fabriken.jpg"), debug=T)

map.Fabrikstandorte <-mapview::mapview(spdf, zcol="ShareSB", col.regions = brewer.pal(20,"Greys"))+sf.coo#+zr_fabriken 
saveWidget(map.Fabrikstandorte, file = paste0(getwd(), "/map.Fabrikstandorte.html"))
webshot::webshot(url = paste0(getwd(), "/map.Fabrikstandorte.html"), file = paste0(getwd(),"/map.Fabrikstandorte.jpg"), debug=T)

map.Verbaende <- mapview::mapview(spdf, zcol="Verband")#, col.regions = brewer.pal(17,"Greens"))#+sf.coo+zr_fabriken
saveWidget(map.Verbaende@map, file = paste0(getwd(), "/map.Verbaende.html"))
webshot::webshot(url = paste0(getwd(), "/map.Verbaende.html"), file = paste0(getwd(),"/map.Verbaende.jpg"), debug=T)

map.shareadopters<-mapview::mapview(spdf, zcol="ShareAdopters", col.regions = brewer.pal(20,"Greens"))#+zr_fabriken
saveWidget(map.shareadopters@map, file = paste0(getwd(), "/map.shareadopters.html"))
webshot::webshot(url = paste0(getwd(), "/map.shareadopters.html"), file = paste0(getwd(),"/map.shareadopters.jpg"), debug=T)

mapShareadoptersfabriken <- map.shareadopters+zr_fabriken
mapShareadoptersfabriken<- mapShareadoptersfabriken %>%
  leafem::addStaticLabels(label = sf.ZR_fabriken$Fabrikstandort,
                          noHide = FALSE,
                          direction = 'left',
                          offset = c(20,10),
                          textOnly = TRUE,
                          textsize = "10px",
                          opacity = 10)
saveWidget(mapShareadoptersfabriken, file = paste0(getwd(), "/mapShareadoptersfabriken.html"))
webshot::webshot(url = paste0(getwd(), "/mapShareadoptersfabriken.html"), file = paste0(getwd(),"/mapShareadoptersfabriken.jpg"), debug=T)

map.sand_content<-mapview::mapview(spdf, zcol="sand_content", col.regions = brewer.pal(20,"Reds"))
saveWidget(map.sand_content, file = paste0(getwd(), "/map.sand_content.html"))
webshot::webshot(url = paste0(getwd(), "/map.sand_content.html"), file = paste0(getwd(),"/map.sand_content.jpg"), debug=T)

map.clay_content<-mapview::mapview(spdf, zcol="clay_content", col.regions = brewer.pal(20,"Reds"))
saveWidget(map.clay_content, file = paste0(getwd(), "/map.clay_content.html"))
webshot::webshot(url = paste0(getwd(), "/map.clay_content.html"), file = paste0(getwd(),"/map.clay_content.jpg"), debug=T)

map.elevation<-mapview::mapview(spdf, zcol="elev_mean", col.regions = brewer.pal(20,"Reds"))#
saveWidget(map.elevation, file = paste0(getwd(), "/map.elevation.html"))
webshot::webshot(url = paste0(getwd(), "/map.elevation.html"), file = paste0(getwd(),"/map.elevation.jpg"), debug=T)

#webshot::install_phantomjs()
#mapshot(map.Verbaende, url = "map_Verbaende.html", remove_controls = TRUE)
#mapshot(map.shareadopters, url = "map_ShareAdopters.html", remove_controls = TRUE)
#mapshot(map.sand_content, url = "map_sandcontent.html", remove_controls = TRUE)
#mapshot(map.clay_content, url = "map_claycontent.html", remove_controls = TRUE)
#mapshot(map.elevation, url = "map_elevation.html", remove_controls = TRUE)
#mapshot(map.Fabrikstandorte, url = "map_Fabrikstandorte.html", remove_controls = TRUE)
