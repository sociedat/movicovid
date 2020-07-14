rm(list = ls())

library(tidyverse)
library(sp)
library(sf)
library(rgeos)
library(rgdal)
library(maptools)

tiles <- readOGR("out/shape/poligonos_fb.shp", stringsAsFactors = F)
tiles <- sp::spTransform(tiles, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
tilesF <- fortify(tiles)

mydir <- list.files("./inp/shp/ageb/edo/", pattern="*.shp", full.names=T, recursive=FALSE)

ageb_list <- lapply(mydir, readOGR)

ageb <- rbind(ageb_list[[1]], ageb_list[[2]])
ageb <- sp::spTransform(ageb, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
agebF <- fortify(ageb)
# plot(ageb)

rm(ageb_list)

ageb_tile <- over(tiles, ageb) #

tiles@data$CVE_AGEB <- ageb_tile$CVE_AGEB
tiles@data$CVE_ENT <- ageb_tile$CVE_ENT
tiles@data$CVE_MUN <- ageb_tile$CVE_MUN
tiles@data$ID_TILE <- seq(1:715)

ageb_tile <- over(ageb, tiles) #

ageb@data <- cbind(ageb@data, ageb_tile$ID_TILE)
agebF <- fortify(ageb, region = "ageb_tile$ID_TILE")
agebF$id <- as.numeric(agebF$id)
agebF <- left_join(agebF, ageb_tile, by=c('id' = 'ID_TILE'))

tilesF <- fortify(tiles, group = "CVE_AGEB")

writeOGR(tiles, dsn="out/shape/tiles_ageb.shp", layer="facebook", driver="ESRI Shapefile")
write.csv(agebF, "out/shape/tiles_ageb.csv", row.names = FALSE, fileEncoding = 'UTF-8')

# ggplot() +
#         geom_polygon(agebF, mapping = aes(x=long.x, y = lat.x, group = group, fill = CVE_ENT)) +
#         geom_polygon(tilesF, mapping = aes(x=long, y = lat, group = group), color = "black", fill = NA) +
#         theme(
#                 legend.position = "none"
#         )



