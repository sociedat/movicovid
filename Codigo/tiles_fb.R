rm(list = ls())

library(tidyverse)
library(sp)
library(deldir)
library(dismo)
library(rgdal)

# library(stringi)
# library(sf)
# library(rgdal)
# library(raster)
# library(pracma)
# library(leaflet)
# library(cholera)
# library(ggforce)
# library(ggvoronoi)

data <- read_csv("inp/tiles/Mexico City Coronavirus Disease Prevention Map Feb 28 2020 Id  Movement between Tiles__2020-06-29 0000.csv")
data <- janitor::clean_names(data)

tempo <- data %>% 
        dplyr::select(geometry) %>% 
        separate(col = geometry, into = c("long", "tres"), sep = ",", remove = F) %>% 
        dplyr::mutate(long = str_sub(long, 13, nchar(long))) %>% 
        separate(col = long, into = c("long", "lat"), sep = " ") %>% 
        mutate(tres = str_sub(tres, 2,nchar(tres))) %>% 
        separate(col = tres, into = c("cuatro", "cinco"), sep = " ") %>% 
        mutate(cinco = str_sub(cinco, 1,nchar(cinco)-1)) %>% 
        mutate(v3 = paste0("(", long, ",", lat, "),(", cuatro, ",", cinco)) %>% 
        filter(!is.na(long) | !is.na(lat) | !is.na(cuatro) | !is.na(cinco)) %>% 
        dplyr::select(long, lat) %>% 
        mutate(
                long = as.numeric(long),
                lat = as.numeric(lat)
        )

tempo_points <- SpatialPointsDataFrame(tempo[,c("long", "lat")],
                                        tempo,
                                        proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
plot(tempo_points)

tempo_polygon <- voronoi(tempo_points)
plot(tempo_polygon)
tempoF <- fortify(tempo_polygon)

writeOGR(tempo_polygon, dsn="out/shape/poligonos_fb.shp", layer="facebook", driver="ESRI Shapefile")
write.csv(tempoF, "out/shape/poligonos.csv", row.names = FALSE, fileEncoding = 'UTF-8')

