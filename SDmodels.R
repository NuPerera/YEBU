install.packages("remotes")
install.packages("auk")
install.packages("tidyverse")
install.packages("rnaturalearth")
install.packages("dismo")
install.packages("maps")
install.packages("mapdata")
install.packages("CoordinateCleaner")
install.packages("rgeos")
install.packages("rJava")
remotes::install_github("mstrimas/ebppackages")

remotes::install_github("CornellLabofOrnithology/auk", force = TRUE)
library(remotes)
library(dplyr)
library(auk)
library(lubridate)
library(sf)
library(gridExtra)
library(tidyverse)
library(rnaturalearth)
library(dplyr)
library(dismo)
library(maptools)
library(maps)
library(mapdata)
library(raster)
library(CoordinateCleaner)
library(rgeos)
library(rJava)

#read files
ebd<- read_tsv("/Users/gamageperera/Desktop/YEBU/data/ebird/ebd_LK_yeebul1_201104_202104_relFeb-2021.txt")

#subset
ebd_subset<-c("SCIENTIFIC NAME", "OBSERVATION COUNT", "LATITUDE", "LONGITUDE", "OBSERVATION DATE")
ebd_selected<-ebd[ebd_subset]
write.csv(ebd_selected,"/Users/gamageperera/Desktop/YEBU/data/ebird\\yebu_ebd.csv", row.names = FALSE)
yebu_ebd<-read_csv("/Users/gamageperera/Desktop/YEBU/data/ebird\\yebu_ebd.csv")
yebu_ebd<-yebu_ebd[,3:4]
yebu_ebd

map.scale(160,0,relwidth = 0.15, metric = TRUE, ratio = TRUE)
map('worldHires', 'Sri Lanka')

# plot points
points(yebu_ebd$LONGITUDE, yebu_ebd$LATITUDE, col='orange', pch=20, cex=0.75)
# plot points again to add a border, for better visibility
points(yebu_ebd$LONGITUDE, yebu_ebd$LATITUDE, col='red', cex=0.75)

#Make spatial points dataframe, which will have a spatial extent
spdf <- SpatialPointsDataFrame( ebd_selected[ c("LONGITUDE" , "LATITUDE") ] , 
                                data = data.frame( ebd_selected$`SCIENTIFIC NAME` ) , 
                                proj4string = CRS("+proj=longlat +datum=WGS84") )

#subsetting range of lat long
ebd_cleaned<- data.frame(species=letters[1:1232],
                         decimallatitude = runif(1232, min = 6.4, max = 7.6),
                         decimallongitude = runif(1232, min= 80.2, max = 81.1))

ebd_cleaned


#map of the occurence localities
data(wrld_simpl)
plot(wrld_simpl, xlim=c(80.6,80.6), ylim=c(5.5,10), axes=TRUE, col="light yellow")


#Trying a different map
map('worldHires', 'Sri Lanka')

# add the points
points(ebd_cleaned$decimallongitude, ebd_cleaned$decimallatitude, col='orange', pch=20, cex=0.75)
# selected points in red
points(ebd_cleaned, cex=1, col='red', pch='x')






#make raster based on the extent of your data
r <- raster( extent( spdf ) )

# set the resolution of the cells to (for example) 1 degree
res(r) <- 1
# expand (extend) the extent of the RasterLayer a little
r <- extend(r, extent(r)+1)
# sample:
yebu <- gridSample(spdf, r, n=1)
# to illustrate the method and show the result
p <- rasterToPolygons(r)
plot(p, border='gray')
points(spdf)
# selected points in red
points(spdf, cex=1, col='red', pch='x')

