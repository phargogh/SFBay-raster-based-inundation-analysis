#script attempts to intersect San Mateo County OLUs with habitat/species areas to calculate area of each habitat/species in each OLU
#using this as a starting point https://gis.stackexchange.com/questions/140504/extracting-intersection-areas-in-r
# intersect v ginterect https://gis.stackexchange.com/questions/276928/raster-intersect-and-gintersection-give-different-results-in-r 


library(sp)
library(raster)
library(rgdal)
library(rgeos)
library(maptools)

setwd("D:/NatCap/SF_Bay/")

##example for a single habitat and geographic unit that works 
OLU <- readOGR(dsn ="D:/NatCap/SF_Bay/SMC", layer = "SMC_OLUs_complete_subtidal_albersconical")
wmonarch <- readOGR(dsn ="D:/NatCap/SF_Bay/habitat/hab_albersconical", layer = "western_monarch")
#names(wmonarch)[names(wmonarch)=="gridcode"] <- "monarch_suit"
#intersect habitat/species with AOI
i_wmonarch <- intersect (OLU, wmonarch)
#plot to display intersection 
plot(OLU, axes=T); plot(wmonarch, add=T); plot(i_wmonarch, add=T, col='red')
# Extract areas from polygon objects (convert sq m to sq km) then attach as attribute
i_wmonarch$area <- area(i_wmonarch) / 1000000
#summarize area of habitat/species by OLU
agg_wmonarch <- aggregate(i_wmonarch$area ~ OLU_ID, data=i_wmonarch, FUN=sum)
#head(agg_wmonarch)
#note that, where there are multiple codes for a single habitat, each combo will be summed seperately 
#aggregate(i_wmonarch$area ~ monarch_suit + OLU_ID, data=i_wmonarch, FUN=sum)
write.csv(agg_wmonarch, "D:/NatCap/SF_Bay/habitat/hab_albersconical/agg_wmonarch.csv")



# Some generalized code that mostly works.  
#I've only tested it with OLUs and not the conservation network or individual PCAs
#It's doing what it's supposed to (i.e. there are the correct CSVs in the folder that contain the data I want), but it seems to stall on the eel_grass data
#At this point, the script outputs CSVs and not the shapefile, which is fine for now


setwd("D:/NatCap/SF_Bay/habitat/hab_albersconical")
OLU <- readOGR(dsn ="D:/NatCap/SF_Bay/SMC", layer = "SMC_OLUs_complete_subtidal_albersconical")
shps <- list.files(pattern="*.shp$")

for (f in shps) {
  
  #sub out the shp with nothing (i.e. remove the .shp)
  fs <- gsub(".shp", "", f)
  print(fs)
  hab <- readOGR(dsn =".", layer = fs)
  #plot(hab)
  
    hab_aoi <- intersect(OLU, hab) # this creates a NULL object if they don't intersect 
  
try(
  if(is.null(hab_aoi)){
  agg_hab <- cbind(OLU@data, data.frame(habarea=0))
} else {
    hab_aoi$habarea <- area(hab_aoi) / 1000000 
  agg_hab <- aggregate(hab_aoi$habarea ~ OLU_ID, data=hab_aoi, FUN=sum, na.rm=TRUE)
}
)
   write.csv(agg_hab, paste0(fs, "_area.csv"), row.names=F)
}


for (f in shps) {
  
  #sub out the shp with nothing (i.e. remove the .shp)
  fs <- gsub(".shp", "", f)
  print(fs)
  hab <- readOGR(dsn =".", layer = fs)
  #plot(hab)
  
  hab_aoi <- intersect(OLU, hab) # this creates a NULL object if they don't intersect 
  
  try(
    if(is.null(hab_aoi)){
      agg_hab <- cbind(OLU@data, data.frame(habarea=0))
    } else {
      hab_aoi$habarea <- area(hab_aoi) / 1000000 
      agg_hab <- aggregate(hab_aoi$habarea ~ OLU_ID, data=hab_aoi, FUN=sum, na.rm=TRUE)
    }
  )
  write.csv(agg_hab, paste0(f, "_area.csv"), row.names=F)
}







