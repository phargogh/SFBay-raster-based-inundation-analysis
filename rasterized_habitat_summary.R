library(raster)
library(maptools)
library(sp)
library(rgdal)
library(gdalUtils)

# Inputs:
#   * All habitat layers are ESRI Shapefiles within a specific folder.
#   * All OLU/geo unit layers are ESRI Shapefiles within a specific folder
#   * All SLR raster layers are within a specific folder
# 
# Setup:
#   * rasterized_habitats/
#   * rasterized_geounits/
#
# Process:
#
# --> Preprocessing:
# --> Objective: all layers are rasters that perfectly overlap.
#   * Determine bounding box of target raster from union of geounits envelopes
#   * For each habitat layer:
#     * Create a new byte raster layer matching a source layer.
#     * Rasterize the habitat layer onto the source layer.
#       * Save the habitat raster to rasterized_habitats/<habitat_name>.tif
#   * For each geounit layer:
#     * Create a new byte raster layer matching a source layer.
#     * Rasterize the geounit layer onto the source layer.
#       * Save the OLU raster to rasterized_geounits/<geounit_name>.tif
#
# --> Determine overlap of habitats and geounits
#   * For each OLU raster:
#     * For each habitat raster:
#       * Determine overlap.
#       * Write overlap*pixelarea to output CSV
#
# --> Determine inundated habitat per geounit per SLR scenario
#   * For each geounit raster:
#     * For each SLR scenario raster:
#       * For each habitat raster:
#         * Determine inundated habitat
#         * Write overlap*pixelarea to output CSV

list_vectors <- function(directory){
  return(list.files(path=directory, pattern='.shp$', full.names=TRUE))
}

# For eyeballing the CRSs of the habitat vectors.
list_crs <- function(list_of_vectors) {
  for (vector_path in list_of_vectors){
    layer_name <- gsub('.shp', '', basename(vector_path))
    print(OGRSpatialRef(vector_path, layer_name))
  }
}

get_vector_bbox <- function(vector_path){
  layer_name <- gsub('.shp', '', basename(vector_path))
  
  # On average, It's much faster to do a system call to ogrinfo (especially in summary-only mode)
  # than it is to rely on rgdal::ogrInfo to read in all geometries when we only
  # need the bounding boxes for this calculation.
  extent_string <- ogrinfo(vector_path, layer_name, so=TRUE)[9]  # 9th row contains extents.
  vector_bbox = as.numeric(unlist(regmatches(extent_string, gregexpr('(-?[0-9.]+)', extent_string))))
  return(vector_bbox)
}

bbox_union_of_vectors <- function(list_of_vectors) {
  bbox_union <- NULL
  for (vector_path in list_of_vectors){
    print(vector_path)
    vector_bbox = get_vector_bbox(vector_path)

    if (is.null(bbox_union)) {
      bbox_union <- vector_bbox
    } else {
      bbox_union[1] <- min(bbox_union[1], vector_bbox[1])
      bbox_union[2] <- min(bbox_union[2], vector_bbox[2])
      bbox_union[3] <- max(bbox_union[3], vector_bbox[3])
      bbox_union[4] <- max(bbox_union[4], vector_bbox[4])
    }
  }
  return(bbox_union)
}

rasterize_vector <- function(vector_path, out_raster_path, bbox, pixel_size, all_touched=FALSE){
  gdal_rasterize(vector_path,
                 out_raster_path,
                 at=all_touched,
                 burn=1,  # burned pixels will have a value of 1
                 te=bbox,  # georeferenced extents
                 te=c((ceiling(bbox[3] - bbox[1]) / pixel_size),  # width of raster in pixels
                      (ceiling(bbox[4] - bbox[2]) / pixel_size)),  # height of raster in pixels
                 ot='Byte',  # store values as byte values
                 init=0  # Default band values to 0
  )
}
