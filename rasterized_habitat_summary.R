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

rasterize_vector <- function(vector_path, dest_dir, bbox, pixel_size, all_touched=FALSE){
  rasterized_name <- gsub('.shp', '.tif', basename(vector_path))

  gdal_rasterize(vector_path,
                 file.path(dest_dir, rasterized_name),
                 at=all_touched,
                 burn=1,  # burned pixels will have a value of 1
                 te=bbox,  # georeferenced extents
                 co='COMPRESS=LZW',  # LZW compression is compatible with ArcGIS
                 ts=c(ceiling(bbox[3] - bbox[1]) / pixel_size,  # width of raster in pixels
                      ceiling(bbox[4] - bbox[2]) / pixel_size),  # height of raster in pixels
                 ot='Byte',  # store values as byte values to minimize required disk space.
                 init=0  # Default band values to 0
  )
}

habitat_summary_analysis <- function(workspace, habitats_dir, geounits_dir, pixel_size, out_table) {
  dir.create(workspace)
  rasterized_habitats_dir <- file.path(workspace, 'rasterized_habitats')
  rasterized_geounits_dir <- file.path(workspace, 'rasterized_geounits')
  dir.create(rasterized_habitats_dir)
  dir.create(rasterized_geounits_dir)
  
  # Get the union of the bboxes of the vectors in habitats_dir and geounits_dir
  print("Getting bounding box for analysis")
  analysis_bbox = bbox_union_of_vectors(
    append(list_vectors(habitats_dir), list_vectors(geounits_dir)))
  

  # There's probably a nice way to loop over these two sets of vectors,
  # but I haven't found it yet.
  for (habitats_vector in list_vectors(habitats_dir)){
    rasterize_vector(habitats_vector,
                     rasterized_habitats_dir,
                     analysis_bbox,
                     pixel_size,
                     all_touched=FALSE)
  }
  
  for (geounits_vector in list_vectors(geounits_dir)) {
    rasterize_vector(habitats_vector,
                     rasterized_geounits_dir,
                     analysis_bbox,
                     pixel_size,
                     all_touched=FALSE)
  }
}

overlap_between_habitats_and_geounits <- function(habitat_rasters_dir, geounit_rasters_dir, out_csv){
  column_names <- c("Geounit")
  
  # iterate over the habitats to add column names to the list
  for (habitat_raster_filename in list.files(path=habitat_rasters_dir, pattern='.tif$')){
    column_names <- c(column_names, c(gsub('.tif', '', habitat_raster_filename)))
  }
  
  # create an empty dataframe with correct columns.
  summary_df = data.frame(ncol=length(column_names))
  colnames(summary_df) <- column_names
  
  n_geounit <- 1
  for (geounit_raster_path in list.files(path=geounit_rasters_dir, pattern='.tif$', full.names=TRUE)){
    geounit_raster <- raster(geounit_raster_path)
    cell_area <- xres(habitat_raster) * yres(habitat_raster)
    geounit_matrix <- as.matrix(geounit_raster)
    
    for (habitat_raster_path in list.files(path=habitat_rasters_dir, pattern='.tif$', full.names=TRUE)){
      habitat_matrix <- as.matrix(raster(habitat_raster_path))
      overlapping_area <- sum(geounit_matrix & habitat_matrix) * cell_area
      column_name = gsub('.tif', '', basename(habitat_raster_path))
      summary_df[n_geounit, column_name] <- overlapping_area
    }
    n_geounit <- n_geounit + 1
  }
  write.csv(summary_df, out_csv)
  
}
