library(raster)
library(maptools)
library(sp)
library(rgdal)
library(gdalUtils)
library(tools)

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

# Get the bounding box (in projected coordinates) of an ESRI Shapefile.
#
# Returns a 4-item vector of (minx, miny, maxx, maxy)
get_vector_bbox <- function(vector_path){
  layer_name <- gsub('.shp', '', basename(vector_path))
  
  # On average, It's much faster to do a system call to ogrinfo (especially in summary-only mode)
  # than it is to rely on rgdal::ogrInfo to read in all geometries when we only
  # need the bounding boxes for this calculation.
  extent_string <- ogrinfo(vector_path, layer_name, so=TRUE)[9]  # 9th row contains extents.
  vector_bbox = as.numeric(unlist(regmatches(extent_string, gregexpr('(-?[0-9.]+)', extent_string))))
  return(vector_bbox)
}

# Take the union of the bboxes of a list of spatial files and return it.
#
# Files must be either GeoTiffs with the lowercase extension .tif or they
# must be ESRI Shapefiles with the lowercase extension .shp.
#
# Returns a 4-item vector of (minx, miny, maxx, maxy)
bbox_union <- function(list_of_spatial_files) {
  bbox_union <- NULL
  for (filepath in list_of_spatial_files){
    if (file_ext(filepath) == 'tif'){
      spatial_bbox = c(gdalinfo(filepath, raw_output=FALSE)$bbox)
    } else {
      # Assume it's an ESRI shapefile
      spatial_bbox = get_vector_bbox(filepath)
    }

    if (is.null(bbox_union)) {
      bbox_union <- spatial_bbox
    } else {
      bbox_union[1] <- min(bbox_union[1], spatial_bbox[1])
      bbox_union[2] <- min(bbox_union[2], spatial_bbox[2])
      bbox_union[3] <- max(bbox_union[3], spatial_bbox[3])
      bbox_union[4] <- max(bbox_union[4], spatial_bbox[4])
    }
  }
  return(bbox_union)
}

# Take an ESRI Shapefile and rasterize it.
#
# Parameters:
#  vector_path (string path) - The vector to rasterize.
#  dest_dir (string path) - The directory where the raster should be written.
#  bbox (vector) - A 4-item vector of (minx, miny, maxx, maxy), in projected coordinates.
#    The new raster will have this bounding box.
#  pixel_size (number) - The pixel size of the output raster.
#  all_touched=FALSE (boolean) - If TRUE, any pixel that overlaps the geometry will be burned in.
#    If FALSE, GDAL only includes pixels where the centerpoint is within the geometry.
#  fieldname='*' (string) - If provided, this parameter must be used in conjunction with the ``fieldvalue`` parameter
#    If provided, the field values will be selected from the field matching this name.
#  fieldvalue=NULL (string) - If fieldname is defined, only those geometries where ``fieldname``'s value matches this
#    ``fieldvalue`` will be rasterized.
rasterize_vector <- function(vector_path, dest_dir, bbox, pixel_size, all_touched=FALSE, fieldname='*', fieldvalue=NULL){
  if (fieldname == '*') {
    where_sql <- ""
    rasterized_name <- gsub('.shp', '.tif', basename(vector_path))
  } else {
    where_sql <- sprintf("%s='%s'", fieldname, fieldvalue)
    rasterized_name <- sprintf('%s.tif', fieldvalue)
  }
  
  sprintf('Rasterizing %s to %s', vector_path, file.path(dest_dir, rasterized_name))
  
  gdal_rasterize(vector_path,
                 file.path(dest_dir, rasterized_name),
                 at=all_touched,
                 burn=1,  # burned pixels will have a value of 1
                 te=bbox,  # georeferenced extents
                 co='COMPRESS=LZW',  # LZW compression is compatible with ArcGIS
                 ts=c(ceiling(bbox[3] - bbox[1]) / pixel_size,  # width of raster in pixels
                      ceiling(bbox[4] - bbox[2]) / pixel_size),  # height of raster in pixels
                 ot='Byte',  # store values as byte values to minimize required disk space.
                 init=0,  # Default band values to 0
                 where=where_sql,  # only rasterize the polygon we want, based on field value.
                 verbose=TRUE  # show gdal commands used
  )
}


# Take a vector and rasterize geometries by their fieldname.
#
# New rasters are created with names according to the values within the defined fieldname.
# So, if the vector dataset has three distinct field values ("Rock", "Sand", "Grass"), three
# new rasters will be created in the output directory, "Rock.tif", "Sand.tif", "Grass.tif".
# If multiple features have the same field value, those geometries will all be represented in the
# output raster.
#
# Parameters:
#    vector_path (string): The vector to rasterize
#    fieldname (string): The fieldname with values to group by.
#    out_dir (string): The directory the output rasters should be written to.
#    bbox (vector): 4-item vector of (minx, maxx, miny, maxy).
#    pixel_size (number): The pixel size of the outputs.
#    all_touched=FALSE (boolean): Whether to burn in any pixels that the geometries encounter.
rasterize_vector_by_fieldname <- function(vector_path, fieldname, out_dir, bbox, pixel_size, all_touched=FALSE){
  layer <- gsub('.shp', '', basename(vector_path))
  response <- ogrinfo(vector_path,
                      layer=layer,
                      q=TRUE,  # suppress most of the stuff we don't care about
                      sql=sprintf('SELECT DISTINCT %s FROM %s', fieldname, layer)  # just print the fields we want
  )
  for (line in response) {
    # Does the line printed contain a field value?
    if (length(grep(sprintf('^  %s ', fieldname), line)) > 0){
      field_value <- trimws(strsplit(line, '=')[[1]][2])
      rasterize_vector(vector_path,
                       dest_dir=out_dir,
                       bbox=bbox,
                       pixel_size=pixel_size,
                       all_touched=all_touched,
                       fieldname=fieldname,  
                       fieldvalue=field_value  # limit rasterization to just this polygon
      )
      
    }
  }
}


warp_raster_to_bbox <-function(raster_path, target_raster_path, bbox, pixel_size) {
  gdalwarp(raster_path,
           target_raster_path,
           te=bbox,  # The new raster extents
           overwrite=TRUE,  # overwrite the raster if it already exists
           multi=TRUE,  # enable multithreaded warping
           wo='INIT_DEST=0',  # initialize newly-exposed pixel values to 0
           r='near',  # use nearest-neighbor resampling if needed.
           verbose=TRUE,  # show the GDAL command being executed.
           ts=c(ceiling(bbox[3] - bbox[1]) / pixel_size,  # width of raster in pixels
                ceiling(bbox[4] - bbox[2]) / pixel_size)  # height of raster in pixels
  )
}

habitat_summary_analysis <- function(workspace, habitats_dir, geounits_dir, slr_dir, pixel_size, out_table) {
  dir.create(workspace)
  rasterized_habitats_dir <- file.path(workspace, 'rasterized_habitats')
  rasterized_geounits_dir <- file.path(workspace, 'rasterized_geounits')
  aligned_slr_dir <- file.path(workspace, 'aligned_slr_rasters')
  dir.create(rasterized_habitats_dir)
  dir.create(rasterized_geounits_dir)
  dir.create(aligned_slr_dir)
  
  # Get the union of the bboxes of the vectors in habitats_dir and geounits_dir
  # and the rasters in slr_dir.
  # This assumes that all inputs are in the same CRS and projection.
  print("Getting bounding box for analysis")
  raster_files <- list.files(path=slr_dir, pattern='.tif$', full.names=TRUE)
  analysis_bbox = bbox_union(
    append(list_vectors(habitats_dir),
           append(list_vectors(geounits_dir),
                  raster_files)))
  
  print('Expanding SLR rasters')
  for (raster_file in raster_files){
    target_raster_path <- file.path(aligned_slr_dir, basename(raster_file))
    warp_raster_to_bbox(raster_file, target_raster_path, analysis_bbox, pixel_size)
  }
  
  print("Rasterizing habitat vectors")
  for (habitats_vector in list_vectors(habitats_dir)){
    rasterize_vector(habitats_vector,
                     rasterized_habitats_dir,
                     analysis_bbox,
                     pixel_size,
                     all_touched=FALSE)
  }

  # These are all specificallly defined as individual geo. unit rasters are created for each
  # polygon in each geounit vector.  Maybe there's a nice way to loop through all of these?
  print("Rasterizing geounit vectors")
  rasterize_vector_by_fieldname(
    'data/jd_geounits/SMC_OLUs_complete_subtidal_albersconical.shp', 'NAME', rasterized_geounits_dir, analysis_bbox,
    pixel_size, all_touched=FALSE)

}

overlap_between_habitats_and_geounits <- function(habitat_rasters_dir, geounit_rasters_dir, out_csv){
  # create an empty dataframe that we can populate later.
  summary_df = data.frame()

  for (geounit_raster_path in list.files(path=geounit_rasters_dir, pattern='.tif$', full.names=TRUE)){
    print(sprintf('Calculating overlap for geounit %s', basename(geounit_raster_path)))
    column_names <- c('geounit')
    row_values <- c(basename(geounit_raster_path))
    
    geounit_raster <- raster(geounit_raster_path)
    cell_area <- xres(geounit_raster) * yres(geounit_raster)
    geounit_matrix <- as.matrix(geounit_raster)
    
    for (habitat_raster_path in list.files(path=habitat_rasters_dir, pattern='.tif$', full.names=TRUE)){
      habitat_matrix <- as.matrix(raster(habitat_raster_path))
      overlapping_area <- sum(geounit_matrix & habitat_matrix) * cell_area
      column_name = gsub('.tif', '', basename(habitat_raster_path))
      column_names <- append(column_names, c(column_name))
      row_values <- append(row_values, c(overlapping_area))
      print(sprintf('Overlapping area for habitat %s: %s', basename(habitat_raster_path), overlapping_area))
    }
    summary_df <- rbind(summary_df, as.data.frame(as.list(setNames(row_values, column_names))))
  }
  write.csv(summary_df, out_csv)
  
}

