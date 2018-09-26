library(raster)

setwd('workspace')

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
#   * For each habitat layer:
#      * Create a new byte raster layer matching a source layer.
#      * Rasterize the habitat layer onto the source layer.
#         * Save the habitat raster to rasterized_habitats/<habitat_name>.tif
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
