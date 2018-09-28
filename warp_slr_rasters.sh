#!/usr/bin/env sh

######################################################################
# @author      : jdouglass (jdouglass@DN0a230095.SUNet)
# @file        : warp_slr_rasters
# @created     : Friday Sep 28, 2018 12:02:58 PDT
#
# @description : 
######################################################################

mask_rasters_dir=data/jd_masked_slr
reprojected_dir=data/jd_slr/

mkdir -pf $reprojected_dir

for file in `find $mask_rasters_dir -name "*.tif"`
do
    gdalwarp \
        -co COMPRESS=LZW \
        -r near \
        -ot Byte \
        -overwrite \
        -t_srs "+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +datum=NAD27 +units=m +no_defs " \
        "$file" \
        "$reprojected_dir/$(basename $file)"
done
