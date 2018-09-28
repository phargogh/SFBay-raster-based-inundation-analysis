# Look through the Inundation rasters and create a mask.
mkdir -p data/jd_masked_slr
for file in `find data/Resampled30m_BCDC_Inundation_Rasters/ -name "*.tif"`
do
    python preprocess_inundation_rasters.py $file data/jd_masked_slr/$(basename $file)
done
