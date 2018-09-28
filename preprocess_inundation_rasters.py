# encoding=UTF-8
"""preprocess_inundation_rasters.py

To execute this file:

$ python preprocess_inundation_rasters.py <sourcefile> <destfile>

"""

import sys

import numpy
from osgeo import gdal
import pygeoprocessing


def main(inundation_raster, target_raster):
    """Create boolean raster indicating which pixels are underwater.

    Parameters:
        inundation_raster (string): The path to an inundation raster with
            pixel values indicating the depth of inundation.
        target_raster (string): Where to write the output raster.

    Returns:
        ``None``

    """

    def mask_inundation(depth_under_water):
        """Create a boolean raster indicating whether rasters are underwater.

        Parameters:
            depth_under_water (ndarray): A float array with pixel values
                representing the depth of inundation at a pixel.  Large
                negative values are probably nodata.

        Returns:
            A numpy array with pixel values of 1 if a pixel is underwater,
            0 if above water.

        """
        out_matrix = numpy.empty(depth_under_water.shape, dtype=numpy.uint8)
        out_matrix[:] = 0
        out_matrix[depth_under_water > 0] = 1
        return out_matrix

    pygeoprocessing.raster_calculator(
        [(inundation_raster, 1)], mask_inundation, target_raster,
        gdal.GDT_Byte, nodata_target=255)


if __name__ == '__main__':
    main(*sys.argv[1:])
