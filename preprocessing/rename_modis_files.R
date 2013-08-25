################################################################################
## BiFoRe Scripts                                                             ##
##                                                                            ##
## RENAME MODIS FILES                                                         ##
##                                                                            ##
##                                                                            ##
## Author: Simon Schlauss (sschlauss@gmail.com)                               ##
## Version: 2013-08-25                                                        ##
##                                                                            ##
################################################################################


rename_modis_files <- function(path.modis)
{
  file.rename("band_EV_1KM_Emissive_b0.tif", "B20.tif")
  file.rename("band_EV_1KM_Emissive_b1.tif", "B21.tif")
  file.rename("band_EV_1KM_Emissive_b2.tif", "B22.tif")
  file.rename("band_EV_1KM_Emissive_b3.tif", "B23.tif")
  file.rename("band_EV_1KM_Emissive_b4.tif", "B24.tif")
  file.rename("band_EV_1KM_Emissive_b5.tif", "B25.tif")
  file.rename("band_EV_1KM_Emissive_b6.tif", "B27.tif")
  file.rename("band_EV_1KM_Emissive_b7.tif", "B28.tif")
  file.rename("band_EV_1KM_Emissive_b8.tif", "B29.tif")
  file.rename("band_EV_1KM_Emissive_b9.tif", "B30.tif")
  file.rename("band_EV_1KM_Emissive_b10.tif", "B31.tif")
  file.rename("band_EV_1KM_Emissive_b11.tif", "B32.tif")
  file.rename("band_EV_1KM_Emissive_b12.tif", "B33.tif")
  file.rename("band_EV_1KM_Emissive_b13.tif", "B34.tif")
  file.rename("band_EV_1KM_Emissive_b14.tif", "B35.tif")
  file.rename("band_EV_1KM_Emissive_b15.tif", "B36.tif")
  file.rename("band_EV_1KM_RefSB_b0.tif", "B08")
  file.rename("band_EV_1KM_RefSB_b1.tif", "B09")
  file.rename("band_EV_1KM_RefSB_b2.tif", "B10")
  file.rename("band_EV_1KM_RefSB_b3.tif", "B11")
  file.rename("band_EV_1KM_RefSB_b4.tif", "B12")
  file.rename("band_EV_1KM_RefSB_b5.tif", "B13.1")
  file.rename("band_EV_1KM_RefSB_b6.tif", "B13.2")
  file.rename("band_EV_1KM_RefSB_b7.tif", "B14.1")
  file.rename("band_EV_1KM_RefSB_b8.tif", "B14.2")
  file.rename("band_EV_1KM_RefSB_b9.tif", "B15")
  file.rename("band_EV_1KM_RefSB_b10.tif", "B16")
  file.rename("band_EV_1KM_RefSB_b11.tif", "B17")
  file.rename("band_EV_1KM_RefSB_b12.tif", "B18")
  file.rename("band_EV_1KM_RefSB_b13.tif", "B19")
  file.rename("band_EV_1KM_RefSB_b14.tif", "B26")
  file.rename("band_EV_250_RefSB_b0.tif", "B01")
  file.rename("band_EV_250_RefSB_b1.tif", "B02")
  file.rename("band_EV_500_RefSB_b0.tif", "B03")
  file.rename("band_EV_500_RefSB_b1.tif", "B04")
  file.rename("band_EV_500_RefSB_b2.tif", "B05")
  file.rename("band_EV_500_RefSB_b3.tif", "B06")
  file.rename("band_EV_500_RefSB_b4.tif", "B07")
}