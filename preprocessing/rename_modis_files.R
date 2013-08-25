'''
250 <- MOD02QKM | band_names = 1,2
500 <- MOD02HKM | band_names = 3,4,5,6,7
1km <- MOD021KM | RefSB band_names = 8,9,10,11,12,13lo,13hi,14lo,14hi,15,16,17,18,19,26
                | Emissive band_names = 20,21,22,23,24,25,27,28,29,30,31,32,33,34,35,36
'''


path.wd <- "/home/schmingo/Diplomarbeit/src/satellite/MOD02_2013-07-07_copy/"
setwd (path.wd)

band.names.250.refsb <- c("B01","B02")
band.names.500.refsb <- c("B03","B04","B05","B06","B07")
band.names.1km.refsb <- c("B08","B09","B10","B11","B12","B13.1","B13.2","B14.1","B14.2","B15","B16","B17","B18","B19","B26")
band.names.1km.emiss <- c("B20","B21","B22","B23","B24","B25","B27","B28","B29","B30","B31","B32","B33","B34","B35","B36")


files.list.sat <- list.files(path.wd, 
                             pattern = ".tif$", full.names = TRUE)

file.exists("band_EV_1KM_Emissive_b0.tif")

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

