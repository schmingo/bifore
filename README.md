BiFoRe scripts
=========
> Diplomarbeit

#### Fernerkundliche Vorhersage ausgewählter zoologischer Biodiversitätsparameter. 

<p> Der biologische Datensatz basiert auf Biodiversitätsdaten welche von Claudia Hemp am Kilimanjaro von 1996 - 2012 aufgenommen wurden. Die fernerkundliche Grundlage bildet der MODIS Satellit Aqua. </p>
<p> Zu jeder Observierung im Biodiversitätsdatensatz wird mittels MODIS Cloudmask (MYD35) eine zeitnahe wolkenfreie Szene (MYD02) selektiert. Über die Geokoordinaten werden aus allen 36 Bändern des MODIS Produkts die entsprechenden Grauwerte extrahiert und weiter prozessiert. Mit Hilfe von RandomForest wird anschließend versucht eine Korrelation zwischen den prozessierten Grauwerten und den gemessenen Biodiversitätsdaten nachzuweisen. </p>
<br />
<br />
## Preprocessing
<br />
* **Level 0050** Prepare biodiversity dataset
<p>
 - Replace 0-values with NA
 - Remove observations before MODIS launch date
 - Remove observations without coordinates
 - Remove species with less than 10 observations in different plots
 - Calculate number of species
 - Add LatLong coordinates
</p>
<br />
* **Level 0100** MODIS cloudcheck
<p>
 - Separate MYD35 day and night .hdf files
 - Convert MYD35 .hdf to .tif files using MRTswath
 - Check MYD35 .tif for each observation date for cloudiness
 - Create new .csv with cloud-free date and diff-days for each observation
</p>
<br />
* **Download MYD02 files**
<p></p><br />
* **Level 0200** Convert MYD02 .hdf to .tif
<p>
 - Convert .hdf to .tif 1000m resolution
 - Convert .hdf to .tif 500m resolution
 - Convert .hdf to .tif 250m resolution
 - Remove aggregated and uncertain bands
 - Rename .tif
</p>
<br />
* **Level 0300** Extract greyvalues for every single Observation
<p>
 - Replace invalid values from .tif with NA
 - Extract scalefactors and offset for each .tif from .hdf files
 - Calculate new greyvalues for each .tif ```(scalefactor-offset)*scalefactor```    
 - Extract new greyvalue for each observation
 - Calculate first derivate of each extracted greyvalue
 - Calculate standard deviation for 3x3 pixel raster (1000m)
 - Calculate standard deviation for 5x5 pixel raster (500m)
 - Calculate standard deviation for 11x11 pixel raster (250m)
 - Write new .csv including calculated greyvalues, first derivate and standard deviation 
</p>
<br />
## RandomForest
<br />


