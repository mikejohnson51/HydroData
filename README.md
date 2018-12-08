# HydroData: Earth Systems Data in R <img src="man/figures/logo.png" width=160 height = 120 align="right" />

[![travis](https://travis-ci.org/mikejohnson51/HydroData.svg?branch=master)](http://travis-ci.org/mikejohnson51/HydroData)  [![DOI](https://zenodo.org/badge/112221493.svg)](https://zenodo.org/badge/latestdoi/112221493)

**HydroData** is designed to help (1) find, (2) get, (3) visualize, and (4) format disparate earth systems data through a core language (R); a common geospatial reference (ESPG:4629) ; and unifying vocabulary built around querying data by an area of interest (AOI). The package supports access to 20+ National/Global data sources. 

All functions are designed to work with the [AOI](https://mikejohnson51.github.io/AOI/) package and magrittr pipe operation `%>%` allowing successive HydroData calls to be directly chained to an area of interest:

|**Number**|**Dataset**                 | **Description**                                                            | **Format**                       |
|----------|----------------------------| ---------------------------------------------------------------------------|----------------------------------|
|1         | **GHCN**                   | Global Historical Climate Network                                          | Vector point                     |
|2         | **International Airports** | OpenFlights Geo-database                                                   | Vector point                     |
|3         | **NID**                    | National Inventory of Dams (USACE)                                         | Vector point                     |
|4         | **NWIS**                   | National Water Information System Site Index                               | Vector point                     |
|5         | **Snotel**                 | The NRCS Snow Measurement Stations                                         | Vector point                     |
|6         | **GagesII**                | Geospatial Attributes of Gages for Evaluating Streamflow                   | Vector point, line               |
|7         | **NLDI**                   | The Network Linked Data Index                                              | Vector point, line, polygon      |
|8         | **TIGER**                  | 2017 US Census Bureau Road Network                                         | Vector line                      |
|9         | **NHDPlus**                | National Hydrography Dataset Waterbodies, River Network, Catchments        | Vector line, polygon             |
|10        | **EPA Basins**             | Environmental Protection Agency NWIS drainage basins                       | Vector polygon                   |
|11        | **SSURGO**                 | National Cooperative Soil Survey Geospatial Soil Data Base                 | Vector polygon                   |
|12        | **WBD**                    | The Watershed Boundary Dataset (all HUC levels)                            | Vector polygon                   |
|13        | **CDL**                    | USDA Crop Data Layers (2008 - 2017)                                        | Raster                           |
|14        | **NED**                    | National Elevation Dataset (10 and 30 meter resolutions)                   | Raster                           |
|15        | **NLCD**                   | National Land Cover Dataset (01,06,11) (Impervious, canopy, Land cover)    | Raster                           |

```r
myData = getAOI(clip = list("UCSB", 10, 10)) %>% 
findNHD() %>% 
findWaterbodies %>% 
findNWIS %>% 
findNED()
```
```
str(myData,max.level = 2)

List of 5
 $ AOI        :Formal class 'SpatialPolygons' [package "sp"] with 4 slots
 $ nhd        :Formal class 'SpatialLinesDataFrame' [package "sp"] with 4 slots
 $ waterbodies:Formal class 'SpatialPolygonsDataFrame' [package "sp"] with 5 slots
 $ nwis       :Formal class 'SpatialPointsDataFrame' [package "sp"] with 5 slots
 $ NED        :Formal class 'RasterLayer' [package "raster"] with 12 slots
 - attr(*, "class")= chr "HydroData"
```

For now, most internal HydroData operations are based of the 'simple feature (sf)' library however objects are all returned as Spatial* sp objects. As the community around simple features grows the default output of 'HydroData' might change. Until then users can use the `to_sf` function to convert a `HydroData` object to simple features where appropriate.

HydroData offer in-package tools for generating interactive visualizations of HydroData Spatial* objects

```r
myData.sf = myData %>% to_sf
```
```
str(myData.sf,max.level = 1)

List of 5
 $ NED        :Formal class 'RasterLayer' [package "raster"] with 12 slots
 $ AOI        :Classes ‘sf’ and 'data.frame':	1 obs. of  1 variable:
  ..- attr(*, "sf_column")= chr "geometry"
  ..- attr(*, "agr")= Factor w/ 3 levels "constant","aggregate",..: 
  .. ..- attr(*, "names")= chr(0) 
 $ nhd        :Classes ‘sf’ and 'data.frame':	100 obs. of  92 variables:
  ..- attr(*, "sf_column")= chr "geometry"
  ..- attr(*, "agr")= Factor w/ 3 levels "constant","aggregate",..: NA NA NA NA NA NA NA NA NA NA ...
  .. ..- attr(*, "names")= chr [1:91] "id" "ogc_fid" "comid" "fdate" ...
 $ waterbodies:Classes ‘sf’ and 'data.frame':	11 obs. of  24 variables:
  ..- attr(*, "sf_column")= chr "geometry"
  ..- attr(*, "agr")= Factor w/ 3 levels "constant","aggregate",..: NA NA NA NA NA NA NA NA NA NA ...
  .. ..- attr(*, "names")= chr [1:23] "id" "objectid" "comid" "fdate" ...
 $ nwis       :Classes ‘sf’ and 'data.frame':	3 obs. of  8 variables:
  ..- attr(*, "sf_column")= chr "geometry"
  ..- attr(*, "agr")= Factor w/ 3 levels "constant","aggregate",..: NA NA NA NA NA NA NA
  .. ..- attr(*, "names")= chr [1:7] "OBJECTID" "feature_id" "site_no" "site_name" ...
  
```
HydroData offer in-package tools for generating interactive visualizations of HydroData Spatial* objects

```r
 myData %>% explore()

```
<br>
<img src="man/figures/explore_ex.png" width=500 />
<br>

To download and get started with HydroData, install from GitHub using the `devtools` packages:

```r
library(devtools)
install_github("mikejohnson51/HydroData")
```

### Support:

HydroData is written by [Mike Johnson](https://mikejohnson51.github.io), a graduate Student at the [University of California, Santa Barbara](https://geog.ucsb.edu) in [Keith C. Clarke's](http://www.geog.ucsb.edu/~kclarke/) Lab, 2018 and is funded through the NOAA National Water Center (NWC) via the UCAR COMET Program (2017/18).


