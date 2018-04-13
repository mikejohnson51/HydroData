# HydroData #

[![travis](https://travis-ci.org/mikejohnson51/HydroData.svg?branch=master)](https://travis-ci.org/mikejohnson51/HydroData)  
[![Coverage Status](https://coveralls.io/repos/github/mikejohnson51/HydroData/badge.svg?branch=master)](https://coveralls.io/github/mikejohnson51/HydroData?branch=master)  

**HydroData** is designed to help (1) find, (2) get, (3) visualize and (4) format disparate earth systems data through a core language (R); a common geospatial reference; and unifying vocabulary built around ‘finding’ and ‘getting’ data for an area of interest (AOI). The primary input needed from users is a defined AOI. Examples for doing this can be found [here](https://rawgit.com/mikejohnson51/HydroData/master/vignettes/DefiningAOIs.html) .

'Finding data' for an AOI refers to identifying the spatial feature classes and ID numbers needed to download data. 'Getting' data refers to the process of downloading tabular or raster data relating to an set of staion or AOI. The package supports access to 19 National/Global data sources. Most of which are highlighted in this [vignette](https://rawgit.com/mikejohnson51/HydroData/master/vignettes/HydroData_example.html):

## Data Sources

  - The [ USGS National Water Information System (NWIS)](https://nwis.waterdata.usgs.gov/nwis) stream gages
  - The [ USGS National Elevation Dataset (NED)](http://ned.usgs.gov) elevation data at 1 and 1/3 arc-second
  - The [ USGS National Hydrography Dataset (NHD)](http://nhd.usgs.gov) 
  - The [Global Historical Climatology Network
    (GHCN)](http://www.ncdc.noaa.gov/data-access/land-based-station-data/land-based-datasets/global-historical-climatology-network-ghcn), coordinated by National Climatic Data Center at NOAA
  - The [Daymet](https://daymet.ornl.gov/) gridded estimates of daily weather parameters for North America, version 3, available from the Oak Ridge National Laboratory’s Distributed Active Archive Center (DAAC)
  - The [National Land Cover Database (NLCD)](https://www.mrlc.gov/) from 2001, 2006, and 2011
  - The [SNOTEL station inventory]()
  - The [US Army Corps National Dam Inventory]()
  - The [USGS reservoir dataset]()
  - The [World waterbody dataset]()
  - The [watershed boundary dataset](https://nhd.usgs.gov/userGuide/Robohelpfiles/NHD_User_Guide/Feature_Catalog/Watershed_Boundary_Dataset/Watershed_Boundary_Dataset.htm) at all levels from 2 to 12
  - The [global airport dataset]()
  - The [TIGER road network dataset](https://www.census.gov/geo/maps-data/data/tiger.html) from the Census Bueuea
  - The [USDA Cropland Layer Dataset](https://www.nass.usda.gov/Research_and_Science/Cropland/SARS1a.php) from 1997:2017 when/where available
  - The [USGS Water Use Dataset](https://water.usgs.gov/watuse/) for 2005, 2010, and 2015 
  - The [PRISM monthly and Annual Normal Datasets](http://prism.oregonstate.edu) from Oregon State
  - [Weather Underground daily weather records](https://www.wunderground.com)
  - The [NOAA National Water Model](http://water.noaa.gov/about/nwm) Streamflow forcasts 
  - The [Koppen Climate Classifation Dataset](http://koeppen-geiger.vu-wien.ac.at/present.htm) 
  
To download, install from GitHub using the `devtools` packages:

```r
library(devtools)
install_github("USGS-R/dataRetrieval")
```

Please feel free to offer suggestions for improvement, ask for datasets to be included, or to ask general questions!

