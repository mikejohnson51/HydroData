`HydroData`
================

**HydroData** HydroData is an R package designed to help find, get, and visualize climate, hydrology, and the landscape data in a single framework developed with a core language (R) and common geospatial reference, and unifying vocabulary built around ‘finding’ and ‘getting’ data for an area of interest. In this contexts finding data referes to identifying the spatail feature classes and identifers needed to download data, and getting data refers to the process of getting tabular or raster data relating to an AOI or found location. Currently the package support access to 18 National/Global data sources:

##Installation

To install the  package, you must install from GitHub using the `devtools` packages:

```r
library(devtools)
install_github("mikejohnson51/HydroData")
```

##Data Sources

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
  - The [PRISM monthly and Annual Normal Datasets]() from Oregon State
  - [Weather Underground daily weather records]()
  - The [NOAA National Water Model]() Streamflow forcasts _______
  - The [Koppen Climate Classifation Dataseet]() from _______

## Defining an AOI

The central componenet of every function in HydroData is a user defined area of interest (AOI) which can be defined in a number of ways using three core parameters: state, county, and clip_unit

(1) An AOI can be defined as a state name or abbriviation:

```r
 CA = getAOI(state = "California")
 TX = getAOI(state = "tx")
 
 plot(CA); plot(TX)
```

(2) An AOI can be defined by a state, county pair:

```r
 sb = getAOI(state = "California", county = "Santa Barbara")
 
 plot(sb)
```

(3) An AOI can be defined by a user supplied Spatial* or Raster* object:
 
```r
 la.met = getAOI(clip_unit = rgdal::readOGR("/LA_metro"))
 
 plot(la.met)
 
```
 
(4) An area defined by a (1)centroid, (2) a bounding box height and width, and (3) an optional bounding box origin:
      The centroid can be defined by:
      
```r
# Select a 100 sqmi AOI the the National Water Center at the center:

 nwc = getAOI(clip_unit = list("National Water Center", 10, 10))

# Select a 100 sqmi AOI with a known Lat, Lon pair at the center
 
 pt = getAOI(clip_unit = list(34.41, 119.85, 10, 10))

# Select a 100 sqmi AOI with the KMART near UCSB at the lower left corner
 
 goleta = getAOI(list("KMART near UCSB", 10, 10, "lowerleft"))
 
# Select a 100 sqmi AOI with UC Berkley at the Upper right corner
 
 berkley = getAOI(list("UC Berkley", 10, 10, "upperright"))
``` 

In all of the following functions state, county, and clip_unit are offered as parameters so that you can define your search for finding and getting data.

## Other Common Parameters

Much like state,county, and clip_unit are used to construct AOI defintions all HydroData functions share other commom parameters:

 - basemap:  When TRUE adds a basemap to a returned list
 - boundary: When TRUE adds a the spatial object defining the AOI to a returned list
 - save: When TRUE write the called data to a users disk in their working dircory in a HydroData folder
 
Other functions might have unique parameters such as year or resolution which will be covered in the examples however with these parameters the basics of all functions are known:

## Find Data Functions

#### USGS NWIS Gaging Stations

```r
# Find all USGS stations in El Paso County, Colorado

elpaso = findUSGS(state = 'CO', county = 'El Paso', basemap = T, boundary = T)

plot(elpaso$basemap)
plot(elpaso$vboundary, add = T, lwd = 5)
plot(elpaso$stations, add = T, col = 'darkgreen')

head(elpaso$stations)
```

#### GCHN Stations

```r
# Find all USGS stations in El Paso County, Colorado

elpaso = findUSGS(state = 'CO', county = 'El Paso', basemap = T, boundary = T)

plot(elpaso$basemap)
plot(elpaso$vboundary, add = T, lwd = 5)
plot(elpaso$stations, add = T, col = 'darkgreen')

head(elpaso$stations)
```















![USCB](https://www.ucsb.edu/graphic-identity/downloads/wave/ucsbwave-black.png)

## Installation

To install the  package, you must install from GitHub using the `devtools` packages:

```r
library(devtools)
install_github("mikejohnson51/HydroData")
```
