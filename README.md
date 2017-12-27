`HydroData`
================

**HydroData** is a package designed to ease the burden of data collection and processing for hydrologic and earth systems analysis. It also focuses of helping researchers more quickly map and communicate the data they use.

Currently, 8 datasets are avaialble with the intention of adding at least 8 more. The central componenet of every function in this package is a user defined area of interest (AOI) which can be defined in a number of ways. With defined AOI parameters each function internally follows a three step work flow:

1. Create AOI shapefile
2. Define API call (if needed) and download data  
3. Clip to AOI boudaries 

Within each step the approapriate projection trasforms, and grid alignments are handled internally. 

## AOI Definiton
This AOI can be defined in one of 4 ways:

 1) A state **ex: 'CA'**
 2) A state, county description **ex: c(state = 'TX', county = 'Harris')**
 2) A user supplied shapefile (.shp) (**ex: rgdal::readOGR('la_metro.shp')**)
 3) An area defined by a centroid and a bounding box height and width:
      The centroid can be defined by:
        
       1. A coordinate pair **ex: c(34.41, 119.85, 10, 10)**
            
            If get local area use **get_ip_loc()** to import your (lat, lon)
        
       2. A place name **ex: list('KMART near UCSB', 10, 10)**
        
## Current Supported functions
 1)  NHD flowlines
 2)  WBD boundaries (at all levels)
 3)  NED elevation (1 arc second and 1/3 arc second resolution) 
 4)  NLCD (years 2001, 2006, 2011)
 5)  USGS Stream gages
 6)  NID Dams
 7)  USGS Reservoirs
 8)  Roads (Tiger)
 9)  NOAA GCHN
 10) USDA Cropscape
 11) USGS County Water Use stats
 12) National Water Model Streamflow
 13) CyberGIS HAND products
 14) USA county boundaries
 
## Functions in Development
 
 1) SNOTEL
 2) SSUGRO soil data
 3) PRISM climatology data

![USCB](https://www.ucsb.edu/graphic-identity/downloads/wave/ucsbwave-black.png)

## Installation

To install the  package, you must install from GitHub using the `devtools` packages:

```r
library(devtools)
install_github("mikejohnson51/HydroData")
```
