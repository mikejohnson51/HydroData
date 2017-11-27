`HydroData`
================
A package for aggregating data needed for mapping and modeling hydrology. Paired with the NWM package makes accessing and applying NWM data easy. The central componenet of every function is a user defined area of interest (AOI). 

## AOI Definiton
This AOI can be defined in any one of 4 ways:

 1) A state or state, county description *ex: "CA"* or *"CA, Santa Barbara"*
 2) A user supplied shapefile (.shp) (*ex: la_metro.shp*)
 3) A area defined by a centroid and a bounding box height and width in miles:
      
      The centroid can be defined by:
        
        1. A coordinate pair **ex: c(34.41, 119.85, 10, 10)**
            * If interested in local area use get_ip_loc() to get local lat, lon
        
        2. A place name *ex: c('UCSB', 10, 10)*
        
## Current Supported functions
 1) NHD flowlines
 
 2) WBD boundaries
 
 3) NED elevation (1 arc and 1/3 arc)
  
 4) NLCD 2001, 2006, 2011
  
 5) USGS Stations
  
 6) NID Dams
  
 7) USGS Reservoirs
  
 8) Roads (Tiger, OSM)
 
 
 ## Functions in Development
 1) NOAA GCHN
 2) LANDSAT MODIS products
 3) SNOTEL
 4) USDA Cropscape
 5) NASS
 6) SSUGRO soils
 7) USGS County Water Use states
 
![USCB](https://www.ucsb.edu/graphic-identity/downloads/wave/ucsbwave-black.png)

## Installation

To install the  package, you must install from GitHub using the `devtools` packages:

```r
library(devtools)
install_github("mikejohnson51/rainfall_runoff")
```
