# HydroData #

Solarized dark             |  Solarized Ocean
:-------------------------:|:-------------------------:
[![travis](https://travis-ci.org/mikejohnson51/HydroData.svg?branch=master)](https://travis-ci.org/mikejohnson51/HydroData)  |  [![Coverage Status](https://coveralls.io/repos/github/mikejohnson51/HydroData/badge.svg?branch=master)](https://coveralls.io/github/mikejohnson51/HydroData?branch=master)  

[![travis](https://travis-ci.org/mikejohnson51/HydroData.svg?branch=master)](https://travis-ci.org/mikejohnson51/HydroData)  
[![Coverage Status](https://coveralls.io/repos/github/mikejohnson51/HydroData/badge.svg?branch=master)](https://coveralls.io/github/mikejohnson51/HydroData?branch=master)  

**HydroData** is designed to help (1) find, (2) get, (3) visualize and (4) format disparate earth systems data through a core language (R); a common geospatial reference; and unifying vocabulary built around ‘finding’ and ‘getting’ data for an area of interest (AOI). The primary input needed from users is a defined AOI. Full documentation for this package can be found [here](https://rawgit.com/mikejohnson51/HydroData/HydroData/) .

'Finding data' for an AOI refers to identifying the spatial feature classes and ID numbers needed to download data. 'Getting' data refers to the process of downloading tabular or raster data relating to an set of staion or AOI. The package supports access to 19 National/Global data sources. Most of which are highlighted in this [vignette](https://rawgit.com/mikejohnson51/HydroData/master/vignettes/HydroData_example.html):
  
To download, install from GitHub using the `devtools` packages:

```r
library(devtools)
install_github("mikejohnson51/HydroData")
```

Please feel free to offer suggestions for improvement, ask for datasets to be included, or to ask general questions!

