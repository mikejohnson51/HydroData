# Find Stations for NOAA'S 1981-2010 CLIMATE NORMALS

# ID         is the station identification code.  Note that the first two
# characters denote the FIPS country code, the third character
# is a network code that identifies the station numbering system
# used, and the remaining eight characters contain the actual
# station ID.
# LATITUDE   is latitude of the station (in decimal degrees).
# LONGITUDE  is the longitude of the station (in decimal degrees).
# ELEVATION  is the elevation of the station (in meters, missing = -999.9).
# STATE      is the U.S. postal code for the state (for U.S. stations only).
# NAME       is the name of the station.
# GSNFLAG    is a flag that indicates whether the station is part of the GCOS
# Surface Network (GSN). The flag is assigned by cross-referencing
# the number in the WMOID field with the official list of GSN
# stations. There are two possible values:
#
#   Blank = non-GSN station or WMO Station number not available
# GSN   = GSN station
#
# HCNFLAG    is a flag that indicates whether the station is part of the U.S.
# Historical Climatology Network (HCN).  There are two possible
# values:
#
#   Blank = non-HCN station
# HCN   = HCN station
#
# WMOID      is the World Meteorological Organization (WMO) number for the
# station. If the station has no WMO number, then the field is blank.
# METHOD*    is an indication of whether a "traditional" or a "pseudonormals"
# approach was utilized for temperature or precipitation. This field
# in only found in prcp-inventory.txt and temp-inventory.txt

findNOAAnorms = function(AOI, ids = FALSE){

  ## index: ftp://ftp.ncdc.noaa.gov/pub/data/normals/1981-2010/

  if(!(class(AOI) %in% c("list","HydroData"))){AOI = list(AOI = AOI)}

  x = readLines("ftp://ftp.ncdc.noaa.gov/pub/data/normals/1981-2010/station-inventories/allstations.txt")

  mydata <- data.frame(ID      = substr(x, 1,  11),
                       LAT     = as.numeric(substr(x, 13, 20)),
                       LON     = as.numeric(substr(x, 22, 30)),
                       ELEV    = as.numeric(substr(x, 32, 37)),
                       ST.ABB  = substr(x, 39, 40),
                       NAME    = substr(x, 42, 71),
                       GSNFLAG = substr(x, 73, 75),
                       HCNFLAG = substr(x, 77, 79),
                       WMOID   = substr(x, 81, 85),
                       WMOID   = substr(x, 87, 99),
                       stringsAsFactors = FALSE
  )

sp = SpatialPointsDataFrame(coords = cbind(mydata$LON, mydata$LAT), data = mydata, proj4string = AOI$AOI@proj4string)
sp = sp[AOI$AOI, ]

if(!is.null(sp)){

  AOI[["nnorms"]] = sp

  report = paste(length(sp), "Normal Stations")

  AOI = return.what(AOI, type = 'nnorms', report, vals = if(ids){"ID"})
}
return(AOI)
}


