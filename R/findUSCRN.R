

# Selected subsets of monthly, daily, hourly and sub-hourly (5-minute) USCRN/USRCRN data are available for easy access by users ranging from the general public to science experts.
# When using these data, please cite Diamond et. al.(2013) and for soil observations, also cite Bell et. al. (2013)


findUSCRN = function(AOI, ids = FALSE){

## index: ftp://ftp.ncdc.noaa.gov/pub/data/normals/1981-2010/

if(!(class(AOI) %in% c("list","HydroData"))){AOI = list(AOI = AOI)}

x = read.table("ftp://ftp.ncdc.noaa.gov/pub/data/uscrn/products/stations.tsv", sep = '\t',header = T,quote='', comment='')

sp = SpatialPointsDataFrame(coords = cbind(x$LONGITUDE, x$LATITUDE), data = x, proj4string = AOI$AOI@proj4string)
sp = sp[AOI$AOI, ]

if(!is.null(sp)){

  AOI[["uscrn"]] = sp

  report = paste(length(sp), "USCRN Stations")

  AOI = return.what(AOI, type = 'nnorms', report, vals = if(ids){"WBAN"})
}
return(AOI)
}
