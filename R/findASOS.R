findASOS = function(AOI, ids = FALSE){

  ## index: ftp://ftp.ncdc.noaa.gov/pub/data/noaa/

  if(!(class(AOI) %in% c("list","HydroData"))){AOI = list(AOI = AOI)}

  x = readLines("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.txt")

  x = x[23:NROW(x)]

  mydata <- data.frame(USAF      = substr(x, 1,  6),
                       WBAN     = substr(x, 8, 12),
                       NAME     = substr(x, 13, 42),
                       CTRY    =  substr(x, 44, 45),
                       ST  = substr(x, 47, 48),
                       CALL    = substr(x, 51, 55),
                       LAT = as.numeric(gsub("+", "", substr(x, 57, 64))),
                       LON = as.numeric(gsub("+", "", substr(x, 66, 73))),
                       ELEV =  as.numeric(gsub("+", "", substr(x, 75, 81))),
                       Start   = as.Date(substr(x, 83, 90), format = "%Y%m%d"),
                       End   = as.Date(substr(x, 92, 99), format = "%Y%m%d"),
                       stringsAsFactors = FALSE
  )

   mydata = mydata[!is.na(mydata$LAT),]
   mydata = mydata[!is.na(mydata$LON),]

  sp = SpatialPointsDataFrame(coords = cbind(mydata$LON, mydata$LAT), data = mydata, proj4string = AOI$AOI@proj4string)
  sp = sp[AOI$AOI, ]

  if(!is.null(sp)){

    AOI[["ISDS"]] = sp

    report = paste(length(sp), "ACOS Stations")

    AOI = return.what(AOI, type = 'ISDS', report, vals = if(ids){"USAF"})
  }
  return(AOI)
}

