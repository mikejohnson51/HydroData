#' Find Neareast Airports
#'
#' @param location a lat, long pair
#' @param n the number of Airports to return
#'
#' @return A list contatining a data.frame of Airports and a minimum bounding box data.frame
#' @export
#' @author Mike Johnson

findNearestAirports = function(location = NULL, n = 5){

  df = HydroData::ap

  sp = SpatialPointsDataFrame(coords = cbind(df$lon, df$lat), df, proj4string = AOI::HydroDataProj)

  sp::plot(sp)

  if(class(location) == 'numeric') { point = SpatialPoints(cbind(location[1], location[2]))
  } else {
    x = dismo::geocode(location)
  point = SpatialPoints(cbind(x$longitude, x$latitude), proj4string = AOI::HydroDataProj )
  }

  dist = spDistsN1(sp, point, longlat = T)

  ndx = cbind(df[(order(dist)[1:n]), ],  dist[(order(dist)[1:n])])
  names(ndx) = c("name",    "city",    "country", "IATA",    "ICAO",   "lat",     "long",     "elev",
                 "tz",      "tzname", "Distance_km")

  bb = getBoundingBox(ndx)

  return(list(data = ndx, extent = bb))
}


