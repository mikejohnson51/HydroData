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

  sp = sp::SpatialPointsDataFrame(coords = cbind(df$lon, df$lat), data = as.data.frame(df), proj4string = AOI::aoiProj)

  if(class(location) == 'numeric') { point = SpatialPoints(cbind(location[1], location[2]))
  } else {
   x = AOI::getPoint(location)
   point = sp::SpatialPoints(cbind(x$lon, x$lat), proj4string = AOI::aoiProj)
  }

  dist = spDistsN1(sp, point, longlat = T)

  ndx = cbind(df[(order(dist)[1:n]), ],  dist[(order(dist)[1:n])])
  names(ndx) = c("name",    "city",    "country", "IATA",    "ICAO",   "lat",     "long",     "elev",
                 "tz",      "tzname", "Distance_km")

  bb = getBoundingBox(ndx)

  return(list(data = ndx, extent = bb))
}


