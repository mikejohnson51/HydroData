#' Find Neareast USGS stations
#'
#' @param location a lat, long pair or Colloqiual Name
#' @param n the number of Airports to return
#'
#' @return A list contatining a data.frame of Airports and a minimum bounding box data.frame
#' @export
#' @author Mike Johnson

findNearestUSGS = function(location = NULL, n = 5){

  df = HydroData::usgsStations

  sp = sp::SpatialPointsDataFrame(coords = cbind(df$lon_reachCent, df$lat_reachCent), data = as.data.frame(df), proj4string = AOI::aoiProj)

  if(class(location) == 'numeric') { point = SpatialPoints(cbind(location[1], location[2]))
  } else {
    x = AOI::getPoint(location)
    point = sp::SpatialPoints(cbind(x$lon, x$lat), proj4string = AOI::aoiProj)
  }

  dist = spDistsN1(sp, point, longlat = T)

  ndx = cbind(df[(order(dist)[1:n]), ],  dist[(order(dist)[1:n])])
  names(ndx) = c("OBJECTID", "comid", "site_no", "name", "da_sqkm", "lat", "long", "dist_km")

  bb = getBoundingBox(ndx)

  return(list(data = ndx, extent = bb))
}
