#' Find Neareast COMIDs
#'
#' @param location a lat, long pair
#' @param n the number of COMIDs to return
#'
#' @return A list contatining a data.frame of COMIDs and a minimum bounding box data.frame
#' @export
#' @author Mike Johnson

findNearestCOMID = function(location = NULL, n = 5){

  df = HydroData::nhd_outlet

  sp = SpatialPointsDataFrame(coords = cbind(df$lat, df$long), df, proj4string = CRS("+init=epsg:4326"))

  if(class(location) == 'numeric') { point = SpatialPoints(cbind(location[1], location[2]))
  } else if (class(location) == 'data.frame') {
    point = SpatialPoints(cbind(location$lon, location$lat), CRS("+init=epsg:4326"))
  } else { x = dismo::geocode(location)
    point = SpatialPoints(cbind(x$longitude, x$latitude), proj4string = CRS("+init=epsg:4326") )
  }

  dist = spDistsN1(sp, point, longlat = T)

  ndx = cbind(df[(order(dist)[1:n]), ],  dist[(order(dist)[1:n])])
  names(ndx) = c("COMID", "lat", "long", "Distance_km")

  bb = getBoundingBox(ndx)

  return(list(data = ndx, extent = bb))
}

?data


