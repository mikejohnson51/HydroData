#' Find Neareast COMIDs
#'
#' @param location a lat, long pair or Colloqiual Name
#'
#' @return A list contatining a data.frame of Airports and a minimum bounding box data.frame
#' @export
#' @author Mike Johnson

findNearestHUC12 = function(location = NULL, n = 1, ids = TRUE){

  if(class(location) == 'numeric') { point = SpatialPoints(cbind(location[1], location[2]))
  } else {
    x = AOI::getPoint(location)
    point = sp::SpatialPoints(cbind(x$lon, x$lat), proj4string = AOI::aoiProj)
  }

  point = sf::st_as_sf(point)

  test = query_cida(AOI = suppressMessages(AOI::getAOI(clip = list(location, 5,5))), type = 'huc12', spatial = FALSE)

  dist = data.frame(ID = test$huc12, DIST = sf::st_distance(x = test, y = point))

  dist = dist[order(dist$DIST),]
  dist = dist[c(1:n),]

  test1 = test[test$huc12 %in% dist$ID,]

  bb = AOI::getBoundingBox(test1)

  if(ids){ items = test1$huc12} else {items = list(COMIDs = test1$huc12, extent = bb)}

  return(items)

}



