#' Find Neareast COMIDs
#'
#' @param location a lat, long pair or Colloqiual Name
#'
#' @return A list contatining a data.frame of Airports and a minimum bounding box data.frame
#' @export
#' @author Mike Johnson

findNearestHUC12 = function(point = NULL, n = 1, ids = FALSE, AOI = FALSE){

  h = 10
  w = 10
  lines = data.frame()
  pt = definePoint(point)

  while(dim(lines)[1] < n){
    lines <-  suppressWarnings(query_cida(AOI = suppressMessages(
      AOI::getAOI(clip = list(pt$coords$lat, pt$coord$lon, h, w))),
      type = 'huc12',
      spatial = FALSE))

    if(is.null(lines)){lines = data.frame()}
    h = h + 2
    w = w + 2
  }

  dist = data.frame(huc12 = lines$huc12, Distance_km = sf::st_distance(x = lines, y = pt$geo))
  dist = dist[order(dist$Distance_km)[1:n],]
  fin = merge(lines, dist, by = "huc12")

  if (any(AOI, ids)) {
    fin = list(closest_huc12 = fin)
    if (AOI) {
      fin[["AOI"]] = AOI::getBoundingBox(fin$closest_huc12)
    }
    if (ids) {
      fin[["huc12"]] = unique(fin$closest_huc12$huc12)
    }
  }

  return(fin)

}



