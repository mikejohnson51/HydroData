#' Find Neareast COMIDs
#'
#' @param location a lat, long pair or Colloqiual Name
#'
#' @return A list contatining a data.frame of Airports and a minimum bounding box data.frame
#' @export
#' @author Mike Johnson

findNearestCOMID = function(point = NULL, n = 5, ids = FALSE, AOI = FALSE){

  h = 5
  w = 5
  lines = data.frame()
  pt = definePoint(point)

  while (dim(lines)[1] < n) {
    lines <-  suppressWarnings(query_cida(
      AOI = suppressMessages(AOI::getAOI(
        clip = list(pt$coords$lat, pt$coord$lon, h, w)
      )),
      type = 'nhdflowline_network',
      spatial = FALSE
    ))

    if (is.null(lines)) {
      lines = data.frame()
    }
    h = h + 2
    w = w + 2
  }

  dist = data.frame(comid = lines$comid,
                    Distance_km = sf::st_distance(x = lines, y = pt$geo))
  dist = dist[order(dist$Distance_km)[1:n], ]
  fin = merge(lines, dist, by = "comid")

  if (any(AOI, ids)) {
    fin = list(closest_comids = fin)
    if (AOI) {
      fin[["AOI"]] = AOI::getBoundingBox(fin$closest_comids)
    }
    if (ids) {
      fin[["comids"]] = unique(fin$closest_comids$comid)
    }
  }

  return(fin)

}

