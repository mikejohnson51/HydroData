#' Find Neareast Airports
#'
#' @param location a lat, long pair
#' @param n the number of Airports to return
#' @export
#'
#' @return A list contatining a data.frame of Airports and a minimum bounding box data.frame
#' @author Mike Johnson

findNearestAirports = function(point = NULL, n = 5, ids = FALSE, AOI = FALSE){

  pt = definePoint(point)

  df = HydroData::ap
  df = sf::st_as_sf(x = df,  coords = c("lon", "lat"), crs = as.character(AOI::aoiProj))

  dist = data.frame(ICAO = df$ICAO, Distance_km = sf::st_distance(x = df, y = pt$geo))
  dist = dist[order(dist$Distance_km)[1:n],]
  fin = merge(df, dist, by = "ICAO")

  if (any(AOI, ids)) {
    fin = list(closest_ap = fin)
    if (AOI) {
      fin[["AOI"]] = AOI::getBoundingBox(fin$closest_ap)
    }
    if (ids) {
      fin[["ICAO"]] = unique(fin$closest_ap$ICAO)
    }
  }

  return(fin)
}

