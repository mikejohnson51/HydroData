#' Find Neareast USGS stations
#'
#' @param location a lat, long pair or Colloqiual Name
#' @param n the number of Airports to return
#'
#' @return A list contatining a data.frame of Airports and a minimum bounding box data.frame
#' @export
#' @author Mike Johnson

findNearestNWIS = function(location = NULL, n = 5, ids = FALSE, AOI = FALSE){

  pt = definePoint(point)

  df = HydroData::usgsStations
  df = sf::st_as_sf(x = df,  coords = c("lon_reachCent", "lat_reachCent"), crs = as.character(AOI::aoiProj))

  dist = data.frame(site_no = df$site_no, Distance_km = sf::st_distance(x = df, y = pt$geo))
  dist = dist[order(dist$Distance_km)[1:n],]
  fin = merge(df, dist, by = "site_no")

  if (any(AOI, ids)) {
    fin = list(closest_nwis = fin)
    if (AOI) {
      fin[["AOI"]] = AOI::getBoundingBox(fin$closest_nwis)
    }
    if (ids) {
      fin[["site_no"]] = unique(fin$closest_nwis$site_no)
    }
  }

  return(fin)

}


