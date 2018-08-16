#' @title Find Neareast NWIS stations
#' @description \code{findNearestNWIS} returns a \code{SpatialPointsDataFrame} of the 'n' number of NWIS gages closest to a declared point.
#' @param point a point described by lat/long, can be piped from \link[AOI]{geocode}
#' @param n the number of NWIS gages to find (default = 5)
#' @param ids If TRUE, a vector of station IDs is added to retuned list (default = \code{FALSE})
#' @param bb If TRUE, the geometry of the minimum bounding area of the features is added to returned list  (default = \code{FALSE})
#' @export
#' @seealso findNWIS
#' @return a list() of minimum length 2: AOI and nwis
#' @author Mike Johnson
#' @examples
#' \dontrun{
#' pt = geocode("UCSB") %>% findNearestNWIS(n = 5)
#' }

findNearestNWIS = function(point = NULL, n = 5, ids = FALSE, bb = FALSE){

  fin = list(loc = point)

  df = HydroData::usgsStations
  df = sf::st_as_sf(x = df,  coords = c("lon_reachCent", "lat_reachCent"), crs = as.character(AOI::aoiProj))
  point = sf::st_as_sf(x = point,  coords = c("lon", "lat"), crs = as.character(AOI::aoiProj))
  dist = data.frame(site_no = df$site_no, Distance_km = sf::st_distance(x = df, y = point))
  dist = dist[order(dist$Distance_km)[1:n],]

  fin[["nwis"]] = as_Spatial(merge(df, dist, by = "site_no"))
  if (bb)  { fin[["AOI"]] = AOI::getBoundingBox(fin$closest_nwis)}
  if (ids) { fin[["site_no"]] = unique(fin$closest_nwis$site_no) }


  return(fin)

}


