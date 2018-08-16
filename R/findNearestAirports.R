#' @title Find the Airports nearest a point
#' @description \code{findNearestAirports} returns a \code{SpatialPointsDataFrame} of the 'n' number of airports closest to a declared point.
#' @param point a point described by lat/long, can be piped from \link[AOI]{geocode}
#' @param n the number of Airports to find (default = 5)
#' @param ids If TRUE, a vector of airport ICAO IDs is added to retuned list (default = \code{FALSE})
#' @param bb If TRUE, the geometry of the minimum bounding area of the features is added to returned list  (default = \code{FALSE})
#' @export
#' @seealso findAirports
#' @return a list() of minimum length 2: AOI and ap
#' @author Mike Johnson
#' @examples
#' \dontrun{
#' pt = geocode("UCSB") %>% findNearestAirports(n = 5)
#' }

findNearestAirports = function(point = NULL, n = 5, ids = FALSE, bb = FALSE){

  fin = list(loc = point)

  df = HydroData::ap
  df = sf::st_as_sf(x = df,  coords = c("lon", "lat"), crs = as.character(AOI::aoiProj))
  point = sf::st_as_sf(x = point,  coords = c("lon", "lat"), crs = as.character(AOI::aoiProj))
  dist = data.frame(ICAO = df$ICAO, Distance_km = sf::st_distance(x = df, y = point))
  dist = dist[order(dist$Distance_km)[1:n],]

  fin[['ap']] = sf::as_Spatial(merge(df, dist, by = "ICAO"))
  if (bb) { fin[["AOI"]] = AOI::getBoundingBox(fin$ap) }
  if (ids) { fin[["ICAO"]] = unique(fin$closest_ap$ICAO) }

  return(fin)
}

