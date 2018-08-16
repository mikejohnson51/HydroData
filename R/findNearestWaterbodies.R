#' @title Find Neareast NHD Waterbodies
#' @description \code{findNearestWaterbodies} returns a \code{SpatialPointsDataFrame} of the 'n' number of NHDwaterbodies closest to a declared point.
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
#' pt = geocode("UCSB") %>% findNearestWaterbodies(n = 5)
#' }

findNearestWaterbodies = function(point = NULL, n = 5, ids = FALSE, bb = FALSE){

  h = 10
  w = 10
  lines = data.frame()
  fin = list(loc = point)

  while(dim(lines)[1] < n){
    lines <-  suppressWarnings(query_cida(AOI = suppressMessages(
      AOI::getAOI(clip = list(point$lat, point$lon, h, w))),
      type = 'nhdwaterbody',
      spatial = FALSE))

    if(is.null(lines)){lines = data.frame()}
    h = h + 2
    w = w + 2
  }

  point = sf::st_as_sf(x = point,  coords = c("lon", "lat"), crs = as.character(AOI::aoiProj))
  dist = data.frame(objectid = lines$objectid, Distance_km = sf::st_distance(x = lines, y = point))
  dist = dist[order(dist$Distance_km)[1:n],]

  fin[["waterbodies"]] = sf::as_Spatial(merge(lines, dist, by = "objectid"))
  if (bb) { fin[["AOI"]] = AOI::getBoundingBox(fin$waterbodies) }
  if (ids) { fin[["objectid"]] = unique(fin$waterbodies$objectid) }


  return(fin)

}


