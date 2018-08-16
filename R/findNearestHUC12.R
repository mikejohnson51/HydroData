#' @title Find Neareast HUC12 boundaries
#' @description \code{findNearestHUC12} returns a \code{SpatialPolygonDataFrame} of the 'n' number of HUC12 basins closest to a declared point.
#' @param point a point described by lat/long, can be piped from \link[AOI]{geocode}
#' @param n the number of basins to find (default = 5)
#' @param ids If TRUE, a vector of basin HUC IDs is added to retuned list (default = \code{FALSE})
#' @param bb If TRUE, the geometry of the minimum bounding area of the features is added to returned list (default = \code{FALSE})
#' @export
#' @seealso findWBD
#' @return a list() of minimum length 2: AOI and wbd
#' @author Mike Johnson
#' @examples
#' \dontrun{
#' pt = geocode("UCSB") %>% findNearestHUC12(n = 5)
#' }

findNearestHUC12 = function(point = NULL, n = 1, ids = FALSE, bb = FALSE){

  h = 10
  w = 10
  lines = data.frame()
  fin = list(loc = point)

  while(dim(lines)[1] < n){
    lines <-  suppressWarnings(query_cida(AOI = suppressMessages(
      AOI::getAOI(clip = list(point$lat, point$lon, h, w))),
      type = 'huc12',
      spatial = FALSE))

    if(is.null(lines)){lines = data.frame()}
    h = h + 2
    w = w + 2
  }

  point = sf::st_as_sf(x = point,  coords = c("lon", "lat"), crs = as.character(AOI::aoiProj))
  dist = data.frame(huc12 = lines$huc12, Distance_km = sf::st_distance(x = lines, y = point))
  dist = dist[order(dist$Distance_km)[1:n],]
  fin[["wbd"]] = merge(lines, dist, by = "huc12")


    if (bb) { fin[["AOI"]] = AOI::getBoundingBox(fin$wbd) }
    if (ids) { fin[["huc12"]] = unique(fin$wbd$huc12) }

  return(fin)

}



