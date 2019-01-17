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

findNearestNWIS = function(point = NULL, n = 5, ids = FALSE, bb = FALSE, ...) {

  h = 5
  w = 5
  sta = data.frame()
  fin = list(loc = point)

  while (dim(sta)[1] < n) {
    sta <-  suppressWarnings(
      findNWIS(
        AOI = AOI::getAOI( clip = list(point$lat, point$lon, h, w )), ... )
    )

    sta = sta[['nwis']]

    if (is.null(sta)) {
      sta = data.frame()
    }

    h = h + 2
    w = w + 2
  }

  point = sf::st_as_sf(x = point,  coords = c("lon", "lat"), crs = as.character(AOI::aoiProj))

  sta = sta %>% sf::st_as_sf( crs = sf::st_crs(point))

  dist = data.frame(site_no = sta$site_no, Distance_km = sf::st_distance(x = sta, y = point))
  dist = dist[order(dist$Distance_km)[1:n], ]

  fin[["nwis"]] = as_Spatial(merge(sta, dist, by = "site_no"))
  if (bb & dim(fin$nwis)[1] > 1) { fin[["AOI"]] = AOI::getBoundingBox(fin$nwis) }
  if (ids) { fin[["site_no"]] = unique(fin$nwis$site_no) }

  return(fin)
}



