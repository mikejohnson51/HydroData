#' @title Find Neareast GHCN stations
#' @description \code{findNearestGHCN} returns a \code{SpatialPointsDataFrame} of the 'n' number of GHCN station closest to a declared point.
#' @param point a point described by lat/long, can be piped from \link[AOI]{geocode}
#' @param n the number of stations to find (default = 5)
#' @param ids If TRUE, a vector of airport stations IDs is added to retuned list (default = \code{FALSE})
#' @param bb If TRUE, the geometry of the minimum bounding area of the features is added to returned list  (default = \code{FALSE})
#' @param parameters Can be used to filter the returned stations by measurement types
#' @export
#' @seealso findGHCN
#' @return a list() of minimum length 2: AOI and ghcn
#' @author Mike Johnson
#' @examples
#' \dontrun{
#' pt = geocode("UCSB") %>% findNearestGHCN(n = 5)
#' }

findNearestGHCN = function(point = NULL, n = 5, parameters = NULL, ids = FALSE, bb = FALSE){

  fin = list(loc = point)

  df = HydroData::ghcn_stations

  if(!is.null(parameters)){
  if((parameters %in% unique(df$PARAMETER))){
    df = df[which(df$PARAMETER == parameters), ]
   }else{
    cat(crayon::red((paste(parameters,  "is not a valid GHCN parameter"))))
   }
  }

  df = sf::st_as_sf(x = df,  coords = c("LON", "LAT"), crs = as.character(AOI::aoiProj))
  point = sf::st_as_sf(x = point,  coords = c("lon", "lat"), crs = as.character(AOI::aoiProj))
  dist = data.frame(ID = df$ID, Distance_km = sf::st_distance(x = df, y = point))
  dist = dist[order(dist$Distance_km)[1:n],]

  fin[["ghcn"]] = sf::as_Spatial(merge(df, dist, by = "ID"))

  if (bb) { fin[["AOI"]] = AOI::getBoundingBox(fin$closest_ghcn)}
  if (ids) { fin[["ID"]] = unique(fin$closest_ghcn$ID) }

  return(fin)
}

