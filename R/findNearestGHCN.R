#' Find Neareast GHCN stations
#'
#' @param location a lat, long pair
#' @param n the number of GHCN to return
#' @param PARAM the parameter of interst
#'
#' @return A list contatining a data.frame of Airports and a minimum bounding box data.frame
#' @export
#' @author Mike Johnson

findNearestGHCN = function(point = NULL, n = 5, parameters = NULL, ids = FALSE, AOI = FALSE){

  pt = definePoint(point)

  df = HydroData::ghcn_stations

  if(!is.null(parameters)){
  if((parameters %in% unique(df$PARAMETER))){
    df = df[which(df$PARAMETER == parameters), ]
   }else{
    warning(paste(parameters,  "is not a valid GHCN parameter"))
   }
  }

  df = sf::st_as_sf(x = df,  coords = c("LON", "LAT"), crs = as.character(AOI::aoiProj))

  dist = data.frame(ID = df$ID, Distance_km = sf::st_distance(x = df, y = pt$geo))
  dist = dist[order(dist$Distance_km)[1:n],]
  fin = merge(df, dist, by = "ID")

  if (any(AOI, ids)) {
    fin = list(closest_ghcn = fin)
    if (AOI) {
      fin[["AOI"]] = AOI::getBoundingBox(fin$closest_ghcn)
    }
    if (ids) {
      fin[["ID"]] = unique(fin$closest_ghcn$ID)
    }
  }

  return(fin)
}

