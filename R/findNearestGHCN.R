#' Find Neareast GHCN
#'
#' @param location a lat, long pair
#' @param n the number of GHCN to return
#' @param PARAM the parameter of interst
#'
#' @return A list contatining a data.frame of Airports and a minimum bounding box data.frame
#' @export
#' @author Mike Johnson

findNearestGHCN = function(location = NULL, n = 5, PARAM = NULL){

  df = HydroData::ghcn_stations

  if(!is.null(PARAM)){
  if((PARAM %in% unique(df$PARAMETER))){
    df = df[which(df$PARAMETER == PARAM), ]
   }else{
    stop(paste(PARAM,  "is not a valid GHCN parameter"))
   }
  }

  sp = SpatialPointsDataFrame(coords = cbind(df$LON, df$LAT), df, proj4string = AOI::HydroDataProj)

  if(class(location) == 'numeric') { point = SpatialPoints(cbind(location[1], location[2]))
  } else {
    x = dismo::geocode(location)
    point = SpatialPoints(cbind(x$longitude, x$latitude), proj4string = AOI::HydroDataProj )
  }

  dist = spDistsN1(sp, point, longlat = T)

  ndx = cbind(df[(order(dist)[1:n]),],  dist[(order(dist)[1:n])])
  names(ndx) = c("ID", "NAME", "lat", "long", "PARAMETER", "START_YEAR",
                 "END_YEAR", "Distance_km")

  bb = getBoundingBox(ndx)

  return(list(data = ndx, extent = bb))
}
