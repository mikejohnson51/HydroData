#' @title Find USGS NWIS Stream Gages
#' @description \code{findNWIS} returns a \code{SpatialPointsDataFrame}
#' of all USGS NWIS gages for an Area of Interest. This dataset is accessed through the NWIS web portal and contains the following attributes:
#' \itemize{
#' \item 'OBJECTID'   : \code{character}  Unique ID in dataset
#' \item 'feature_id'   : \code{character}  NHD COMID for reach
#' \item 'site_no': \code{character}  NWIS ID number
#' \item 'site_name'   : \code{character}  Name of site
#' \item 'da_sqkm'   : \code{character}   Area drainign to gage in square kilometers
#' \item 'lat_reachCent'    : \code{numeric}    Latitude of the reach center, decimil degrees
#' \item 'lon_reachCent'    : \code{numeric}    Longitude of the reach center, decimil degrees
#' }
#' @param AOI A Spatial* or simple features geometry, can be piped from \link[AOI]{getAOI}
#' @param ids If TRUE, a vector of NIWS gage IDs are added to retuned list (default = \code{FALSE})
#' @param comids f TRUE, a vector of NHD COMIDs IDs are added to retuned list (default = \code{FALSE})
#' @return a list() of minimum length 2: AOI and nwis
#' @examples
#' \dontrun{
#' co = getAOI(state = "CO") %>% findNWIS()
#' }
#' @author Mike Johnson
#' @export

findNWIS = function(AOI = NULL, ids = FALSE, comids = FALSE){

  if(!(class(AOI) %in% c("list","HydroData"))){AOI = list(AOI = AOI)}

  usgsStations = HydroData::usgsStations
  sp = sf::st_as_sf(x = usgsStations, coords = c("lon_reachCent", "lat_reachCent"), crs = 4269) %>% sf::as_Spatial()

  sp = sp[AOI$AOI,]

  if (dim(sp)[1] == 0) { warning("0 stations found in AOI") } else {

  AOI[["nwis"]] = sp

  if(comids){ AOI[['nwis_comids']] = sp$feature_id}

  report = paste(length(sp), "USGS NWIS stations")

  AOI = return.what(AOI, type = 'nwis', report, vals = if(ids){"site_no"} else {NULL})
  }

  return(AOI)


}



