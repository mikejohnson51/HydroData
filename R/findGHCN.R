#' @title Find NOAA Maintained GHCN Stations
#' @description \code{findGHCN} returns a \code{SpatailPointsDataframe} of all
#' Global Historical Climatology Network (GHCN) stations within an AOI.
#' Data comes from the \href{https://www.ncdc.noaa.gov/data-access/land-based-station-data/land-based-datasets/global-historical-climatology-network-ghcn}{NOAA} and includes the following attributes:
#' \itemize{
#' \item 'ID'   : \code{character}  Station ID
#' \item 'NAME'   : \code{character}  Station Name
#' \item 'LAT': \code{numeric}  Latitude, decimal degrees
#' \item 'LON'   : \code{numeric} Longitude, decimal degrees
#' \item 'PARAMETER'   : \code{character}   Observed parameter
#' \item 'START_YEAR'    : \code{integer}   Year of first observation
#' \item 'END_YEAR'    : \code{integer}    Year of last observation
#' }
#' @param AOI  A Spatial* or simple features geometry, can be piped from \link[AOI]{getAOI}
#' @param param Returned results can be filtered by a requested parameter valid parameters can be found calling \code{unique(ghcn_stations$PARAMETER)}
#' @param ids  If TRUE,  a vector of station IDs is added to retuned list (default = \code{FALSE})
#' @return a list() of minimum length 2: AOI and ghcn
#' @examples
#' \dontrun{
#' # Get all stations and parameters
#' sta = getAOI(state = "CO", county = "El Paso") %>% findGHCN()
#'
#' #Limit call to precipiation gages
#' prcp.sta = getAOI(state = "CO", county = "El Paso") %>% findGHCN(param = 'PRCP')
#'
#' }
#' @author Mike Johnson
#' @export

findGHCN = function(AOI = NULL, param = NULL, ids = FALSE) {

  if(!(class(AOI) %in% c("list","HydroData"))){AOI = list(AOI = AOI)}
  if(any(class(AOI$AOI) == "sf")){ AOI$AOI = as_Spatial(AOI$AOI) }

  stations = HydroData::ghcn_stations

  if(!is.null(param)){
    if((param %in% unique(stations$PARAMETER))){
      stations = stations[which(stations$PARAMETER == param), ]
    }else{
      warning(paste(param,  "is not a valid GHCN parameter"))
    }
  }

  sp = sp::SpatialPointsDataFrame(cbind(stations$LON, stations$LAT), stations, proj4string = AOI$AOI@proj4string)
  sp = sp[AOI$AOI,]

  if(dim(sp)[1] == 0) { cat(crayon::red("0 GHCN stations found in AOI")) } else {

  AOI[["ghcn"]] = sp

  report = paste(length(unique(sp$ID)), "unique GHCN station(s)")

  AOI = return.what(AOI, type = 'ghcn', report, vals = if(ids){"ID"}else{NULL})

  }

  return(AOI)

}

