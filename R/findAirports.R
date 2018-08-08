#' @title Find Airport Data
#' @description \code{findAirports} returns a \code{SpatialPointsDataframe} of all airports within an AOI.
#' Data comes from the \href{https://openflights.org/data.html}{Openflights} database and includes the following attributes:
#' \itemize{
#' \item 'name'   : \code{character}  Name of airport. May or may not contain the City name.
#' \item 'city'   : \code{character}  Main city served by airport. May be spelled differently from Name.
#' \item 'country': \code{character}  Country or territory where airport is located.
#' \item 'IATA'   : \code{character}  3-letter IATA code
#' \item 'ICAO'   : \code{numeric}    4-letter ICAO code
#' \item 'lat'    : \code{numeric}    Latitude, decimal degrees
#' \item 'lon'    : \code{numeric}    Longitude, decimal degrees
#' }
#' @param AOI  A Spatial* or simple features geometry, can be piped from \link[AOI]{getAOI}
#' @param ids  If TRUE,  a vector of airport ICAO codes is added to retuned list (default = \code{FALSE})
#' @return a list() of minimum length 2: AOI and ap
#' @examples
#' \dontrun{
#' ap = getAOI(state = "CO", county = "El Paso") %>% findAirports()
#' }
#' @author Mike Johnson
#' @export

findAirports = function(AOI = NULL, ids = FALSE) {

  if(!(class(AOI) %in% c("list","HydroData"))){AOI = list(AOI = AOI)}
  if(any(class(AOI$AOI) == "sf")){ AOI$AOI = as_Spatial(AOI$AOI) }

  ap = HydroData::ap

  air = sp::SpatialPointsDataFrame(
    coords = cbind(ap$lon, ap$lat),
    data = as.data.frame(ap),
    proj4string = AOI::aoiProj
  )

  sp = air[AOI$AOI,]

  if (dim(sp)[1] == 0) {
    warning("0 airports found in AOI")
  } else {

  message(formatC(
    as.numeric(length(sp)),
    format = "d",
    big.mark = ","
  ),
  " airport(s) found")

  AOI[["ap"]] = sp

  report = "Returned list includes: airport shapefile"

  AOI = return.what(AOI, type = 'ap', report, vals = if(ids){"ICAO"})
  }

  return(AOI)

}


