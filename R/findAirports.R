#' @title Find Airport Data
#' @description \code{findAirports} returns a \code{SpatialPointsDataframe} of all airports within an AOI.
#' Data comes from the \href{https://openflights.org/data.html}{Openflights} database and includes the following attributes:
#' \itemize{
#' \item 'name'   : \code{character}  Name of airport. May or may not contain the City name.
#' \item 'city'   : \code{character}  Main city served by airport. May be spelled differently from Name.
#' \item 'country': \code{character}  Country or territory where airport is located.
#' \item 'IATA'   : \code{character}  3-letter IATA code
#' \item 'ICAO'   : \code{numeric}    4-letter ICAO code
#' \item 'lat'    : \code{numeric}    Latitude of airport
#' \item 'lon'    : \code{numeric}    Longitude of airport
#' }
#' @param AOI  A Spatial* or simple features geometry, can be piped from \link[AOI]{getAOI}
#' @param ids  If TRUE,  a vector of airport ICAO codes is added to retuned list (default = \code{FALSE})
#' @return a list of minimum length 2: AOI and ap
#' @examples
#' \dontrun{
#' ap = getAOI(state = "CO", county = "El Paso") %>% findAirports()
#' }
#' @author Mike Johnson
#' @export

findAirports = function(AOI = NULL, ids = FALSE) {

  if(!(class(AOI) %in% c("list","HydroData"))){AOI = list(AOI = AOI)}
  if(any(class(AOI) == "sf")){ AOI = as_Spatial(AOI) }

  ap = HydroData::ap

  air = sp::SpatialPointsDataFrame(
    coords = cbind(ap$lon, ap$lat),
    data = as.data.frame(ap),
    proj4string = AOI::aoiProj
  )

  sp = air[AOI$AOI,]

  if (dim(sp)[1] == 0) { cat(crayon::red("0 airports found in AOI")) } else {

  AOI[["ap"]] = sp
  if(ids){ AOI[["ICAO"]] = sp$ICAO}

  report = paste(length(sp), "Airport(s)")

  AOI = return.what(AOI, type = 'ap', report, vals = if(ids){"ICAO"}else{NULL})
  }

  return(AOI)

}


