#' @title Find USDA NRCS Snotel Stations
#'
#' @description \code{findSnotel} returns a \code{SpatialPointsDataFrame*} Objects cropped to an Area of Interest.
#' Station data is extacted from \href{https://wcc.sc.egov.usda.gov/nwcc/yearcount?network=sntl&counttype=statelist&state=}{NRCS reports} and contains 11 attributes:
#'\itemize{
#' \item 'NETWORK'   : \code{integer}  Description of Network (all = 'SNTL')
#' \item 'STATE'   : \code{character}  The state the station is located in
#' \item 'NAME': \code{POSITct}  Unique station name
#' \item 'ID'   : \code{character}    Unique identifier assigned by NRCS
#' \item 'START.DATE'   : \code{character}    Date the station made first measurment ("Year-Month")
#' \item 'LAT'    : \code{numeric}    Latitude of station, decimil degrees
#' \item 'LON'    : \code{numeric}    Longitude of station, decimil degrees
#' \item 'ELEV'   : \code{character}     Elevation of station
#' \item 'COUNTY'   : \code{integer}  The county the station is located in
#' \item 'HUC12.NAME': \code{character}  The HUC12 name the station is located in
#' \item 'HUC12.ID': \code{character}  The HUC12 ID the station is located in
#' }\cr
#' @param AOI  A Spatial* or simple features geometry, can be piped from \link[AOI]{getAOI}
#' @param ids  If TRUE,  a vector of NHD COMIDs is added to retuned list (default = \code{FALSE})
#' @examples
#' \dontrun{
#' CA.sno = getAOI(state = 'CA') %>% findSnotel()
#'}
#' @export
#' @author
#' Mike Johnson

findSnotel = function(AOI = NULL, ids = FALSE){

  if(!(class(AOI) %in% c("list","HydroData"))){AOI = list(AOI = AOI)}

  sp =  sf::st_as_sf(x = HydroData::snotel, coords = c('LONG', 'LAT'), crs = 4269 ) %>% sf::as_Spatial()

  sp = sp[AOI$AOI,]

  if (dim(sp)[1] == 0) { cat(crayon::red("0 stations found in AOI")) } else {

  AOI[["snotel"]] = sp

  report = paste(length(sp), "snotel stations")
  AOI = return.what(AOI, type = 'snotel', report, vals = if(ids){"ID"})

  }

  return(AOI)

}

