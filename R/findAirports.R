#' Find Airports
#'
#' @description
#' \code{findAirports} returns a list of \code{Spatial*} Objects cropped to an Area of Interest.\cr\cr
#' To better understand defining an AOI using '\emph{state}', '\emph{county}' and '\emph{clip}' see \code{getAOI} and \code{getClipUnit}.\cr\cr
#' Returned \code{list} can be interactivly explored via \code{\link{explore}} and ID values (\code{ids = TRUE}) allow for Weather Underground data access via \code{getWU}.\cr\cr
#' All outputs are projected to \code{CRS '+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0+no_defs'}
#'
#' @param state    Full name(s) or two character abbriviation(s). Not case senstive
#' @param county    County name(s). Requires \code{state} input.
#' @param clip SpatialObject* or list. For details see \code{getClipUnit}
#' @param boundary  If TRUE, the AOI \code{SpatialPolygon(s)} will be joined to returned list
#'
#'  If a user wants greater control over basemap apperance replace TRUE with either:
#' \itemize{
#' \item't':  google terrain basemap
#' \item's':  google sattilite imagery basemap
#' \item'h':  google hybrid basemap
#' \item'r':  google roads basemap
#' }
#'
#' @param ids  If TRUE, returns a list of road names in AOI
#' @param save If TRUE, data is written to a HydroData folder in users working directory.
#'
#' @seealso  \code{\link{getAOI}}
#' @seealso  \code{\link{getWU}}
#' @seealso  \code{\link{explore}}
#'
#' @family HydroData 'find' functions
#'
#' @return
#' \code{findAirports} returns a named list of minimum length 1:
#'
#' \enumerate{
#' \item 'airports': A \code{SpatialPointsFrame*}\cr
#'
#' Pending parameterization, \code{findAirports} can also return:
#'
#' \item 'boundry':   A \code{SpatialPolygon*} of AOI if \code{boundary = TRUE}
#' \item 'fiat':      A \code{SpatialPolygon*} of intersected county boundaries if \code{boundary = TRUE}
#' \item 'ids':       A vector of 4 digit Airport Codes if \code{ids = TRUE}
#' }
#'
#' @examples
#'\dontrun{
#' # Find Airports in El Paso County, Colorado
#'
#' ap = findAirports(state = "CO", county = "El Paso", basemap = T, boundary = T)
#'
#' # Static Mapping
#'
#' plot(ap$boundary, add = T, lwd = 3)
#' plot(ap$airports, add = T, pch = 16, cex = 2)
#'
#' # Generate Interactive Map
#'
#' explore(ap)
#'}
#'
#' @export
#' @author
#' Mike Johnson

findAirports = function(state = NULL, county = NULL, clip = NULL, boundary = FALSE, ids = FALSE, save = FALSE){

  ap = HydroData::ap

  air = sp::SpatialPointsDataFrame(coords = cbind(ap$lon, ap$lat), data = as.data.frame(ap), proj4string = AOI::aoiProj)

  AOI = AOI::getAOI(state, county, clip)

  sp = air[AOI, ]

  if (length(sp) == 0) { stop("0 airports found in AOI") }

  message(formatC(as.numeric(length(sp)), format="d", big.mark=","), " airports found")

  items = list(name = AOI::nameAOI(state, county, clip),
               source = "Open Flights",
               airports =  sp)

  report = "Returned list includes: airport shapefile"

  items = return.what(sp, items, report, AOI, boundary, clip, ids = if(ids){ids = "ICAO"} )

  if(save){
    save.file(data      = items,
              state     = state,
              county    = county,
              clip = clip,
              agency    = 'NCAR',
              source    = "NCAR",
              dataset   = "airports",
              other     = NULL )
  }

  return(items)
}

