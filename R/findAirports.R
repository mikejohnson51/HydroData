#' Find Airports
#'
#' @description
#' \code{findAirports} returns a list of \code{Spatial*} Objects cropped to an Area of Interest.\cr\cr
#' To better understand defining an AOI using '\emph{state}', '\emph{county}' and '\emph{clip}' see \code{getAOI} and \code{getClipUnit}.\cr\cr
#' Returned \code{list} can be interactivly explored via \code{\link{explore}} and ID values (\code{ids = TRUE}) allow for Weather Underground data access via \code{getWU}.\cr\cr
#' All outputs are projected to \code{CRS '+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0+no_defs'}
#'
#' @param boundary  If TRUE, the AOI \code{SpatialPolygon(s)} will be joined to returned list
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

findAirports = function(AOI = NULL, ids = FALSE) {

  if(!(class(AOI) %in% c("list","HydroData"))){AOI = list(AOI = AOI)}

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


