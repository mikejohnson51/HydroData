#' Find USGS NWIS Stream Gages
#'
#' @description
#' \code{findUSGS} returns a list of \code{Spatial*} Objects cropped to an Area of Interest.\cr\cr
#' To better understand defining an AOI using '\emph{state}', '\emph{county}' and '\emph{clip_unit}' see \code{getAOI} and \code{getClipUnit}.\cr\cr
#' Returned \code{list} can be interactivly explored via \code{\link{explore}} \cr
#' Returned ID values (\code{ids = TRUE}) allow for observed data access via \code{getUSGS} or \link[dataRetrieval]{readNWISdv} .\cr
#' Returned COMID values (\code{comids = TRUE}) allow for National Water Model access via \code{getNWM}.\cr\cr
#'
#' All outputs are projected to \code{CRS '+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0+no_defs'} and station data was extracted from the \href{https://maps.waterdata.usgs.gov/mapper/index.html}{USGS NWIS Mapper}.
#'
#' @param state    Full name(s) or two character abbriviation(s). Not case senstive
#' @param county    County name(s). Requires \code{state} input.
#' @param clip_unit SpatialObject* or list. For details see \code{getClipUnit}
#' @param boundary  If TRUE, the AOI \code{SpatialPolygon(s)} will be joined to returned list
#' @param basemap   If TRUE, a basemap will be joined to returned list
#'
#'  If a user wants greater control over basemap apperance replace TRUE with either:
#' \itemize{
#' \item't':  google terrain basemap
#' \item's':  google sattilite imagery basemap
#' \item'h':  google hybrid basemap
#' \item'r':  google roads basemap
#' }
#'
#' @param ids     If TRUE, returns a list of station IDs in AOI
#' @param comids  If TRUE, returns a list of NHD COMIDs that correspond to the station IDs
#' @param save    If TRUE, data is written to a HydroData folder in users working directory.
#'
#' @seealso  \code{\link{getAOI}}
#' @seealso  \code{\link{explore}}
#' @seealso  \code{\link{getUSGS}}
#' @seealso  \code{\link[dataRetrieval]{readNWISdv}}
#'
#' @family HydroData 'find' functions
#'
#' @return
#' \code{findUSGS} returns a named list of minimum length 1:
#'
#' \enumerate{
#' \item 'nwis':     A \code{SpatialPointsDataFrame} of stations and metadata
#' Pending parameterization, \code{findRoads} can also return:
#'
#' \item 'basemap':   A \code{RasterLayer*} basemap if \code{basemap = TRUE}
#' \item 'boundry':   A \code{SpatialPolygon*} of AOI if \code{boundary = TRUE}
#' \item 'fiat':      A \code{SpatialPolygon*} of intersected county boundaries if \code{boundary = TRUE}
#' \item 'ids':       A vector of road names if \code{ids = TRUE}
#' }
#'
#' @examples
#' \dontrun{
#' # Find stations within 10 square miles of the National Water Center:
#'
#'  nwc.usgs <- findUSGS(clip_unit = list("National Water Center", 10,  10),
#'                              boundary = TRUE, basemap = TRUE, ids = TRUE)
#'
#' # Static Mapping
#'
#'  plot(nwc.usgs$basemap)
#'  plot(nwc.usgs$boundary, add = TRUE, lwd = 5)
#'  plot(nwc.usgs$usgs, add = TRUE, lwd = 2, pch = 16,  col = "darkgreen")
#'
#' # Generate Interactive Map
#'
#' explore(nwc.usgs)
#'
#' # Get discharge data for all stations usign dataRetrival Package
#'
#'  data = getUSGS(IDs = nwc.usgs$ids)
#'
#' # Generate Interactive Timseries
#'
#' inspect(data, param = 'Flow')
#' }
#'
#' @export
#' @author
#' Mike Johnson
#'

findUSGS = function(state = NULL, county = NULL, clip_unit = NULL, boundary = FALSE, basemap = FALSE, ids = FALSE, save = FALSE, comids = FALSE){

  AOI  = getAOI(state, county, clip_unit)

  usgsStations = HydroData::usgsStations

  sp = SpatialPointsDataFrame(cbind(usgsStations$lon_reachCent, usgsStations$lat_reachCent), usgsStations, proj4string = HydroDataProj)

  sp = sp[AOI,]

  if (length(sp) == 0) { stop("0 stations found in AOI") }

  message(formatC(as.numeric(length(sp)), format="d", big.mark=","), " USGS gages found within AOI")

  items = list(name = nameAOI(state, county, clip_unit),
               source = "USGS NWIS",
               proj = HydroDataProj,
               nwis = sp)

  report = "Returned list includes: USGS NWIS shapefile"

  items = return.what(sp, items, report, AOI, basemap, boundary, clip_unit, ids = if(ids){ids = 'site_no'})

  if(save){

    save.file(data      = items,
              state     = state,
              county    = county,
              clip_unit = clip_unit,
              agency    = 'USGS',
              source    = "NWIS",
              dataset   = "nwis",
              other     =  NULL )
  }

class(items) = "HydroData"
return(items)

}



