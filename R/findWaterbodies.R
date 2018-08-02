#' Find National Hydrography Data Waterbodies
#'
#' @description
#' \code{findWaterbodies} returns a list of \code{Spatial*} Objects cropped to an Area of Interest.\cr\cr
#' To better understand defining an AOI using '\emph{state}', '\emph{county}' and '\emph{clip}' see \code{getAOI} and \code{getClipUnit}.\cr\cr
#' Returned \code{list} can be interactivly explored via \code{\link{explore}} and COMID values (\code{ids = TRUE}) allow for National Water Model access via \code{getNWM}.\cr\cr
#' All outputs are projected to \code{CRS'+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0+no_defs'} and stream networks are (\emph{down})loaded from the \href{https://cida.usgs.gov}{USGS}.
#'

#'
#' @param ids  If TRUE, returns a list of COMIDS for NHD reaches
#' @param save If TRUE, data is written to a HydroData folder in users working directory.
#'
#' @seealso  \code{\link{getAOI}}
#' @seealso  \code{\link{getNWM}}
#' @seealso  \code{\link{explore}}
#'
#' @family HydroData 'find' functions
#'
#' @return
#' \code{findNHD} returns a list of minimum length 1:
#'
#' \enumerate{
#' \item 'flowlines': A \code{SpatialLinesDataFrame}\cr
#'
#'
#' Pending parameterization, \code{findNHD} can also return:
#'
#' \item 'basemap':   A \code{RasterLayer*} basemap if \code{basemap = TRUE}
#' \item 'boundry':   A \code{SpatialPolygon*} of AOI if \code{boundary = TRUE}
#' \item 'fiat':      A \code{SpatialPolygon*} of intersected county boundaries if \code{boundary = TRUE}
#' \item 'ids':       A vector of COMIDs if \code{ids = TRUE}
#' }
#'
#'
#' @examples
#' \dontrun{
#' # Find NHD data for El Paso County, Colorado
#'
#' el.paso = findNHD(state = 'CO',
#'                   county = 'El Paso',
#'                   boundary = TRUE,
#'                   basemap = 'r',
#'                   ids = TRUE)
#'
#' # Static Mapping
#'
#'  plot(el.paso$basemap)
#'  plot(el.paso$boundary, lwd = 5, add = T)
#'  plot(el.paso$flowlines, col = 'blue', lwd = el.paso$flowlines$streamorde, add = T)
#'
#' # Generate Interactive Map
#'
#'   explore(el.paso, save = TRUE)
#'
#' # Get flow data for reaches
#'
#' flows = getNWM(ids = el.paso$ids, startDate = '2017-01-01', endDate = '2017-12-31')
#'
#' # Interactivly explore timeseries data
#'
#' inspect(flows, save = TRUE)
#' }
#'
#' @export
#' @author Mike Johnson
#'

findWaterbodies = function(AOI = NULL, ids = FALSE) {

  if(!(class(AOI) %in% c("list","HydroData"))){AOI = list(AOI = AOI)}

    sl = query_cida(AOI$AOI, type = "nhdwaterbody", spatial = TRUE)

    if(!is.null(sl)){

    AOI[["waterbodies"]] = sl

    report = "Returned list includes: NHD waterbodies shapefile"

    AOI = return.what(AOI, type = 'waterbodies', report, vals = if(ids){"objectid"})
   }

    return(AOI)
  }




