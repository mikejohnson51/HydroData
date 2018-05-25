#' Find USGS recorded USA Reservoirs (Point Data)
#'
#' @description
#' \code{findReservoir} returns a list of \code{SpatialPointsDataframe*} Objects cropped to an Area of Interest.\cr\cr
#' To better understand defining an AOI using '\emph{state}', '\emph{county}' and '\emph{clip_unit}' see \code{getAOI} and \code{getClipUnit}.\cr\cr
#' Returned \code{list} can be interactivly explored via \code{\link{explore}}.\cr\cr
#' All outputs are projected to \code{CRS '+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0+no_defs'} and reservoir data is (\emph{down})loadeded from the \href{https://water.usgs.gov/GIS}{USGS GIS portal}
#'
#' @param state     Full name(s) or two character abbriviation(s). Not case senstive
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
#' @param ids  If TRUE, returns a list of reservoir names
#' @param save If TRUE, data is written to a HydroData folder in users working directory.
#'

#' @seealso \code{\link{getAOI}}
#' @seealso \code{\link{explore}}
#'
#' @family HydroData 'find' functions
#'
#' @return
#' \code{findReservoir} returns a list of minimum length 1:
#'
#' \enumerate{
#' \item 'reservoirs': A \code{SpatialPointsDataFrame}\cr
#'
#' Pending parameterization, \code{findReservoir} can also return:
#'
#' \item 'basemap':   A \code{RasterLayer*} basemap if \code{basemap = TRUE}
#' \item 'boundry':   A \code{SpatialPolygon*} of AOI if \code{boundary = TRUE}
#' \item 'fiat':      A \code{SpatialPolygon*} of intersected county boundaries if \code{boundary = TRUE}
#' \item 'ids':       A vector of reservoir names if \code{ids = TRUE}
#' }
#'
#' @examples
#' \dontrun{
#' # Find all reservoirs in Texas
#'
#' tx.res = findReservoir(state = "TX", boundary = TRUE, basemap = TRUE, save = TRUE)
#'
#' # Static Maping
#'
#' plot(tx.res$basmap)
#' plot(tx.res$boundary, add = TRUE)
#' plot(tx.res$reservoirs, add = TRUE)
#'
#' # Generate Interactive Map
#'
#' explore(tx.res)
#'}
#'
#' @export
#' @author
#' Mike Johnson

findReservoir = function(state = NULL,
                         county = NULL,
                         clip_unit = NULL,
                         boundary = FALSE,
                         basemap = FALSE,
                         save = FALSE,
                         ids = FALSE) {

  AOI = getAOI(state, county, clip_unit)

  sp = download.shp('https://water.usgs.gov/GIS/dsdl/reservoir_shp.zip', type = "reservoirs")

  sp = sp[AOI,]

  if (length(sp) == 0) { stop("0 reservoirs found in AOI") }

  message(formatC(as.numeric(length(sp)), format = "d", big.mark = ","), " reservoirs found")

  items = list(name = nameAOI(state,county,clip_unit),
               source = 'USGS',
               reservoirs = sp
               )

  report = "Returned list includes: Reservoir shapefile"

  items = return.what(sp, items, report, AOI, basemap, boundary, clip_unit, ids = if(ids){ids = "RESNAME"} )

  if (save) {
    save.file(
      data = items,
      state = state,
      county = county,
      clip_unit = clip_unit,
      agency  = 'USGS',
      source  = "Water",
      dataset = "reservoirs",
      other   = NULL
    )
  }

  class(items) = 'HydroData'
  return(items)
}
