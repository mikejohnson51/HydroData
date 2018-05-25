#' Find USA Water Bodies (Polygon)
#'
#' @description
#' \code{findWaterboides} returns a list of \code{Spatial*} Objects cropped to an Area of Interest.\cr\cr
#' To better understand defining an AOI using '\emph{state}', '\emph{county}' and '\emph{clip_unit}' see \code{getAOI} and \code{getClipUnit}.\cr\cr
#' Returned \code{list} can be interactivly explored via \code{\link{explore}}.\cr\cr
#' All outputs are projected to \code{CRS '+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0+no_defs'} and spatial water bodies data is (\emph{down})loaded from the \href{http://open-esrifederal.opendata.arcgis.com/datasets/9dff3cf646704abd9e74265f02abeb09_0}{ESRI Federal Open Data Portal}.
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
#' @param ids  If TRUE, returns a list of water body names in AOI
#' @param save If TRUE, data is written to a HydroData folder in users working directory.
#'
#' @seealso  \code{\link{getAOI}}
#' @seealso  \code{\link{findReservoir}}
#' @seealso  \code{\link{explore}}
#'
#' @family HydroData 'find' functions
#'
#' @return
#' \code{findWaterbodies} returns a named list of minimum length 1:
#'
#' \enumerate{
#' \item 'waterbodies': A \code{SpatialLinesDataFrame*}\cr
#'
#' Pending parameterization, \code{findRoads} can also return:
#'
#' \item 'basemap':   A \code{RasterLayer*} basemap if \code{basemap = TRUE}
#' \item 'boundry':   A \code{SpatialPolygon*} of AOI if \code{boundary = TRUE}
#' \item 'fiat':      A \code{SpatialPolygon*} of intersected county boundaries if \code{boundary = TRUE}
#' \item 'ids':       A vector of waterbody names if \code{ids = TRUE}
#' }
#'
#' @examples
#' \dontrun{
#' # Find  water bodies in San Diego County, CA
#'
#' wb = findWaterbodies(state = "CA", county = "San Diego", basemap = T, boundary = T)
#'
#' # Static Mapping
#'
#' plot(wb$basemap)
#' plot(wb$boundary, add = T)
#' plot(wb$waterbodies, add = T, col = 'blue')
#'
#' # Generate Interactive Map
#'
#' explore(wb)
#'}
#' @export
#' @author
#' Mike Johnson


findWaterbodies = function(state = NULL, county = NULL, clip_unit = NULL, boundary = FALSE, basemap = FALSE, ids = FALSE, save = FALSE){

  AOI = getAOI(state = state, county = county, clip_unit = clip_unit)

  sp = download.shp('http://open-esrifederal.opendata.arcgis.com/datasets/9dff3cf646704abd9e74265f02abeb09_0.zip', "water bodies")
  sp = sp[AOI,]

  if (length(sp) == 0) { stop("0 waterbodies found in AOI") }

  message(formatC(as.numeric(length(sp)), format="d", big.mark=","), " water bodies found within AOI")

  items = list(name = nameAOI(state, county, clip_unit),
               source = "ESRI",
               waterbodies = sp)

  report = "Returned list includes: ESRI waterbodies polygon shapefile"

  items = return.what(sp, items, report, AOI, basemap, boundary, clip_unit, ids = if(ids){ids = 'NAME'})

  if(save){
      save.file(data    = items,
                state   = state,
                county  = county,
                clip_unit = clip_unit,
                agency  = 'ESRI',
                source  = "Global",
                dataset = "waterbodies",
                other   = NULL )
    }

  class(items) = "HydroData"
  return(items)
}



