#' Find US Census Bureau TIGER Road Networks
#'
#' @description
#' \code{findRoads} returns a list of \code{Spatial*} Objects cropped to an Area of Interest.\cr\cr
#' To better understand defining an AOI using '\emph{state}', '\emph{county}' and '\emph{clip_unit}' see \code{getAOI} and \code{getClipUnit}.\cr\cr
#' Returned \code{list} can be interactivly explored via \code{\link{explore}}.\cr\cr
#' All outputs are projected to \code{CRS '+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0+no_defs'} and road networks are (\emph{down})loaded from the 2017 \href{https://www2.census.gov/geo/tiger/TIGER2017}{US Census servers}.
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
#' @param ids  If TRUE, returns a list of road names in AOI
#' @param save If TRUE, data is written to a HydroData folder in users working directory.
#'
#' @seealso  \code{\link{getAOI}}
#' @seealso  \code{\link{explore}}
#'
#' @family HydroData 'find' functions
#'
#' @return
#' \code{findRoads} returns a named list of minimum length 1:
#'
#' \enumerate{
#' \item 'roads': A \code{SpatialLinesDataFrame*}\cr
#'
#' Pending parameterization, \code{findRoads} can also return:
#'
#' \item 'basemap':   A \code{RasterLayer*} basemap if \code{basemap = TRUE}
#' \item 'boundry':   A \code{SpatialPolygon*} of AOI if \code{boundary = TRUE}
#' \item 'fiat':      A \code{SpatialPolygon*} of intersected county boundaries if \code{boundary = TRUE}
#' \item 'ids':       A vector of road names if \code{ids = TRUE}
#' }
#'
#' @examples
#'\dontrun{
#' # Find Roads Near UCSB
#'
#' roads = findRoads(clip_unit = list("UCSB", 10, 10), basemap = T, boundary = T)
#'
#' # Static Mapping
#'
#' plot(roads$basemap)
#' plot(roads$boundary, add = T)
#' plot(roads$roads, add = T)
#'
#' # Generate Interactive Map
#'
#' explore(roads)
#'}
#'
#' @export
#' @author
#' Mike Johnson
#'

findRoads = function(state = NULL,
                     county = NULL,
                     clip_unit = NULL,
                     boundary = FALSE,
                     basemap = FALSE,
                     ids = FALSE,
                     save = FALSE) {

########## 1. Define AOI ##########

  input.shp = list()

  A = getAOI(state, county, clip_unit)

  if (!is.null(clip_unit)) {
    AOI = getFiatBoundary(clip_unit = A)
  } else {
    AOI = A
  }

  FIP = sprintf("%05d", as.numeric(AOI$geoid))

  if (is.null(FIP)) {
    FIP = sprintf("%05d", as.numeric(AOI$map$FIP))
  }

########## 2. Download Data ##########

  urls = paste0("https://www2.census.gov/geo/tiger/TIGER2017/ROADS/tl_2017_",FIP, "_roads.zip")

  if (length(urls) > 1) { verb = 'are'; noun = 'files' } else { verb = 'is'; noun = 'file' }

  message(paste("There", verb, length(urls), "TIGER", noun, "in this scene."))

  for (i in seq_along(urls)) {
   input.shp[[i]] = download.shp(URL = urls[1], type = paste('TIGER', i))
   input.shp[[i]] = input.shp[[i]][A,]
  }

  sl <- do.call("rbind", input.shp)
  sl <- raster::crop(x = sl,  y = raster::extent(A))

  items = list(
    name = nameAOI(state, county, clip_unit),
    source = "TIGER",
    roads = sl)

  report = "Returned list includes: Cropped TIGER roads shapefile"

  items = return.what(sp = sl, items, report, AOI, basemap, boundary, clip_unit, ids = if(ids){ids = 'FULLNAME'})

  if (save) {
    save.file(
      data = items,
      state = state,
      county = county,
      clip_unit = clip_unit,
      agency  = 'USCensus',
      source  = "TIGER",
      dataset = "roads",
      other   = NULL
    )
  }

  class(items) = "HydroData"
  return(items)
}
