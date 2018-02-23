#' Find Water Bodies within Area of Interest
#'
#' \code{findWaterbodies} finds polygon waterbodies within an Area of Interest from the ESRI Global Waterbodies database
#'
#'\code{findWaterbodies} returns a named list of minimum length 1:
#' \enumerate{
#' \item 'wb': A \code{SpatialLinesDataFrame} of NHD flowlines and metadata
#' \item 'basemap':   A \code{RasterLayer} basemap if 'basemap' is \code{TRUE}
#' \item 'fiat':      A \code{SpatialPolygon} of fiat boundaries if 'boundary' is \code{TRUE}
#' \item 'clip':      A \code{SpatialPolygon} of clip unit boundary if 'boundary' is \code{TRUE}
#' \item 'ids':       A vector of flowline COMIDs if 'ids' is \code{TRUE}
#' }
#'
#'
#' @param state     character. Provide full name(s) or two character abbriviation(s). Not case senstive
#' @param county    character. Provide county name(s). Requires \code{state} input.
#' @param clip_unit SpatialObject* or list. For details see \code{?getClipUnit}
#' @param boundary  logical. If TRUE, the AOI \code{SpatialPolygon(s)} will be joined to returned list
#' @param basemap   logical. If TRUE, a basemap will be joined to returned list from \code{\link[dismo]{gmap}}.
#'
#'  #' If a user wants greater control over basemap apperance replace TRUE with either:
#' \itemize{
#' \item't':  a terrain imagery basemap
#' \item's':  a sattilite imagery basemap
#' \item'h':  a hybrid imagery basemap
#' \item'r':  a roadmap imagery basemap
#' }
#'
#' @param save logical. If TRUE, all data is saved to a HydroData folder created in users working directory. Find working directory with \code{\link[getwd()]}
#'
#' @examples
#' \dontrun{
#' #Find all water bodies in San Luis Obispo, CA
#'
#' slo.wb = findWaterbodies(state = 'CA', county = "San Luis Obispo",
#'                          boundary = TRUE, basemap = TRUE)
#'
#' plot(slo.wb$basemap)
#' plot(slo.wb$boundary, add = T)
#' plot(slo.wb$waterbodies, add = T, col = 'blue')
#'}
#' @export
#' @author
#' Mike Johnson

findWaterbodies = function(state = NULL, county = NULL, clip_unit = NULL, boundary = FALSE, basemap = FALSE, save = FALSE){

  items =  list()
  report = vector(mode = 'character')

  A = getAOI(state = state, county = county, clip_unit = clip_unit)
  message("AOI defined as the ", nameAOI(state = state, county = county, clip_unit = clip_unit), ".\nShapefile determined.\nLoading North American Water Bodies")

  URL = 'http://open-esrifederal.opendata.arcgis.com/datasets/9dff3cf646704abd9e74265f02abeb09_0.zip'

  sp = download.shp( URL, "water bodies") %>% spTransform(HydroDataProj)
  sp = sp[A,]
    message(formatC(as.numeric(length(sp)), format="d", big.mark=","), " water bodies found within ", nameAOI(state = state, county = county, clip_unit = clip_unit))

  items[['waterbodies']] = sp ; report = append(report, "Returned list includes: water bodies shapefile")

  if(length(report) > 1) {report[length(report)] = paste("and",  tail(report, n = 1))}
    message(paste(report, collapse = ", "))

  if (boundary) {
      if (!is.null(clip_unit)) {
        items[['fiat']] = getFiatBoundary(clip_unit = sp
        )
        report = append(report, "fiat boundary shapefile")
        items[['clip']] = A
        report = append(report, "clip boundary shapefile")
      } else {
        items[['fiat']] = A
        report = append(report, "boundary shapefile")
      }
    }

    if (!(basemap == FALSE)) {
      items[['basemap']] =  getBasemap(sp, type = basemap)     ##AOI$bmap
      report = append(report, "basemap raster")
    }

    if(save){
      save.file(data    = items,
                state   = state,
                county  = county,
                clip_unit = clip_unit,
                agency  = 'ESRI',
                source  = "FedData",
                dataset = "waterbodies",
                other   = NULL )
    }

  return(items)
}




