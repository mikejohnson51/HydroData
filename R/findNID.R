#' Find US Army Core Dams within Area of Interest
#'
#' @description  \code{findNID} finds all US Army Corps Dams for an Area of Interest from the National Inventory of Dams dataset.
#' This dataset is access through the dams package.
#'
#'  \code{findNID} returns a named list of minimum length 1:
#' \enumerate{
#' \item 'dams':      A \code{SpatialPointsDataFrame} of NID dams and metadata
#' \item 'basemap':   A \code{RasterLayer} basemap if 'basemap' is \code{TRUE}
#' \item 'fiat':      A \code{SpatialPolygon} of fiat boundaries if 'boundary' is \code{TRUE}
#' \item 'clip':      A \code{SpatialPolygon} of clip unit boundary if 'boundary' is \code{TRUE}
#' }
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
#' @param save logical. If TRUE, all data is saved to a HydroData folder created in users working directory. Find working directory with \code{getwd()}
#'
#' @seealso \itemize{
#'          \item \code{\link{getClipUnit}}
#'          \item \link{getAOI}
#'          \item \link[HydroData]{explore}
#'          }
#'
#' @family HydroData 'find' functions
#'
#' @return All HydroData outputs are projected to \emph{'+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0+no_defs'}
#'
#' @examples
#' \dontrun{
#' # Find all dams in Texas
#'
#' tx.dams = findNID(state = "TX", boundary = TRUE, basemap = 'r', save= TRUE)
#' plot(tx.dams$basmap)
#' plot(tx.dams$boundary, add = TRUE)
#' plot(tx.dams$dams, add = TRUE)
#'}
#' @export
#' @author
#' Mike Johnson

findNID = function(state = NULL, county = NULL, clip_unit = NULL, boundary = FALSE, basemap = FALSE, save = FALSE){

  dams = dams::nid_cleaned %>% tidyr::drop_na(Longitude, Latitude)

  AOI = getAOI(state, county, clip_unit)

  sp = SpatialPointsDataFrame(cbind(dams$Longitude, dams$Latitude), data = dams, proj4string = HydroDataProj)
  message("All dams in CONUS loaded: ", formatC(dim(sp)[1], format="d", big.mark=","), " dams in total")

  sp = sp[AOI, ]

  if (length(sp) == 0) { stop("0 dams found in AOI") }

  message(formatC(as.numeric(length(sp)), format="d", big.mark=","), " NID dams found")

  items = list(name = nameAOI(state, county, clip_unit),
               source = "USACE NID",
               dams = sp)

  report = "Returned list includes: NID dams shapefile"

  items = return.what(sp, items, report, AOI, basemap, boundary, clip_unit, ids = NULL)

    if(save){
      save.file(data = items,
                state = state,
                county = county,
                clip_unit = clip_unit,
                agency  = 'USACE',
                source  = "NID",
                dataset = "dams",
                other   = NULL )
    }

  class(items) = "HydroData"
  return(items)
}



