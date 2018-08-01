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
#' @param clip SpatialObject* or list. For details see \code{?getClipUnit}
#' @param boundary  logical. If TRUE, the AOI \code{SpatialPolygon(s)} will be joined to returned list
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

findNID = function(AOI = NULL, ids = FALSE){

  if(class(AOI) != "HydroData"){AOI = list(AOI = AOI)}

  dams = dams::nid_cleaned
  dams = dams[!is.na(dams$Longitude),]
  dams = dams[!is.na(dams$Latitude),]

  sp = sp::SpatialPointsDataFrame(cbind(dams$Longitude, dams$Latitude), data = dams, proj4string = AOI::aoiProj)

  message("All dams in CONUS loaded: ", formatC(dim(sp)[1], format="d", big.mark=","), " dams in total")

  sp = sp[AOI$AOI, ]

  if (dim(sp)[1] == 0) { stop("0 dams found in AOI") }

  message(formatC(as.numeric(length(sp)), format="d", big.mark=","), " NID dams found")

  AOI[["dams"]] = sp

  report = "Returned list includes: NID dams shapefile"

  AOI = return.what(AOI, type = 'dams', report, vals = if(ids){"Dam_Name"})


  return(AOI)
}



