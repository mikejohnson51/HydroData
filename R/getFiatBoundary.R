#' Get County and State SpatialPolygon(s)
#'
#' @details
#' \code{getFiatBoundary} gets a \code{SpatialPolygon} for a defiend state and/or county or those intersecting a clip_unit.
#' Fiat boundaries come from the 2017 US Census Bureau 2017 TIGER Dataset.
#'
#' All HydroData outputs are projected to \emph{'+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0+no_defs'}
#'
#' @param state     character. Full name or two character abbriviation. Not case senstive
#' @param county    character. Provide county name(s). Requires 'state' input.
#' @param clip_unit SpatialObject* or list. For details see \code{?getClipUnit}
#'
#' @return \code{getFiatBoundary} returns a \code{SpatialPolygon} Object
#' @export
#' @seealso \itemize{
#'          \item \code{\link{getClipUnit}}
#'          \item \code{\link{getAOI}}
#'          }
#'
#' @family HydroData 'get' functions
#'
#' @examples
#' \dontrun{
#' # Get Single State
#'     getFiatBoundary(state = "CA")
#'
#' # Get Multi-state
#'     getFiatBoundary(state = c("CA","Utah","Nevada"))
#'
#' # Get County
#'     getFiatBoundary(state = "CA", county = "San Luis Obispo")
#'
#' # Get Muli-county
#'    getFiatBoundary(state = "CA", county = c("San Luis Obispo", "Santa Barbara", "Ventura"))
#'
#' # Get counties that intersect with defined clip_unit
#'    getFiatBoundary(clip_unit = list("UCSB", 10, 10, "lowerleft"))
#'}
#'
#' @family
#'
#' @author
#' Mike Johnson

getFiatBoundary <- function(state = NULL, county = NULL, clip_unit = NULL) {

  map <- readRDS("data/countymaps.rds") %>% spTransform(HydroDataProj)

  if(!is.null(clip_unit)){
    A = getAOI(state = NULL, county = NULL, clip_unit = clip_unit)
    map = map[A, ]
    return(map)
  } else {

  state <- toupper(state)

  for (i in 1:length(state)) {
    if (nchar(state[i]) > 2) {
      state[i] <- simpleCap(tolower(state[i]))
    } else {
      state[i] <- setNames(state.name, state.abb)[state[i]]
    }
  }

  map <- map[map$STATE %in% state, ]

  if (is.null(county)) {
    map <- maptools::unionSpatialPolygons(map, ID = map$STATEFP)
    if (length(map) < 1) {
    }
    return(map)
  } else {
    county.map <- vector(mode = "character")
    for (i in 1:length(county)) {
      county.map <- append(county.map, simpleCap(tolower(county[i])))
    }
    map <- map[map$NAME %in% county.map, ]
    if (length(map) < 1) {
      stop("County defined does not exist. Please check spelling and that county exists in 2017.")
    }
    return(map)
  }
  }

}

