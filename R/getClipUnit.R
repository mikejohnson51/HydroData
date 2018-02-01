#' Get a bouding box for a location
#'
#' @details
#' \code{getClipUnit} gets a \code{SpatialPolygon} for a defiend state and/or county or those intersecting a clip_unit.
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







getClipUnit = function(location = NULL, width = NULL, height = NULL, origin = NULL){

    if(class(location) == "numeric"){ location = location }
    if(is.null(origin)){ origin = 'center' }

    if(class(location) == "character"){
      location = suppressMessages( dismo::geocode(location, output = 'latlon') )
      location = c(location$lat, location$lon)
    }

    if(origin == "center"){
      df = (height/2)/69                               # north/south
      dl = ((width/2)/69) / cos(location[1] * pi/180)  # east/west
      south = location[1] - df
      north = location[1] + df
      west  = location[2] - dl
      east  = location[2] + dl
    }

    if(origin == "lowerleft"){
      df = (height)/69
      dl = ((width)/69) / cos(location[1] * pi/180)
      south = location[1]
      north = location[1] + df
      west  = location[2]
      east  = location[2] + dl
    }

    if(origin == "lowerright"){
        df = (height)/69
        dl = ((width)/69) / cos(location[1] * pi/180)
        south = location[1]
        north = location[1] + df
        west  = location[2] - dl
        east  = location[2]
    }

    if(origin == "upperright"){
        df = (height)/69
        dl = ((width)/69) / cos(location[1] * pi/180)
        south = location[1] - df
        north = location[1]
        west  = location[2] - dl
        east  = location[2]
    }

    if(origin == "upperleft"){
        df = (height)/69
        dl = ((width)/69) / cos(location[1] * pi/180)
        south = location[1] - df
        north = location[1]
        west  = location[2]
        east  = location[2] + dl
    }

    coords = matrix(c(west, south,
                      east, south,
                      east, north,
                      west, north,
                      west, south),
                      ncol = 2,
                      byrow = TRUE)

    P1 = Polygon(coords)
    shp = SpatialPolygons(list(Polygons(list(P1), ID = "a")), proj4string= HydroDataProj)
    return(shp)
}

