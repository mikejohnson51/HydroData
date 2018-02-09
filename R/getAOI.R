#' Get Area of interest (AOI) boundary
#'
#'@details
#' \code{getAOI} gets a bounding box or fiat boundary to serve as AOI in all \code{HydroData} functions. All HydroData outputs are projected
#' to \emph{'+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0+no_defs'}.
#'
#'
#' @param state     character.  Full name or two character abbriviation. Not case senstive
#' @param county    character. Can be full name or state abbriviation. Requires 'state' input.
#' @param clip_unit SpatialObject* or list. If a list, a clip unit requires 3 inputs:\enumerate{
#'                                     \item A point: \itemize{
#'                                     \item 'named location' ex: "UCSB"
#'                                      \item 'lat, lon' pair: ex: -36, -120
#'                                      }
#'                                      \item  Bounding box diminsions \itemize{
#'                                       \item  A bounding box height in miles ex: 10
#'                                        \item A bounding box width in miles ex: 10
#'                                      }
#'                                      \item The realtive location of the point to the bounding box \itemize{
#'                                         \item 'center', 'lowerleft', 'lowerright', 'upperrigh', 'upperleft'
#'                                         \item Default is: 'center'
#'                                         }
#'                                         }
#'
#' 3 to 5 members can be used to describe a clip unit and \strong{ORDER MATTERS!! (location, height, width, origin)} :\itemize{
#'                                     \item 3 members: location name, height, width\itemize{
#'                                         \item ex: \emph{list("UCSB", 10, 10) }}
#'                                     \item 4 members: lat, long, height, width\itemize{
#'                                         \item ex: \emph{list(36, -120, 10, 10) }}
#'                                     \item 4 members: location name, height, width, origin\itemize{
#'                                         \item ex: \emph{list("UCSB", 10, 10, "lowerright) }}
#'                                     \item 5 members: lat, long, height, width, origin\itemize{
#'                                         \item ex: \emph{list(36,-120, 10, 10, "upperright) }}
#'                                     }
#'
#' @return \code{getAOI} returns a \code{SpatialPolygon}
#' @export
#'
#' @examples
#' \dontrun{
#' #Get AOI defined by a state
#'     getAOI(state = 'CA')
#'
#' #Get AOI defined by state & county pair
#'     getAOI(state = 'California', county = 'Santa Barbara')
#'
#' #Get AOI defined by external shapefile
#'     getAOI(clip_unit = rgdal::readOGR('la_metro.shp'))
#'
#' #Get AOI defined by 10 mile2 bounding box using users location as centroid
#'     getAOI(clip_unit = c(get_ip_loc(), 10, 10))
#'
#' #Get AOI defined by 10 mile2 bounding box using the 'KMART near UCSB' as centroid
#'     getAOI(clip_unit = c('KMART near UCSB', 10, 10))
#'
#' #By HUC8 unit covering users location
#'     getAOI(clip_unit = getWBD(get_ip_loc(), level = 8))
#' }
#'
#' @seealso \itemize{
#'          \item \code{\link{getClipUnit}}
#'          \item \code{\link{getFiatBoundary}}
#'          }
#'
#' @family HydroData 'get' functions
#'
#' @author
#' Mike Johnson

getAOI = function(state = NULL, county = NULL, clip_unit = NULL){

#------------------------------------------------------------------------------#
# Error Catching                                                               #
#------------------------------------------------------------------------------#
    if(!is.null(state) && !is.null(clip_unit)){
        stop("Only 'state' or 'clip_unit' can be used. Set the other to NULL")
      }

    if(is.null(state) && is.null(clip_unit) && !is.null(county)){
        stop("The use of 'county' requires the 'state' parameter be used as well.")
      }

    if(is.null(state) && is.null(clip_unit)){
        stop("Requires a 'clip_unit' or 'state' parameter to execute")
      }

    if(!is.null(clip_unit) && (!is.null(state) || !is.null(county))){
        stop("If providing 'clip_unit', leave 'state' and 'county' as 'NULL'")
      }

    if(!is.null(state) && !is.character(state)){
        stop("State must be a character value. Try surrounding in qoutes...")
      }

    if(!is.null(state) && !(toupper(state) %in% datasets::state.abb || state %in% datasets::state.name)){
        stop("State not recongized. Full names or abbreviations can be used. Please check spelling.")
    }

#-----------------------------------------------------------------------------------#
# Fiat Boundary Defintion (Exisiting Spatial/Raster Feature or getFiatBoundary())   #
#-----------------------------------------------------------------------------------#

# AOI by state

  if(is.null(clip_unit) && !is.null(state)){
      shp <- getFiatBoundary(state = state, county = county)

      return(shp)
    }

# AOI by user shapefile

    if(grepl(pattern = "Raster", class(clip_unit), ignore.case = T, fixed = F) ){
      shp =  rasterToPolygons(clip_unit) %>%
        spTransform(HydroDataProj)
      return(shp)
      }

   if(grepl(pattern = "Spatial", class(clip_unit), ignore.case = T, fixed = F) ){
    shp =  clip_unit %>%
      spTransform(HydroDataProj)
    return(shp)
  }


#------------------------------------------------------------------------------#
# Clip Unit Defintion  (getClipUnit() for 3,4, or 5 inputs)                    #
#------------------------------------------------------------------------------#

# AOI defined by location and bounding box width and height

    if(length(clip_unit) == 3){
      if(is.numeric(clip_unit[[1]])){
        p <- clip_unit[[1]]
          clip_unit[[4]] <- clip_unit[[3]]
         clip_unit[[3]] <- clip_unit[[2]]
          clip_unit[[2]] <- p[2]
          clip_unit[[1]] <- p[1]

      } else if(!any(is.character(clip_unit[[1]]), is.numeric(clip_unit[[2]]),!is.numeric(clip_unit[[3]]))){
        stop("A clip_unit with length 3 must be defined by:
             (1) A name (i.e 'UCSB', 'The Walmart near the National Water Center') (character)
             (2) A bound box height (in miles) (numeric)
             (3) A bound box width (in miles) (numeric)")
      } else {
        location <- clip_unit[[1]]
        h        <- clip_unit[[2]]
        w        <- clip_unit[[3]]
        o <- NULL
      }
    }

# AOI defined by (centroid lat, long, and bounding box width and height) or (loaction, width, height, origin)

  if(length(clip_unit) == 4){

    if(all(
            all(is.numeric(clip_unit[[1]]), is.numeric(clip_unit[[2]]), is.numeric(clip_unit[[3]]), is.numeric(clip_unit[[4]])),
            all(!is.numeric(clip_unit[[1]]), is.numeric(clip_unit[[2]]), is.numeric(clip_unit[[3]]), !is.numeric(clip_unit[[4]]))
        )){

        stop("A clip_unit with length 4 must be defined by:
            (1) A latitude (numeric)
            (2) A longitude (numeric)
            (3) A bound box height (in miles) (numeric)
            (4) A bound box width (in miles) (numeric)

             or

             (1) A location (character)
             (2) A bound box height (in miles) (numeric)
             (3) A bound box width (in miles) (numeric)
             (3) A bound box origion (character)
             ")

    } else if (all(is.numeric(clip_unit[[1]]), is.numeric(clip_unit[[2]]), is.numeric(clip_unit[[3]]), is.numeric(clip_unit[[4]]))) {

      if(!(-14.4246950943  <= clip_unit[[1]] && clip_unit[[1]] <= 71.4395725902)){
        stop("Latitude must be vector element 1 and within: (-14.4246950943 <= x <= 71.4395725902)")
       }

      if( !(-179.229655487 <= clip_unit[[2]] && clip_unit[[2]] <= 179.856674735)){
        stop("Longitude must be vector element 2 and within: (-179.229655487 <= x <= 179.856674735)")
       }
          location <- c(clip_unit[[1]], clip_unit[[2]])
          h      <- clip_unit[[3]]
          w      <- clip_unit[[4]]
          o      <- NULL

  } else if (all(is.character(clip_unit[[1]]), is.numeric(clip_unit[[2]]), is.numeric(clip_unit[[3]]), is.character(clip_unit[[4]]))) {

    location <- clip_unit[[1]]
    h        <- clip_unit[[2]]
    w        <- clip_unit[[3]]
    o        <- clip_unit[[4]]

  }
}

# if AOI defined by lat, long, width, height, origin

  if(length(clip_unit) == 5){

    if(all( is.numeric(clip_unit[[1]]), is.numeric(clip_unit[[2]]), is.numeric(clip_unit[[3]]), is.numeric(clip_unit[[4]]), is.character(clip_unit[[5]]))){
      location <- c(clip_unit[[1]], clip_unit[[2]])
      h        <- clip_unit[[3]]
      w        <- clip_unit[[4]]
      o        <- clip_unit[[5]]
    }
  }

shp <- getClipUnit(location = location, width = w, height = h, origin = o)

#------------------------------------------------------------------------------#
# Return AOI                                                                   #
#------------------------------------------------------------------------------#

return(shp)

}


