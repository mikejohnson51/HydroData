#'  Find USGS Stream Gages within a defined area
#'
#' @details
#'  \code{fingUSGS} finds USGS NWIS stream gages within an Area of Interest. Metadata allows for easy data download via
#'  the USGS \code{\link[dataRetrieval]{readNWISdv}} package using 'site_no' or the \code{getNWM} function using 'feature_ids'.
#'  To better understand how to define an AOI for all \code{HydroData} functions please see \code{?getAOI}.
#'
#'
#' \code{findUSGS} returns a named list of minimum length 1:
#' \enumerate{
#' \item 'nwis':     A \code{SpatialPointsDataFrame} of stations and metadata
#' \item 'basemap':  A \code{RasterLayer} basemap if basemap is \code{TRUE}
#' \item 'boundary': A \code{SpatialPolygon} of defined AOI if boundary is \code{TRUE}
#' }
#'
#' @param state     character. Provide full name(s) or two character abbriviation(s). Not case senstive
#' @param county    character. Provide county name(s). Requires 'state' input.
#' @param clip_unit SpatialObject* or list. For details see \code{?getClipUnit}
#' @param boundary  logical. If TRUE, the AOI \code{SpatialPolygon(s)} will be joined to returned list
#' @param basemap   logical. If TRUE, a basemap will be joined to returned list
#' @param save      logical. If TRUE, all data is saved to a HydroData folder created in users working directory
#' @param ids       logical. Returns a vector of station IDs for 'getting' data in list
#'
#' @return All HydroData outputs are projected to \emph{'+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0+no_defs'}
#' @export
#' @seealso \itemize{
#'          \item \code{\link{getClipUnit}}
#'          \item \link{getAOI}
#'          \item \link[dataRetrieval]{readNWISdv}
#'          }
#'
#' @family HydroData 'find' functions
#'
#' @examples
#' \dontrun{
#' # Find all stations in Harris County, Texas:
#'
#'  harris.usgs <- findUSGS(state = 'TX', county = 'harris',
#'                              boundary = TRUE, basemap = TRUE, ids = TRUE,
#'                              save = TRUE)
#'
#' # Find all stations within 10 square miles of the National Water Center:
#'
#'  nwc.usgs <- findUSGS(clip_unit = list("National Water Center", 10,  10),
#'                              boundary = TRUE, basemap = TRUE, ids = TRUE,
#'                              save = TRUE)
#'
#' # Visualize Data:
#'
#'  plot(harris.usgs$basemap)
#'  plot(harris.usgs$boundary, add = TRUE, lwd = 5)
#'  plot(harris.usgs$usgs, add = TRUE, lwd = 2, col = "darkgreen")
#'
#' # Get discharge data for all stations
#'
#'  Q <- dataRetrieval::readNWISdv(siteNumbers = harris.usgs$ids[1],
#'                                     parameterCd = "00060")
#'  Q <- dataRetrieval::renameNWISColumns(Q)
#'
#' #Plot Discharge Data
#'  p <- ggplot(Q, aes(Date,Flow)) +
#'          labs(x = "Date",
#'               y = attr(Q,"variableInfo")$variableDescription,
#'               title = paste0("Streamflow at USGS Station: ", harris.usgs$ids[1])) +
#'           geom_line()
#'  p
#' }
#'
#' @author
#' Mike Johnson
#'


findUSGS = function(state = NULL, county = NULL, clip_unit = NULL, boundary = FALSE, basemap = FALSE, ids = FALSE, save = FALSE){
  #------------------------------------------------------------------------------#
  # Define AOI and reporters                                                     #
  #------------------------------------------------------------------------------#
  items =  list()
  report = vector(mode = 'character')

  AOI  = getAOI(state = state, county = county, clip_unit = clip_unit)

  message("AOI defined as the ", nameAOI(state = state, county = county, clip_unit = clip_unit), ".
           Now loading loading CONUS USGS data...")

  #------------------------------------------------------------------------------#
  # Load Data from data folder                                                   #
  #------------------------------------------------------------------------------#
  load('data/usgsStations.rda')


  sp = SpatialPointsDataFrame(cbind(usgsStations$lon_reachCent, usgsStations$lat_reachCent), usgsStations)
  sp@proj4string = AOI@proj4string
  sp = sp[AOI,]

  message("All USGS Data loaded: ", formatC(dim(sp)[1], format="d", big.mark=","), " gages in total")
  rm(usgsStations)

  message(formatC(as.numeric(length(sp)), format="d", big.mark=","),
          " USGS gages found within ", nameAOI(state = state, county = county, clip_unit = clip_unit))

  #------------------------------------------------------------------------------#
  # Format list to return based on user input                                    #
  #------------------------------------------------------------------------------#

                 items[['nwis']] = sp
                 report = append(report, "Returned list includes: USGS NWIS shapefile")

  if (basemap)  {items[['basemap']] = getBasemap(AOI = AOI)
                 report = append(report, "basemap")
  }

  if (boundary) {items[['boundary']] = AOI
                 report = append(report, "boundary")
  }

  if (ids)      {items[['ids']] = sprintf("%08d", as.numeric(sp$site_no))
                 report = append(report, "list of station IDs")}

  if(length(report) > 1) {report[length(report)] = paste("and",  tail(report, n = 1))
  }

  message(paste(report, collapse = ", "))

  #------------------------------------------------------------------------------#
  # Optional Save                                                               #
  #------------------------------------------------------------------------------#

  if(save){

    save.file(data      = items,
              state     = state,
              county    = county,
              clip_unit = clip_unit,
              agency    = 'USGS',
              source    = "NWIS",
              dataset   = "gages",
              other     =  NULL )

  }

  #------------------------------------------------------------------------------#
  # Return files                                                                 #
  #------------------------------------------------------------------------------#

  return(items)
}




