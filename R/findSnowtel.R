#' Locate all NRCS snotel stations within an Area of Interest
#'
#' Function to locate all snotel stations within and Area of Interest
#'
#' @param state a character string. Can be full name or state abbriviation
#' @param county a character string. Can be full name or state abbriviation
#' @param clip_unit can be provided as a shapefile or as a vector defineing centroid and bounding box diminsion
#' @param keep.boundary logical. If TRUE, the AOI shapefile will be returned with gage data in a list
#' @param keep.basemap logical. If TRUE, the google basemap will be returned with gage data in a list
#' @param save logical. If TRUE, all data is written to a HydroData folder in the working directory
#' @param ids logical. If TRUE, returns a vector of station IDs
#'
#' @examples
#' \dontrun{
#' #Find all snotel stations in Nevada
#'
#' nv.snow = find_snotel(state = 'NV', keep.boundary = TRUE, keep.basemap = TRUE, save = TRUE)
#' plot(nv.snow$basemap)
#' plot(nv.snow$boundary, add = TRUE, lwd = 5)
#' plot(nv.snow$snotel, add = TRUE, pch = 8, col = "lightblue")
#'}
#' @author
#' Mike Johnson

findSnotel = function(state = NULL, county = NULL, clip_unit = NULL, keep.boundary = FALSE, keep.basemap = FALSE, ids = FALSE, save = FALSE){

  items =  list()
  report = vector(mode = 'character')

  A = getAOI(state = state, county = county, clip_unit = clip_unit)
    message("AOI defined as the ", nameAOI(state = state, county = county, clip_unit = clip_unit), ". Shapefile determined. Now loading loading CONUS USGS data...")

  load('data/snotelStations.rda')

  sp = SpatialPointsDataFrame(cbind(snotel$LONG, snotel$LAT), snotel)
  sp@proj4string = A@proj4string
  sp = sp[A,]
  rm(snotel)
    message(formatC(as.numeric(length(sp)), format="d", big.mark=","), " snotel stations found within ", nameAOI(state = state, county = county, clip_unit = clip_unit))

  items[['snotel']] = sp ; report = append(report, "Returned list includes: snotel shapefile")


  if(length(report) > 1) {report[length(report)] = paste("and",  tail(report, n = 1))}
    message(paste(report, collapse = ", "))

  if(save){
      save.file(data = items,
                state = state,
                county = county,
                clip_unit = clip_unit,
                agency  = 'NRCS',
                source  = "snotel",
                dataset = "stations",
                other   = NULL )
  }

  return(items)
}

