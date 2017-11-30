#' Locate all NOAA GCHN Stations within Area of Interest
#'
#' Function to locate all GCHN stations within Area of Interest
#'
#' @param state a character string. Can be full name or state abbriviation
#' @param county a character string. Can be full name or state abbriviation
#' @param clip_unit can be provided as a shapefile or as a vector defineing centroid and bounding box diminsion
#' @param keep.boundary logical. If TRUE, the AOI shapefile will be returned with station data in a list
#'
#' @examples
#' Find all GCHN stations in Harris County, Texas
#'
#' harris.clim = find_ghcnd_stations(state = "TX", county = "Harris")
#' plot(harris.clim$boundary)
#' plot(harris.clim$gchn, add = TRUE, pch = 16, col = 'blue')
#'
#' @author
#' Mike Johnson

find_ghcnd_stations = function(state = NULL, county = NULL, clip_unit = NULL, keep.boundary = TRUE){

  AOI = define_AOI(state = state, county = county, clip_unit = clip_unit, get.basmap = FALSE)

  message("AOI defined and shapefile determined, loading global GHCN data...")

  url = 'https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-stations.txt'

  stations = read.table(url, fill = TRUE, header = FALSE, stringsAsFactors = FALSE,
                        colClasses = c(rep("character", 3), rep("NULL",6)), col.names = c("ID", "LAT", "LONG", rep("NULL",6)),
                        na.strings=c("",NA)
  )

  message("All GHCN Data loaded: ", formatC(dim(stations)[1], format="d", big.mark=","), " stations in total")
  message("Subsetting to AOI")

test = suppressWarnings(stations %>% na.omit() %>% mutate(LAT = as.numeric(LAT), LONG = as.numeric(LONG)) %>%
    filter(LAT  <= AOI@bbox[2,2]) %>%
    filter(LAT  >= AOI@bbox[2,1]) %>%
    filter(LONG >= AOI@bbox[1,1]) %>%
    filter(LONG <= AOI@bbox[1,2]))

  sp = SpatialPointsDataFrame(cbind(test$LONG, test$LAT), test, proj4string = AOI@proj4string)
  sp = sp[AOI,]

  message(length(sp), " GHCN stations found within AOI")

  if(keep.boundary){
    return(list(boundary = AOI, gchn = sp))
  }
  return(sp)
  }




