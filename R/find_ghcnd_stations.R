#' Locate all NOAA GCHN Stations within Area of Interest
#'
#' Function to locate all GCHN stations within Area of Interest
#'
#' @param state a character string. Can be full name or state abbriviation
#' @param county a character string. Can be full name or state abbriviation
#' @param clip_unit can be provided as a shapefile or as a vector defineing centroid and bounding box diminsion
#' @param keep.boundary logical. If TRUE, the AOI shapefile will be returned with station data in a list
#' @param keep.basemap logical. If TRUE, the google basemap will be returned with gage station in a list
#'
#' @examples
#' Find all GCHN stations in Harris County, Texas
#'
#' harris.clim = find_ghcnd_stations(state = 'TX', county = 'Harris')
#' plot(harris.clim$basemap)
#' plot(harris.clim$boundary, add = TRUE)
#' plot(harris.clim$gchn, add = TRUE, pch = 16, col = 'blue')
#'
#' Get Station IDs
#'
#' IDs = test$stations$ID
#'
#' @author
#' Mike Johnson

find_ghcnd_stations = function(state = NULL, county = NULL, clip_unit = NULL, keep.boundary = FALSE, keep.basemap = FALSE){

  AOI = define_AOI(state = state, county = county, clip_unit = clip_unit, get.basemap = keep.basemap)
  message("AOI defined as the ", nameAOI(state = state, county = county, clip_unit = clip_unit), ". Shapefile determined. Now loading global GHCN data...")

  url = 'https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-stations.txt'
  stations = read.table(url, fill = TRUE, header = FALSE, stringsAsFactors = FALSE,
                        colClasses = c(rep("character", 3), rep("NULL",6)), col.names = c("ID", "LAT", "LONG", rep("NULL",6)),
                        na.strings=c("",NA)
  )

  message("All GHCN Data loaded: ", formatC(dim(stations)[1], format="d", big.mark=","), " stations in total")

  message("Subsetting to ", nameAOI(state = state, county = county, clip_unit = clip_unit))

  if(keep.basemap){
    bb = AOI$shp@bbox
  }else{
    bb = AOI@bbox
  }

test = stations %>% drop_na(LAT) %>% drop_na(LAT) %>%
    mutate(LAT = as.numeric(LAT), LONG = as.numeric(LONG)) %>%
    filter(LAT  <= bb[2,2]) %>%
    filter(LAT  >= bb[2,1]) %>%
    filter(LONG >= bb[1,1]) %>%
    filter(LONG <= bb[1,2])

  sp = SpatialPointsDataFrame(cbind(test$LONG, test$LAT), test)

  if(keep.basemap){
    sp@proj4string = AOI$shp@proj4string
    shp = sp[AOI$shp,]
  }else{
    sp@proj4string = AOI@proj4string
    shp = sp[AOI,]
  }

  message(length(shp), " GHCN stations found.")

  if(keep.boundary && keep.basemap){
    message("List of gages, ", nameAOI(state = state, county = county, clip_unit = clip_unit), " shapefile, and raster basemap returned")
    return(list(stations = shp, boundary = AOI$shp, basemap = AOI$bmap))

  }else if(!keep.boundary && keep.basemap ){
    message("Shapefiles of stations and basemap returned")
    return(list(stations = shp, basemap = AOI$bmap))

  }else if(keep.boundary && !keep.basemap ){
    message("Stations, and ", nameAOI(state = state, county = county, clip_unit = clip_unit), " shapefile returned")
    return(list(station = shp, boundary = AOI))

  }else{
    message("Stations shapefile returned")
    return(shp)
  }

}

