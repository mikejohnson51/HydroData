#' Locate all NRCS Snowtel stations within an Area of Interest
#'
#' Function to locate all snowtel stations within and Area of Interest
#'
#' @param state a character string. Can be full name or state abbriviation
#' @param county a character string. Can be full name or state abbriviation
#' @param clip_unit can be provided as a shapefile or as a vector defineing centroid and bounding box diminsion
#' @param keep.boundary logical. If TRUE, the AOI shapefile will be returned with gage data in a list
#' @param keep.basemap logical. If TRUE, the google basemap will be returned with gage data in a list
#' @param save logical. If TRUE, all data is written to a HydroData folder in the working directory
#'
#' @examples
#' Find all snowtel stations in Nevada
#'
#' nv.snow = find_snowtel(state = 'NV', keep.boundary = TRUE, keep.basemap = TRUE, save = TRUE)
#' plot(nv.snow$basemap)
#' plot(nv.snow$boundary, add = TRUE, lwd = 5)
#' plot(nv.snow$snowtel, add = TRUE, pch = 8, col = "lightblue")
#'
#' @author
#' Mike Johnson

find_snowtel = function(state = NULL, county = NULL, clip_unit = NULL, keep.boundary = FALSE, keep.basemap = FALSE, ids = FALSE, save = FALSE){

  items =  list()
  report = vector(mode = 'character')

  AOI = define_AOI(state = state, county = county, clip_unit = clip_unit, get.basemap = keep.basemap)
  if (any(keep.basemap == TRUE, !is.null(clip_unit))) {A = AOI$shp} else {A = AOI}
    message("AOI defined as the ", nameAOI(state = state, county = county, clip_unit = clip_unit), ". Shapefile determined. Now loading loading CONUS USGS data...")

  snowtel = readRDS('data/snotelStation.rds')
  sp = SpatialPointsDataFrame(cbind(snowtel$LONG, snowtel$LAT), snowtel)
  sp@proj4string = A@proj4string
  sp = sp[A,]
  rm(snowtel)
    message(formatC(as.numeric(length(sp)), format="d", big.mark=","), " Snowtel stations found within ", nameAOI(state = state, county = county, clip_unit = clip_unit))

  items[['snowtel']] = sp ; report = append(report, "Returned list includes: Snowtel shapefile")
  if (keep.boundary) {items[['boundary']] = AOI$shp; report = append(report, "boundary shapefile")}
  if (keep.basemap) {items[['basemap']] = AOI$bmap ; report = append(report, "basemap raster")}
  if(length(report) > 1) {report[length(report)] = paste("and",  tail(report, n = 1))}
    message(paste(report, collapse = ", "))

  if(save){
      save.file(data = items,
                state = state,
                county = county,
                clip_unit = clip_unit,
                agency  = 'NRCS',
                source  = "Snowtel",
                dataset = "stations",
                other   = NULL )
  }

  return(items)
}

