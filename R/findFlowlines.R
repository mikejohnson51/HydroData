#' Find all NHD flowlines within an Area of Interest
#'
#' Download NHD flowlines within an Area of Interest
#'
#' @param state a character string. Can be full name or state abbriviation
#' @param county a character string. Can be full name or state abbriviation
#' @param clip_unit can be provided as a shapefile or as a vector defineing centroid and bounding box diminsion
#' @param keep.boundary logical. If TRUE, the AOI shapefile will be returned with gage data in a list
#' @param keep.basemap logical. If TRUE, the google basemap will be returned with gage data in a list
#' @param comids logical. If TRUE, returns a list of COMIDS for all stream reaches on AOI
#' @param save logical. If TRUE, all data is written to a HydroData folder in the working directory
#'
#' @examples
#' \dontrun{
#' #Find all NHD flowlines in El Paso County, Colorado
#'
#' el.paso = findNHD(state = 'CO', county = 'El Paso',
#'           keep.boundary = TRUE, keep.basemap = TRUE, comids = TRUE, save = TRUE)
#'
#' plot(el.paso$basemap)
#' plot(el.paso$boundary, add = TRUE, lwd = 5)
#' plot(el.paso$flowlines, add = TRUE, col = 'blue', lwd = el.paso$flowlines$streamorde)
#'}
#' @export
#' @author
#' Mike Johnson

findNHD = function(state = NULL, county = NULL, clip_unit = NULL, keep.boundary = FALSE, keep.basemap = FALSE, comids = FALSE, save = FALSE){
  #do.call(file.remove, list(list.files(tempdir(), full.names = T)))
  items =  list()
  report = vector(mode = 'character')
  AOI = getAOI(state = state, county = county, clip_unit = clip_unit)
    message("AOI defined as the ", nameAOI(state = state, county = county, clip_unit = clip_unit), ". Shapefile determined. Now loading NHD flowline data...\n")

  bb = AOI@bbox

  URL = paste0("https://cida.usgs.gov/nwc/geoserver/nhdplus/ows?service=WFS&version=2.0.0&request=GetFeature&typeNames=nhdplus:nhdflowline_network&srsName=EPSG:4326&bbox=",
                 min(bb[2, ]), ",",
                 min(bb[1, ]), ",",
                 max(bb[2, ]), ",",
                 max(bb[1, ]), "&outputFormat=SHAPE-ZIP")

  sl = download.shp(URL = URL, type = 'NHD flowlines') %>% spTransform(AOI@proj4string)
  sl = sl[AOI, ]


  items[['flowlines']] = sl ; report = append(report, "Returned list includes: flowline shapefile")
   if (keep.boundary) {items[['boundary']] = AOI$shp; report = append(report, "boundary shapefile")}
   if (keep.basemap) {items[['basemap']] = AOI$bmap ; report = append(report, "basemap raster")}
   if (comids) {items[['comids']] = sl$comid ;  report = append(report, "list of comids")}

  if(length(report) > 1) {report[length(report)] = paste("and",  tail(report, n = 1))}
   message(paste(report, collapse = ", "))

   if(save){
     save.file(data = items,
               state = state,
               county = county,
               clip_unit = clip_unit,
               agency  = 'USGS',
               source  = "NHD",
               dataset = "flowlines",
               other   = NULL )
   }

   return(items)
}

#do.call(file.remove, list(list.files(tempdir(), full.names = TRUE)))
