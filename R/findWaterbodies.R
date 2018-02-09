#' Locate all Water Bodies within Area of Interest
#'
#' Function to locate polygon waterbodies within an Area of Interest from the USGS Inventory of reservoirs
#'
#' @param state a character string. Can be full name or state abbriviation
#' @param county a character string. Can be full name or state abbriviation
#' @param clip_unit can be provided as a shapefile or as a vector defineing centroid and bounding box diminsion
#' @param keep.boundary logical. If TRUE, the AOI shapefile will be returned with gage data in a list
#' @param keep.basemap logical. If TRUE, the google basemap will be returned with gage data in a list
#' @param save logical. If TRUE, all data is written to a HydroData folder in the working directory
#'
#' @examples
#' \dontrun{
#' #Find all water bodies in San Luis Obispo, CA
#'
#' slo.wb = findWaterbodies(state = 'CA', county = "San Luis Obispo",
#'                          keep.boundary = TRUE, keep.basemap = TRUE)
#'
#' plot(slo.wb$basemap)
#' plot(slo.wb$boundary, add = T)
#' plot(slo.wb$waterbodies, add = T, col = 'blue')
#'}
#' @export
#' @author
#' Mike Johnson

findWaterbodies = function(state = NULL, county = NULL, clip_unit = NULL, keep.boundary = FALSE, keep.basemap = FALSE, save = FALSE){

  items =  list()
  report = vector(mode = 'character')

  A = getAOI(state = state, county = county, clip_unit = clip_unit)
  message("AOI defined as the ", nameAOI(state = state, county = county, clip_unit = clip_unit), ".\nShapefile determined.\nLoading North American Water Bodies")
  #meta = data.frame(STATE = simpleCap(state), COUNTY = simpleCap(county),  TYPE = 'water.bodies', SOURCE = "ESRI Federal Open Data", ACCESS = Sys.time())

  URL = 'http://open-esrifederal.opendata.arcgis.com/datasets/9dff3cf646704abd9e74265f02abeb09_0.zip'

  sp = download.shp( URL, "water bodies") %>% spTransform(A@proj4string)
  sp = sp[A,]
    message(formatC(as.numeric(length(sp)), format="d", big.mark=","), " water bodies found within ", nameAOI(state = state, county = county, clip_unit = clip_unit))

  items[['waterbodies']] = sp ; report = append(report, "Returned list includes: water bodies shapefile")

  if(length(report) > 1) {report[length(report)] = paste("and",  tail(report, n = 1))}
    message(paste(report, collapse = ", "))

  if(save){
      save.file(data    = items,
                state   = state,
                county  = county,
                clip_unit = clip_unit,
                agency  = 'ESRI',
                source  = "FedData",
                dataset = "waterbodies",
                other   = NULL )
    }

  return(items)
}


