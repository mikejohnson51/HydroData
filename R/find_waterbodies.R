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
#' Find all water bodies in San Luis Obispo, CA
#'
#' slo.wb = find_waterbodies(state = 'CA', county = "San Luis Obispo", keep.boundary = TRUE, keep.basemap = TRUE)
#'
#' plot(slo.wb$basemap)
#' plot(slo.wb$boundary, add = T)
#' plot(slo.wb$waterbodies, add = T, col = 'blue')
#'
#' @export
#' @author
#' Mike Johnson

find_waterbodies = function(state = NULL, county = NULL, clip_unit = NULL, keep.boundary = FALSE, keep.basemap = FALSE, save = FALSE){

  items =  list()
  report = vector(mode = 'character')

  AOI = define_AOI(state = state, county = county, clip_unit = clip_unit, get.basemap = keep.basemap)

  if(length(AOI) > 1){A = AOI$shp} else {A = AOI}

  message("AOI defined as the ", nameAOI(state = state, county = county, clip_unit = clip_unit), ".\nShapefile determined.\nLoading North American Water Bodies")
  #meta = data.frame(STATE = simpleCap(state), COUNTY = simpleCap(county),  TYPE = 'water.bodies', SOURCE = "ESRI Federal Open Data", ACCESS = Sys.time())

  URL = 'http://open-esrifederal.opendata.arcgis.com/datasets/9dff3cf646704abd9e74265f02abeb09_0.zip'

  sp = download.shp( URL, "water bodies") %>% spTransform(A@proj4string)
  sp = sp[A,]
    message(formatC(as.numeric(length(sp)), format="d", big.mark=","), " water bodies found within ", nameAOI(state = state, county = county, clip_unit = clip_unit))

  items[['waterbodies']] = sp ; report = append(report, "Returned list includes: water bodies shapefile")
  if (keep.boundary) {items[['boundary']] = AOI$shp; report = append(report, "boundary shapefile")}
  if (keep.basemap) {items[['basemap']] = AOI$bmap ; report = append(report, "basemap raster")}
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


