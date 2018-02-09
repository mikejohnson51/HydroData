#' Locate all Reservoirs within Area of Interest
#'
#' Function to locate all US reservoirs within an Area of Interest from the USGS Inventory of reservoirs
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
#' #Find all reservoirs in Texas
#'
#' tx.res = find_reservoir(state = "TX", keep.boundary = TRUE, keep.basemap = TRUE, save = TRUE)
#' plot(tx.res$basmap)
#' plot(tx.res$boundary, add = TRUE)
#' plot(tx.res$reservoirs, add = TRUE)
#'}
#' @export
#' @author
#' Mike Johnson

findReservoirs = function(state = NULL, county = NULL, clip_unit = NULL, keep.boundary = FALSE, keep.basemap = FALSE, save = FALSE){
  do.call(file.remove, list(list.files(tempdir(), full.names = T)))
  items =  list()
  report = vector(mode = 'character')

  A = getAOI(state = state, county = county, clip_unit = clip_unit)
  message("AOI defined as the ", nameAOI(state = state, county = county, clip_unit = clip_unit), ". Shapefile determined. Now loading USGS Reservoir database...")

  URL = 'https://water.usgs.gov/GIS/dsdl/reservoir_shp.zip'
  sp = download.shp(URL, type = "reservoirs") %>% spTransform(A@proj4string)
  sp = sp[A, ]
    message(formatC(as.numeric(length(sp)), format="d", big.mark=","), " reservoirs found within ", nameAOI(state = state, county = county, clip_unit = clip_unit))

  items[['reservoirs']] = sp ; report = append(report, "Returned list includes: Reservoir shapefile")


    if(length(report) > 1) {report[length(report)] = paste("and",  tail(report, n = 1))}
      message(paste(report, collapse = ", "))

  if(save){
      save.file(data = items,
                state = state,
                county = county,
                clip_unit = clip_unit,
                agency  = 'USGS',
                source  = "Water",
                dataset = "reservoirs",
                other   = NULL )
   }

    return(items)
}

