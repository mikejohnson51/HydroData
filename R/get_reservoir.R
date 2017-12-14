#' Locate all USACE NID Dams within Area of Interest
#'
#' Function to locate all US Army Corps dams within an Area of Interest from the National Inventory of Dams
#'
#' @param state a character string. Can be full name or state abbriviation
#' @param county a character string. Can be full name or state abbriviation
#' @param clip_unit can be provided as a shapefile or as a vector defineing centroid and bounding box diminsion
#' @param keep.boundary logical. If TRUE, the AOI shapefile will be returned with gage data in a list
#' @param keep.basemap logical. If TRUE, the google basemap will be returned with gage data in a list
#'
#' @examples
#' Find all reservoirs in Texas
#'
#' tx.res = find_reservoir(state = "TX", keep.boundary = TRUE, keep.basemap = TRUE)
#' plot(tx.res$basmap)
#' plot(tx.res$boundary, add = TRUE)
#' plot(tx.res$dams, add = TRUE)
#'
#' @export
#' @author
#' Mike Johnson

find_res = function(state = NULL, county = NULL, clip_unit = NULL, keep.boundary = FALSE, keep.basemap = FALSE){

  AOI = define_AOI(state = state, county = county, clip_unit = clip_unit, get.basemap = keep.basemap)
  message("AOI defined as the ", nameAOI(state = state, county = county, clip_unit = clip_unit), ". Shapefile determined. Now loading USGS Reservoir database...")


# Get Data
  URL = 'https://water.usgs.gov/GIS/dsdl/reservoir_shp.zip'
  temp  = tempfile()
  temp1 = tempfile()
  download.file(url = URL, destfile = temp, quiet = TRUE)
  unzip(temp, exdir = temp1)
  sp = readOGR(paste0(temp1, "/reservoir_shp.shp"), verbose = FALSE)
    unlink(temp)
    unlink(temp1)
  message("All reservoirs in CONUS loaded: ", formatC(dim(sp)[1], format="d", big.mark=","), " in total.")
  message("Subsetting to ", nameAOI(state = state, county = county, clip_unit = clip_unit))

  if(keep.basemap){
    sp = spTransform(sp, AOI$shp@proj4string)
    shp = sp[AOI$shp,]
  }else{
    sp = spTransform(sp, AOI@proj4string)
    shp = sp[AOI,]
  }

  message(formatC(as.numeric(length(shp)), format="d", big.mark=","), " reservoirs found.")

  if(keep.boundary && keep.basemap){
    message("Shapefiles of reservoirs,", nameAOI(state = state, county = county, clip_unit = clip_unit), ", and raster basemap returned")
    return(list(reservoirs = shp, boundary = AOI$shp, basmap = AOI$bmap))

  }else if(!keep.boundary && keep.basemap ){
    message("Shapefile of reservoirs and basemap returned")
    return(list(reservoirs = shp, basmap = AOI$bmap))

  }else if(keep.boundary && !keep.basemap ){
    message("Reservoirs, and ", nameAOI(state = state, county = county, clip_unit = clip_unit), " shapefile returned")
    return(list(reservoirs = shp, boundary = AOI))

  }else{
    message("Dams shapefile returned")
    return(shp)
  }
}


