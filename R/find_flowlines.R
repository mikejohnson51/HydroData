#' Find all NHD flowlines within an Area of Interest
#'
#' Function to locate all NHD flowlines within an Area of Interest
#'
#' @param state a character string. Can be full name or state abbriviation
#' @param county a character string. Can be full name or state abbriviation
#' @param clip_unit can be provided as a shapefile or as a vector defineing centroid and bounding box diminsion
#' @param keep.boundary logical. If TRUE, the AOI shapefile will be returned with gage data in a list
#' @param keep.basemap logical. If TRUE, the google basemap will be returned with gage data in a list
#'
#' @examples
#' Find all NHD flowlines in El Paso County, Colorado
#'
#'el.paso = find_flowlines(state = 'CO', county = 'El Paso', keep.boundary = TRUE, keep.basemap = TRUE)
#'
#'plot(el.paso$basemap)
#'plot(el.paso$boundary, add = TRUE, lwd = 5)
#'plot(el.paso$flowlines, add = TRUE, col = 'blue', lwd = el.paso$flowlines$streamorde)
#'
#' Get NHD COMIDs
#'
#' comids = el.paso$flowlines$comid
#'
#' @author
#' Mike Johnson

find_flowlines = function(state = NULL, county = NULL, clip_unit = NULL, keep.boundary = FALSE, keep.basemap = FALSE){

  AOI = define_AOI(state = state, county = county, clip_unit = clip_unit, get.basemap = keep.basemap)
  message("AOI defined as the ", nameAOI(state = state, county = county, clip_unit = clip_unit), ". Shapefile determined. Now loading NHD flowline data...")

  if(keep.basemap){
    bb = AOI$shp@bbox
  }else{
    bb = AOI@bbox
  }

    URL = paste0("https://cida.usgs.gov/nwc/geoserver/nhdplus/ows?service=WFS&version=2.0.0&request=GetFeature&typeNames=nhdplus:nhdflowline_network&srsName=EPSG:4326&bbox=",
                 min(bb[2,]), ",", min(bb[1,]),  ",", max(bb[2,]), ",", max(bb[1,]), "&outputFormat=SHAPE-ZIP")

    temp = tempfile()
    temp1 = tempfile()

    download.file(url = URL, destfile = temp, quiet = TRUE)
    unzip(temp, exdir =temp1)

    sl = readOGR(paste0(temp1,'/nhdflowline_network.shp'), verbose = FALSE)

    if(keep.basemap){
      sl = spTransform(sl, AOI$shp@proj4string)
      shp = sl[AOI$shp,]
    }else{
      sl = spTransform(sl, AOI@proj4string)
      shp = sl[AOI,]
    }

    unlink(temp); unlink(temp1)

    message(formatC(as.numeric(length(shp)), format="d", big.mark=","), " flowlines found.")

    if(keep.boundary && keep.basemap){
      message("Shapefiles of flowlines, ", nameAOI(state = state, county = county, clip_unit = clip_unit), ", and raster basemap returned")
      return(list(flowlines = shp, boundary = AOI$shp, basemap = AOI$bmap))

    }else if(!keep.boundary && keep.basemap ){
      message("Shapefile of flowlines and basemap returned")
      return(list(flowlines = shp, basemap = AOI$bmap))

    }else if(keep.boundary && !keep.basemap ){
      message("Flowlines, and ", nameAOI(state = state, county = county, clip_unit = clip_unit), " shapefile returned")
      return(list(flowlines = shp, boundary = AOI))

    }else{
      message("Flowlines shapefile returned")
      return(shp)
    }
}

?find_flowlines


