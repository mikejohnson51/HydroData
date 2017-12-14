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
#' Find all dams in Texas
#'
#' tx.dams = find_nid(state = "TX", keep.boundary = TRUE, keep.basemap = TRUE)
#' plot(tx.dams$basmap)
#' plot(tx.dams$boundary, add = TRUE)
#' plot(tx.dams$dams, add = TRUE)
#'
#' @export
#' @author
#' Mike Johnson

find_nid = function(state = NULL, county = NULL, clip_unit = NULL, keep.boundary = FALSE, keep.basemap = FALSE){

  AOI = define_AOI(state = state, county = county, clip_unit = clip_unit, get.basemap = keep.basemap)
  message("AOI defined as the ", nameAOI(state = state, county = county, clip_unit = clip_unit), ". Shapefile determined. Now loading loading NID database...")

  library(dams)
  data(nid_cleaned)
  dams = nid_cleaned[nid_cleaned$State != 'AK', ]
  dams = dams[dams$State != 'HI', ]
  dams = dams[dams$State != 'GU', ]
  dams = dams[dams$State != 'PR', ]
  dams = dams %>% drop_na(Longitude) %>% drop_na(Latitude)

  sp = SpatialPointsDataFrame(cbind(dams$Longitude, dams$Latitude), data = dams)
  message("All dams in CONUS loaded: ", formatC(dim(sp)[1], format="d", big.mark=","), " dams in total")
  rm(nid_cleaned)

  message("Subsetting to ", nameAOI(state = state, county = county, clip_unit = clip_unit))

  if(keep.basemap){
    sp@proj4string = AOI$shp@proj4string
    shp = sp[AOI$shp,]
  }else{
    sp@proj4string = AOI@proj4string
    shp = sp[AOI,]
  }

  message(formatC(as.numeric(length(shp)), format="d", big.mark=","), " NID dams found.")

  if(keep.boundary && keep.basemap){
    message("Shapefiles of dams,", nameAOI(state = state, county = county, clip_unit = clip_unit), ", and raster basemap returned")
    return(list(dams = shp, boundary = AOI$shp, basmap = AOI$bmap))

  }else if(!keep.boundary && keep.basemap ){
    message("Shapefile of dams and basemap returned")
    return(list(dams = shp, basmap = AOI$bmap))

  }else if(keep.boundary && !keep.basemap ){
    message("Dams, and ", nameAOI(state = state, county = county, clip_unit = clip_unit), " shapefile returned")
    return(list(dams = shp, boundary = AOI))

  }else{
    message("Dams shapefile returned")
    return(shp)
  }
}

