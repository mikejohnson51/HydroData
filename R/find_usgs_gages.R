#' Locate all USGS NWIS Stream Gages within an Area of Interest
#'
#' Function to locate all USGS stations within and Area of Interest
#'
#' @param state a character string. Can be full name or state abbriviation
#' @param county a character string. Can be full name or state abbriviation
#' @param clip_unit can be provided as a shapefile or as a vector defineing centroid and bounding box diminsion
#' @param keep.boundary logical. If TRUE, the AOI shapefile will be returned with gage data in a list
#' @param keep.basemap logical. If TRUE, the google basemap will be returned with gage data in a list
#'
#' @examples
#' Find all USGS stream gages  in Harris County, Texas
#'
#' harris.usgs = find_USGS_stations(state = 'TX', county = 'harris', keep.boundary = TRUE, keep.basemap = TRUE)
#' plot(harris.usgs$basemap)
#' plot(harris.usgs$boundary, add = TRUE, lwd = 5)
#' plot(harris.usgs$gages, add = TRUE, lwd = 2, col = "darkgreen")
#'
#' Get gage IDs
#'
#' IDs = test$gages$feature_id
#'
#' @author
#' Mike Johnson

find_USGS_gages = function(state = NULL, county = NULL, clip_unit = NULL, keep.boundary = FALSE, keep.basemap = FALSE){

  AOI = define_AOI(state = state, county = county, clip_unit = clip_unit, get.basemap = keep.basemap)
  if(is.null(clip_unit)){AOI = AOI} else {AOI = AOI$map}

  message("AOI defined as the ", nameAOI(state = state, county = county, clip_unit = clip_unit), ". Shapefile determined. Now loading loading CONUS USGS data...")

  #Load usgsStation Data
    load('data/usgsStations.Rdata')
  #Convert to Spatial Points Dataframe
    sp = SpatialPointsDataFrame(cbind(usgsStations$lon_reachCent, usgsStations$lat_reachCent), usgsStations)
  # Remove file
    message("All USGS Data loaded: ", formatC(dim(sp)[1], format="d", big.mark=","), " gages in total")

    rm(usgsStations)

    message("Subsetting to ", nameAOI(state = state, county = county, clip_unit = clip_unit))

  if(keep.basemap){
    sp@proj4string = AOI$shp@proj4string
    shp = sp[AOI$shp,]
  }else{
    sp@proj4string = AOI@proj4string
    shp = sp[AOI,]
  }

    message(formatC(as.numeric(length(shp)), format="d", big.mark=","), " USGS gages found.")

      if(keep.boundary && keep.basemap){
        message("Shapefiles of gages,", nameAOI(state = state, county = county, clip_unit = clip_unit), ", and raster basemap returned")
        return(list(gages = shp, boundary = AOI$shp, basmap = AOI$bmap))

      }else if(!keep.boundary && keep.basemap ){
        message("Shapefile of gages and basemap returned")
        return(list(gages = shp, basmap = AOI$bmap))

      }else if(keep.boundary && !keep.basemap ){
        message("Gages, and ", nameAOI(state = state, county = county, clip_unit = clip_unit), " shapefile returned")
        return(list(gages = shp, boundary = AOI))

      }else{
        message("Gages shapefile returned")
        return(shp)
      }
}

