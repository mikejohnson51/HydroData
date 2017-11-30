#' Define Area of interest (AOI)
#'
#' Internal function used to generate AOI shapefiles in all HydroData functions.
#'
#' @param state a character string. Can be full name or state abbriviation
#' @param county a character string. Can be full name or state abbriviation
#' @param clip_unit can be provided as a shapefile or as a vector defineing centroid and bounding box diminsion
#' @param get_basemap logical. If TRUE AOI shapefile will be returned as a list also containing a basemap of AOI
#'
#' @examples
#' #By state
#' define_AOI(state = 'CA')
#'
#' #By state, county combination
#' define_AOI(state = 'California', county = 'Santa Barbara')
#'
#' #By external shapefile
#' define_AOI(clip_unit = rgdal::readOGR('la_metro.shp'))
#'
#' #By 10 mile2 bounding box using users location as centroid
#' define_AOI(clip_unit = c(get_ip_loc(), 10, 10))
#'
#' #By 10 mile2 bounding box using the 'KMART near UCSB' as centroid
#' define_AOI(clip_unit = c('KMART near UCSB', 10, 10))
#'
#' #By HUC8 unit covering users location
#' define_AOI(clip_unit = get_WBD(get_ip_loc(), level = 8))
#'
#' @author
#' Mike Johnson


define_AOI = function(state = NULL, county = NULL, clip_unit = NULL, get.basemap = TRUE){

if(!is.null(state) && !is.null(clip_unit)){
  stop("Only 'state' or 'clip_unit' can be used. Set the other to NULL")}

if(is.null(state) && is.null(clip_unit) && !is.null(county)){
  stop("The use of 'county' requires the 'state' parameter be used as well.")}

if(is.null(state) && is.null(clip_unit)){
  stop("Requires a 'clip_unit' or 'state' parameter to execute")}

if(!is.null(clip_unit) && (!is.null(state) || !is.null(county))){
    stop("If providing 'clip_unit', leave 'state' and 'county' as 'NULL'")}

if(!is.null(state) && !is.character(state)){
    stop("State must be a character value. Try surrounding in qoutes...")}

if(!is.null(state) && !(state %in% state.abb || state %in% state.name)){
  stop("State not recongized. Full names or abbreviations can be used. Please check spelling.")
}

################################################################################################

if(is.null(clip_unit) && !is.null(state)){
  shp = get_region_sp(state = state, county = county)

  if(get.basemap == TRUE){
    bmap = suppressWarnings(dismo::gmap(shp, lonlat = TRUE))
    return(list(shp = shp, bmap = bmap))
  }else{
    return(shp)
  }
}

if(class(clip_unit) == 'SpatialPolygons'){
  shp = clip_unit
  bmap = suppressWarnings(dismo::gmap(shp, lonlat = TRUE))

  if(get.basemap == TRUE){
    bmap = suppressWarnings(dismo::gmap(shp, lonlat = TRUE))
    return(list(shp = shp, bmap = bmap))
  }else{
    return(shp)
  }
}

################################################################################################

if(class(clip_unit) != "SpatialPolygons" && !(length(clip_unit) == 3 || length(clip_unit) == 4)){
  stop("'clip_unit' must be entered in one of three ways:
(1) As a SpatailPolygon object
(2) As a 3 element list with a (name, bound box height and bounding box width)
(3) As a 4 element list with a (latitude, longitude, bound box height and bounding box width)'")
}

if(length(clip_unit) == 3){
  if(length(clip_unit[[1]]) && is.numeric(clip_unit[[1]]) && is.numeric(clip_unit[[1]])){
    p = clip_unit[[1]]
    clip_unit[[4]] = clip_unit[[3]]
    clip_unit[[3]] = clip_unit[[2]]
    clip_unit[[2]] = p[2]
    clip_unit[[1]] = p[1]

  } else if(!is.character(clip_unit[[1]]) || !is.numeric(clip_unit[[2]]) || !is.numeric(clip_unit[[3]])){
    stop("A clip_unit with length 3 must be defined by:
         (1) A name (i.e 'UCSB', 'The Walmart near the National Water Center') (character)
         (2) A bound box height (in miles) (numeric)
         (3) A bound box width (in miles) (numeric)")
  } else {
    location = clip_unit[[1]]
    h = clip_unit[[2]]
    w = clip_unit[[3]]
  }
}

if(length(clip_unit) == 4){
if(!is.numeric(clip_unit[[1]]) || !is.numeric(clip_unit[[2]]) || !is.numeric(clip_unit[[3]]) || !is.numeric(clip_unit[[4]])){
  stop("A clip_unit with length 4 must be defined by:
      (1) A latitude (numeric)
      (2) A longitude (numeric)
      (2) A bound box height (in miles) (numeric)
      (3) A bound box width (in miles) (numeric)")
}
}

if(length(clip_unit) == 4){
  if(!(-14.4246950943  <= clip_unit[[1]] && clip_unit[[1]] <= 71.4395725902)){
    stop("Latitude must be vector element 1 and within: (-14.4246950943 <= x <= 71.4395725902)")
  }

  # Check 3: Ensure longitude is correct
  if( !(-179.229655487 <= clip_unit[[2]] && clip_unit[[2]] <= 179.856674735)){
    stop("Longitude must be vector element 2 and within: (-179.229655487 <= x <= 179.856674735)")
  }

  location = c(clip_unit[[1]], clip_unit[[2]])
  h = clip_unit[[3]]
  w = clip_unit[[4]]
  }

shp = get_clip_unit(location = location, width = w, height = h)

if(get.basemap == TRUE){
  bmap = suppressWarnings(dismo::gmap(shp, lonlat = TRUE))
  return(list(shp = shp, bmap = bmap))
}else{
  return(shp)
}
}


