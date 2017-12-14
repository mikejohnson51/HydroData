#' Get HUC6 Catchment Boundaries, and HAND Products
#'
#' Downloads and subsets Height Above Nearest Drainage (HAND) and Catchment raster data from the CyberGIS FTP server for an area of interest.
#' HUC6 HAND raster are very large, so inital processing can be time consuming
#'
#' @param state a character string. Can be full name or state abbriviation
#' @param county a character string. Can be full name or state abbriviation
#' @param clip_unit can be provided as a shapefile or as a vector defineing centroid and bounding box diminsion
#' @param keep.boundary logical. If TRUE, the AOI shapefile will be returned with gage data in a list
#'
#' @examples
#' al.hand = get_HAND(clip_unit = list("National Water Center", 10, 10))
#' plot(al.hand$hand, col = rev(blues9))
#' plot
#'
#' @author
#' Mike Johnson
#'
#' @return
#' This function downloads the associated shapefiles into the /Geospatial/Flowlines folder built using
#' \code{\link{build_files}}
#'
#' @seealso
#' \code{\link{build_files}}
#' \code{\link{get_rating_curve}}
#'
#' @export
#'

get_HAND = function(state = NULL, county = NULL, clip_unit = NULL, keep.boundary = FALSE, keep.flowlines = TRUE){

 AOI = define_AOI(state = state, county = county, clip_unit = clip_unit, get.basemap = FALSE) %>% spTransform(CRS = CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0 "))
 #message("AOI defined as the ", nameAOI(state = state, county = county, clip_unit = clip_unit), ". Shapefile determined. Now loading loading NID database...")

 flowlines =  suppressMessages(find_flowlines(clip_unit = AOI))
 HUC6 = unique(substr(flowlines$reachcode, 1, 6))
 message(length(HUC6) , " HUC6 unit(s) intersect(s) ", nameAOI(state = state, county = county, clip_unit = clip_unit), ". Now loading HAND data...")

########################################

  hand = list()
  catchment = list()
  temp = tempdir()

  for(i in 1:length(HUC6)){
    hand[[i]] = paste0(HUC6[i],"/", HUC6[i], "hand.tif")
    catchment[[i]] = paste0(HUC6[i],"/",HUC6[i], "catchmask.tif")
  }

  if(!is.null(hand)){
  for(i in 1:length(hand)){
    file = paste0(temp, "/",substr(hand[i],8,nchar(hand[i])))
    if(!file.exists(file)){
    message("Downloading HAND raster for HUC unit ", substr(hand[i],8,13))
    URL = paste0("http://141.142.170.172/nfiedata/HUC6/", hand[i])
    download.file(url = URL, destfile = file)
    }else{message("HAND raster for HUC unit ", substr(hand[i],8,13), " already exists.")
    }}}


  if(!is.null(catchment)){
  for(i in 1:length(catchment)){
    file = paste0(temp, "/", substr(catchment[i],8,nchar(catchment[i])))
    if(!file.exists(file)){
    message("Downloading catchment raster for HUC unit ", substr(catchment[i],8,13))
    URL = paste0("http://141.142.170.172/nfiedata/HUC6/", catchment[i])
    download.file(url = URL, destfile = file)
    }else{message("Catchment raster for HUC unit ", substr(catchment[i],8,13), " already exists.")
    }}}

  HUC_files = list.files(temp, pattern = HUC6, full.names = TRUE)
  raster.files = grep(".tif$", HUC_files, value = TRUE)

  hand = lapply(grep("hand",raster.files, value=TRUE), raster)
  hand = clip.merge(hand, bounds = AOI)

  catch = lapply(grep("catch", raster.files, value=TRUE), raster)
  catch = clip.merge(catch, bounds = AOI)

  items = list()
  items[["hand"]] = hand
  items[["catch"]] = catch
  if(keep.boundary){items[["boundary"]] = AOI}
  if(keep.flowlines){items[["flowlines"]] = flowlines}

  return(items)
}
