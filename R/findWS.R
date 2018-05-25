#' Find Watershed Boundaries (WBD)
#'
#' @description
#' \code{findWS} returns a list of \code{Spatial*} Objects cropped to an Area of Interest.\cr\cr
#' To better understand defining an AOI using '\emph{state}', '\emph{county}' and '\emph{clip_unit}' see \code{getAOI} and \code{getClipUnit}.\cr\cr
#' Returned \code{list} can be interactivly explored via \code{\link{explore}}.\cr\cr
#' All outputs are projected to \code{CRS '+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0+no_defs'} and spatial watershed data are (\emph{down})loaded from the  \href{https://prd-tnm.s3.amazonaws.com}{USGS Staged Data Server}.
#'
#' @param state    Full name(s) or two character abbriviation(s). Not case senstive
#' @param county    County name(s). Requires \code{state} input.
#' @param clip_unit SpatialObject* or list. For details see \code{getClipUnit}
#' @param boundary  If TRUE, the AOI \code{SpatialPolygon(s)} will be joined to returned list
#' @param basemap   If TRUE, a basemap will be joined to returned list
#'
#'  If a user wants greater control over basemap apperance replace TRUE with either:
#' \itemize{
#' \item't':  google terrain basemap
#' \item's':  google sattilite imagery basemap
#' \item'h':  google hybrid basemap
#' \item'r':  google roads basemap
#' }
#'
#' @param ids  If TRUE, returns a list of HUC8 in AOI
#' @param save If TRUE, data is written to a HydroData folder in users working directory.
#' @param level defines the HUC level of interest
#' @param subbasins If TRUE, all subbasins of the supplied \code{level} will be joined to retuned list
#' @param HUC8 option to supply known HUC8 code(s)
#'
#' @seealso  \code{\link{getAOI}}
#' @seealso  \code{\link{explore}}
#'
#' @family HydroData 'find' functions
#'
#' @return
#' \code{findHUC} returns a named list of minimum length 1:
#'
#' \enumerate{
#' \item 'huc_': A \code{SpatialPolygonDataFrame*} for each requested HUC level defined by \code{level} and \code{subbasins} \cr
#'
#' Pending parameterization, \code{findWS} can also return:
#'
#' \item 'basemap':   A \code{RasterLayer*} basemap if \code{basemap = TRUE}
#' \item 'boundry':   A \code{SpatialPolygon*} of AOI if \code{boundary = TRUE}
#' \item 'fiat':      A \code{SpatialPolygon*} of intersected county boundaries if \code{boundary = TRUE}
#' \item 'ids':       A vector of HUC8 units if \code{ids = TRUE}
#' }
#'
#' @examples
#'\dontrun{
#' # Find all HUC8, 10 and 12 units surrounding the National Water Center
#'
#'  ws = findWS(clip_unit = list("National Water Center", 10 ,10),
#'             boundary = T,
#'             basemap = T,
#'             subbasins = T,
#'             level = 8)
#'
#' # Static Mapping
#'
#' plot(ws$basemap)
#' plot(ws$boundary, add = T)
#' plot(ws$huc_8, lwd = 3, add = T)
#' plot(ws$huc_10, border = 'red', lwd = 2, add = T)
#' plot(ws$huc_12, border = 'blue', add = T)
#'
#' # Generate Interactive Map
#'
#' explore(ws)
#'}
#'
#' @export
#' @author
#' Mike Johnson


findHUC = function(state = NULL,
                  county = NULL,
                  clip_unit = NULL,
                  level = 10,
                  subbasins = FALSE,
                  HUC8 = NULL,
                  boundary = FALSE,
                  basemap = FALSE,
                  ids = FALSE,
                  save = FALSE){

  td <-  tempfile()
  data = list(name = nameAOI(state, county, clip_unit), source = "WBD")

  if(!is.null(HUC8)) {
    HUC8 = HUC8
  } else {
    AOI = getAOI(state, county, clip_unit)
    message("Determining intersecting HUC8 units...")
    flow = suppressMessages(  findNHD(clip_unit = AOI) )
    HUC8 = unique(substr(flow$flowlines$reachcode, 1, 8))
  }

  message("There are ", length(HUC8), " HUC8 units in this AOI: ", paste(HUC8, collapse = ", "))

 if(subbasins){
   level =  seq(level,12, 2)
 } else {
    level = level
}

urls = paste0("https://prd-tnm.s3.amazonaws.com/StagedProducts/Hydrography/NHD/HU8/HighResolution/Shape/NHD_H_", HUC8 ,"_HU8_Shape.zip")

if(length(HUC8) == 1 ) {

  temp <-  tempfile(pattern = "WBD", fileext = ".zip")
  download.file(url = urls, destfile =  temp, quiet = TRUE)
  unzip(temp, exdir = td, overwrite = TRUE)

  list.files(paste0(td,'/Shape'))

for( i in seq_along(level)){
      shp = rgdal::readOGR(paste0(td,'/Shape/WBDHU', level[i],".shp"), stringsAsFactors = F, verbose = F) %>% spTransform(HydroDataProj)
      data[[paste0("huc_", level[i])]] = shp[AOI, ]
}

unlink(temp, recursive = T)

} else {

    series  = list()

  for(i in seq_along(urls)){

    temp = tempfile(pattern = "WBD", fileext = ".zip")
    download.file(url =  urls[i], destfile =  temp, quiet = T)
    unzip(temp, exdir = td, overwrite = TRUE)

    for( j in seq_along(level)){
        shp = rgdal::readOGR(paste0(td,'/Shape/WBDHU', level[j],".shp"), stringsAsFactors = F, verbose = F) %>% spTransform(HydroDataProj)
        data[[paste0("huc_", level[j])]] = shp[AOI, ]
    }

    series[[paste0('hu_', HUC8[i])]] = data

    }

    all.files = unlist(series)

    for( j in seq_along(level)){
      data[[paste0("huc_", level[j])]] <- do.call(rbind, all.files[grepl(pattern = paste0(level[j], "$"), names(all.files))])
    }
}

report = paste0("Returning ", paste0("HUC", level, collapse = ", "), " shapefiles")
items = return.what(sp, items = data, report, AOI, basemap, boundary, clip_unit, ids = NULL )

if(ids){data[["ids"]] = HUC8}


if(save){
    save.file(data = data,
              state = state,
              county = county,
              clip_unit = clip_unit,
              agency  = 'USGS',
              source  = "WBD",
              dataset = "HUC",
              other   = NULL )
  }

class(items) = "HydroData"
return(items)

}

