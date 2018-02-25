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
#' \code{findWS} returns a named list of minimum length 1:
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


findWS = function(state = NULL,
                  county = NULL,
                  clip_unit = NULL,
                  boundary = FALSE,
                  basemap = FALSE,
                  ids = FALSE,
                  level = 10,
                  subbasins = FALSE,
                  HUC8 = NULL,
                  save = FALSE){

  data =  list()
  report = vector(mode = 'character')

  #huc = list()
  urls = NULL
  td <-  tempfile()

  if(!is.null(HUC8)) {
    HUC8 = HUC8
  } else {
    AOI = getAOI(state = state, county = county, clip_unit = clip_unit)
    message("Determining intersecting HUC8 units...")
    flow = suppressMessages(  findNHD(clip_unit = AOI, boundary = F) )

    HUC8 = unique(substr(flow$flowlines$reachcode, 1, 8))
    rm(flow)
  }

  message("There are ", length(HUC8), " HUC8 units in this AOI: ", paste(HUC8, collapse = ", "))

 if(subbasins){
   items =  seq(level,12, 2)
 } else {
    items = level
}

if(!(length(HUC8) > 1) ) {


  temp <-  tempfile(pattern = "WBD", fileext = ".zip")

  urls = paste0("https://prd-tnm.s3.amazonaws.com/StagedProducts/Hydrography/NHD/HU8/HighResolution/Shape/NHD_H_", HUC8 ,"_HU8_Shape.zip")

  download.file(url = urls, destfile =  temp)
  unzip(temp, exdir = td, overwrite = TRUE)

for( i in seq_along(items)){
      shp = rgdal::readOGR(paste0(td,'/Shape/WBDHU', items[i],".shp"), stringsAsFactors = F, verbose = F) %>% spTransform(HydroDataProj)
      data[[paste0("huc_", items[i])]] = shp[AOI, ]
}
  unlink(temp, recursive = T)
  #unlink(td, recursive = T)

 } else {

    series  = list()
    urls = paste0("https://prd-tnm.s3.amazonaws.com/StagedProducts/Hydrography/NHD/HU8/HighResolution/Shape/NHD_H_", HUC8 ,"_HU8_Shape.zip")

  for(i in seq_along(urls)){

    temp = tempfile(pattern = "WBD", fileext = ".zip")
      download.file(url =  urls[i], destfile =  temp, quiet = F)

      unzip(temp, exdir = td, overwrite = TRUE)

      for( j in seq_along(items)){
        shp = rgdal::readOGR(paste0(td,'/Shape/WBDHU', items[j],".shp"), stringsAsFactors = F, verbose = F) %>% spTransform(HydroDataProj)
        data[[paste0("huc_", items[j])]] = shp[AOI, ]
      }

      series[[paste0('hu_',HUC8[i])]] = data

    }

    all.files = unlist(series)

    for( j in seq_along(items)){
      data[[paste0("huc_", items[j])]] <- do.call(rbind, all.files[grepl(pattern = paste0(items[j], "$"), names(all.files))])
    }
 }

  ########################



  if (!(basemap == FALSE))  {
    if (basemap == TRUE) {
      type = 't'
      name = 'terrain'
    } else {
      type = basemap
    }

    if (type == 't') { name = 'terrain'   }
    if (type == 'h') { name = 'hybrid'    }
    if (type == 's') { name = 'satellite' }
    if (type == 'r') { name = 'roadmap'   }

    data[['basemap']] = getBasemap(AOI = data[[1]], type = type)
    report = append(report, paste(name, "basemap"))
  }


  if (boundary) { data[['boundary']] = AOI
  report = append(report, "AOI boundary")

  if (!is.null(clip_unit)) { data[['fiat']] = getFiatBoundary(clip_unit = data[[1]])
  report = append(report, "fiat boundary")
  }
  }

  if (ids) { data[['ids']] = HUC8
  report = append(report, "list of HUC8's")
  }

  if(length(report) > 1) {report[length(report)] = paste("and", tail(report, n = 1))}
  message(paste(report, collapse = ", "))

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

  return(data)

}

