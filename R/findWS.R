#' Find Watershed Boundaries (WBD)
#'
#' @description
#' \code{findWS} returns a list of \code{Spatial*} Objects cropped to an Area of Interest.\cr\cr
#' To better understand defining an AOI using '\emph{state}', '\emph{county}' and '\emph{clip}' see \code{getAOI} and \code{getClipUnit}.\cr\cr
#' Returned \code{list} can be interactivly explored via \code{\link{explore}}.\cr\cr
#' All outputs are projected to \code{CRS '+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0+no_defs'} and spatial watershed data are (\emph{down})loaded from the  \href{https://prd-tnm.s3.amazonaws.com}{USGS Staged Data Server}.
#'
#' @param state    Full name(s) or two character abbriviation(s). Not case senstive
#' @param county    County name(s). Requires \code{state} input.
#' @param clip SpatialObject* or list. For details see \code{getClipUnit}
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
#' \item 'ids':       A vector of HUC8 units if \code{ids = TRUE}
#' }
#'
#' @examples
#'\dontrun{
#' # Find all HUC8, 10 and 12 units surrounding the National Water Center
#'
#'  ws = findWS(clip = list("National Water Center", 10 ,10),
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


findHUC = function(AOI = NULL,
                  level = 8,
                  subbasins = FALSE,
                  ids = FALSE){

  if(class(AOI) != "HydroData"){AOI = list(AOI = AOI)}

  sp = list()
  td <-  tempfile()
  series  = list()

  if(subbasins) { level =  seq(level, 12, 2) }

  if (all(level == 8)) {
    sp[["huc8"]] = query_cida(AOI$AOI, "huc08", spatial = TRUE)
  } else if (all(level == 12)) {
    sp[["huc12"]] = query_cida(AOI$AOI, "huc12", spatial = TRUE)
  } else {

    flow = query_cida(AOI$AOI, type ="huc08")
    HUC8 = flow$huc8

    message("There are ", length(HUC8), " HUC8 units in this AOI: ", paste(HUC8, collapse = ", "))

    urls = paste0(
      "https://prd-tnm.s3.amazonaws.com/StagedProducts/Hydrography/NHD/HU8/HighResolution/Shape/NHD_H_",
      HUC8 ,
      "_HU8_Shape.zip"
    )

      for (i in seq_along(urls)) {
        temp = tempfile(pattern = "WBD", fileext = ".zip")
        download.file(url =  urls[i],
                      destfile =  temp,
                      quiet = F)
        unzip(temp, exdir = td, overwrite = TRUE)

        for (j in seq_along(level)) {
          shp = rgdal::readOGR(
            paste0(td, '/Shape/WBDHU', level[j], ".shp"),
            stringsAsFactors = F,
            verbose = F
          )
          shp <- shp[!duplicated(data.frame(shp$Name)),]
          shp = sp::spTransform(shp, AOI::aoiProj)
          sp[[paste0("huc", level[j])]] = shp[AOI$AOI,]
        }

        series[[paste0('huc', HUC8[i])]] = sp

      }

      all.files = unlist(series)

      for (j in seq_along(level)) {
        sp[[paste0("huc", level[j])]] <-
          do.call(rbind, all.files[grepl(pattern = paste0(level[j], "$"), names(all.files))])
      }
    }

AOI = c(AOI, sp)

tmp = names(AOI)[grep("huc", names(AOI))]

if(length(tmp) > 1){
 tmp = tmp[which.max(as.numeric(gsub("huc", "", tmp)))]
}

report = paste0("Returning ", paste0("HUC", level, collapse = ", "), " shapefiles")

AOI = return.what(AOI, type = tmp, report, vals = if(ids){tmp})

return(AOI)

}

