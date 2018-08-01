#' Find GAGESII points and basins
#'
#' @description
#' \code{findNHD} returns a list of \code{Spatial*} Objects cropped to an Area of Interest.\cr\cr
#' To better understand defining an AOI using '\emph{state}', '\emph{county}' and '\emph{clip}' see \code{getAOI} and \code{getClipUnit}.\cr\cr
#' Returned \code{list} can be interactivly explored via \code{\link{explore}} and COMID values (\code{ids = TRUE}) allow for National Water Model access via \code{getNWM}.\cr\cr
#' All outputs are projected to \code{CRS'+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0+no_defs'} and stream networks are (\emph{down})loaded from the \href{https://cida.usgs.gov}{USGS}.
#'
#' @param AOI and AOI of Interest
#' @param ids If TRUE, returns a list of COMIDS for NHD reaches
#' @param basins If TRUE, returns a list of GAGESII basin in addition
#'
#' @family HydroData 'find' functions
#'
#' @export
#' @author Mike Johnson


findGAGESII = function(AOI = NULL, ids = FALSE, basins = FALSE) {

  if(class(AOI) != "list"){AOI = list(AOI = AOI)}

    AOI[["gagesII"]]  = query_cida(AOI$AOI, type = 'gagesII', spatial = T)

  if(basins){
    AOI[["gagesII_basin"]]  = query_cida(AOI$AOI, type = 'gagesii_basins', spatial = T)
  }

    report = paste0("Returned list includes: gagesII stations", if(basins){" and basins"})

    AOI = return.what(AOI, type = 'gagesII', report, vals = if(ids){"STAID"})

  return(AOI)
}
