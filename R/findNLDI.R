#' Get NLDI Data
#'
#' This function acts as a formal query to the NLDI
#'
#' @param comid a COMID value (see \code{findNHD} and \code{findNearestCOMID})
#' @param nwis  a USGS station values (see \code{findUSGS} and \code{findNearestUSGS})
#' @param huc12 a HUC12 id (see \code{findHUC})
#' @param type a type of data to return ("UT", "UM","DD", "DM", "basin")
#' @param spatial if \code{TRUE} returned data will be sp, else of type sf
#' @param resources what type of resources to add ("comid", "huc12pp", "npdes_rad", "nwis", "wqp")
#'
#' @return a list of spatial / sf features
#' @export
#' @author Mike Johnson
#'

findNLDI = function(comid = NULL,
                   nwis = NULL,
                   huc12 = NULL,
                   type = NULL,
                   spatial = TRUE,
                   resources = NULL) {

  all.sources = c("comid", "huc12pp", "npdes_rad", "nwis", "wqp")

  get_basin_url = function(base.url, url) {
    l = readChar(url, nchars = 1000, useBytes = TRUE)
    comid = gsub(".*comid\":\"*|\",\"navigation*.*", "", l)
    return(paste0(base.url, "comid/", comid, "/basin"))
  }

  urls = list()

  if (sum(!is.null(comid), !is.null(nwis), !is.null(huc12)) != 1) {
    stop("message here")
  }

  base.url = "https://cida.usgs.gov/nldi/"

  if (!is.null(comid)) {
    url = paste0(base.url, "comid/", comid)
  }
  if (!is.null(nwis)) {
    url =  paste0(base.url, "nwissite/USGS-", nwis)
  }
  if (!is.null(huc12)) {
    url = paste0(base.url, "huc12pp/", huc12)
  }

  urls[["site"]] = url

  for (i in seq_along(type)) {
    if (type[i] %in% c("UM", "UT", "DD", "DM")) {
      urls[[type[i]]] = paste0(url, "/navigate/", type[i])
    }
  }

  if (any(type == 'basin')) {
    if (any(!is.null(comid), !is.null(nwis))) {
      urls[["basin"]] = paste0(url, "/basin")
    } else {
      urls[["basin"]] = get_basin_url(base.url, url)
    }
  }

  for (i in seq_along(resources)) {
    tmp = paste0(urls[[2]], "/", resources[i])
    if (length(nchar(readChar(tmp, nchars = 40))) != 0) {
      urls[[resources[i]]] = tmp
    } else {
      message(paste0("No '" , resources[i], "' in this region"))
    }
  }

  shp = lapply(urls, sf::read_sf)

  if (spatial) {
    shp = lapply(shp, sf::as_Spatial)
  }

  shp[['bb']] = AOI::getBoundingBox(shp[[2]])
  return(shp)

}
