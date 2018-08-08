#' @title Find National Hydrography River Networks
#' @description
#' \code{findNHD} returns a \code{SpatialLinesDataframe} of all NHDFlowlines reaches within an AOI.
#' Data comes from the USGS CIDA server and contain 92 attributes, perhaps most notably:
#' \itemize{
#' \item 'comid'   : \code{integer}  Integer value that uniquely identifies the occurrence of each feature in the NHD
#' \item 'reachcode'   : \code{character}  Unique identifier for a ‘reach’. The first eight numbers are the WBD_HUC8
#' \item 'fdate': \code{POSITct}  Date of last feature modification
#' \item 'gnis_id'   : \code{character}    Unique identifier assigned by GNIS
#' \item 'gnis_name'   : \code{character}    Proper name, term, or expression by which a particular geographic entity is known
#' \item 'lengthkm'    : \code{numeric}    Length of linear feature based on Albers Equal Area
#' \item 'areasqkm'    : \code{numeric}    Area of areal feature based on Albers Equal Area,
#' \item 'flowdir'   : \code{character}     Direction of flow relative to coordinate order.
#' \item 'wbareacomi'   : \code{integer}  The COMID of the waterbody through which the flowline flows.
#' \item 'ftype': \code{character}  unique identifier of a feature type
#' }\cr
#' @param AOI  A Spatial* or simple features geometry, can be piped from \link[AOI]{getAOI}
#' @param ids  If TRUE,  a vector of airport ICAO codes is added to retuned list (default = \code{FALSE})
#' @return a list() of minimum length 2: AOI and nhd
#' @examples
#' \dontrun{
#' nhd  = getAOI(clip = list("Tuscaloosa, AL", 10, 10)) %>% findNHD()
#' }
#' @author Mike Johnson
#' @export

findNHD = function(AOI = NULL, ids = FALSE) {

  if(!(class(AOI) %in% c("list","HydroData"))){AOI = list(AOI = AOI)}

  sl = query_cida(AOI$AOI, type = 'nhdflowline_network', spatial = T)

  if(!is.null(sl)){

    AOI[["nhd"]] = sl

    report = "Returned list includes: nhd flowline shapefile"

    AOI = return.what(AOI, type = 'nhd', report, vals = if(ids){"comid"})
  }

  return(AOI)
}
