#' @title Find GAGESII points and basins
#' @description The 'Geospatial Attributes of Gages for Evaluating Streamflow, version II'
#' (\href{https://water.usgs.gov/GIS/metadata/usgswrd/XML/gagesII_Sept2011.xml#Entity_and_Attribute_Information}{GAGESII})
#' provides geospatial data and classifications for 9,322 stream gages maintained by the U.S. Geological Survey (USGS). \code{findGAGESII}
#' returns a \code{SpatialPointsDataFrame*} of GAGESII outlets within an AOI. Data comes from the USGS CIDA server and contains the following attributes:\cr
#' \itemize{
#' \item 'id'   : \code{character}  Internal feature number
#' \item 'STAID'   : \code{character}  USGS NWIS Station ID
#' \item 'STANAME': \code{character}  USGS NWIS Station Name
#' \item 'CLASS'   : \code{character}  Classification (Ref or Non-ref)
#' \item 'AGGECOREGION'   : \code{numeric}    Aggregated ecoregion
#' \item 'DRAIN_SQKM'    : \code{numeric}    Drainage area, sq km
#' \item 'HUC02'    : \code{numeric}    Hydrologic Unit Code, 2-digit
#' \item 'LAT_GAGE'   : \code{character}   %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>%
#' \item 'LNG_GAGE'   : \code{character}  Longitude, decimal degrees
#' \item 'STATE': \code{character}  State at gage location
#' \item 'HCDN_2009'   : \code{character}  If gage is part of HCDN-2009
#' \item 'ACTIVE09'   : \code{numeric}    If gage active in water year 2009
#' \item 'FLYRS1990'    : \code{numeric}    Number of complete years of flow data between 1900 and 2009
#' \item 'FLYRS1950'    : \code{numeric}    Number of complete years of flow data between 1950 and 2009
#' \item 'FLYRS1990'    : \code{numeric}    Number of complete years of flow data between 1990 and 2009
#' } \cr
#' If \code{basins = TRUE} a \code{SpatialPolygonsDataFrame} of the gage drainage basin will also be appended
#' to the returned list and contain the following attributes:
#' \itemize{
#' \item 'id'   : \code{character}  Internal feature number and data identifier
#' \item 'ogr_fid'   : \code{character}  Internal feature number
#' \item 'area': \code{character}  Basin Area
#' \item 'perimeter'   : \code{character}  Basein Parameters
#' \item 'gage_id'   : \code{numeric}    USGS NWIS Station ID
#' }\cr
#' @param AOI A Spatial* or simple features geometry, can be piped from \link[AOI]{getAOI}
#' @param basins If TRUE, returns a list of GAGESII basin in addition
#' @param ids If TRUE, a vector of gage IDs is added to retuned list (default = \code{FALSE})
#' @return a list() of minimum length 2: AOI and gagesII
#' @examples
#' \dontrun{
#' #Get GAGESII outlets for AOI
#' bas = getAOI(clip = list("UCSB", 10, 10)) %>%  findGAGESII()
#'
#' #Get GAGESII outlets and basins for AOI
#' bas = getAOI(clip = list("UCSB", 10, 10)) %>%  findGAGESII(basins = TRUE)
#' }
#' @author Mike Johnson
#' @export


findGAGESII = function(AOI = NULL,
                       basins = FALSE,
                       ids = FALSE) {

  if (!(class(AOI) %in% c("list", "HydroData"))) {
    AOI = list(AOI = AOI)
  }

  if(any(class(AOI$AOI) == "sf")){ AOI$AOI = as_Spatial(AOI$AOI) }

  sl = query_cida(AOI$AOI, type = 'gagesII', spatial = T)


  if (!is.null(sl)) {
    AOI[["gagesII"]]  = sl

    if (basins) {
      AOI[["gagesII_basin"]]  = query_cida(AOI$AOI, type = 'gagesii_basins', spatial = T)
    }

    report = paste0("Returned list includes: gagesII stations", if (basins) {
      " and basins"
    })

    AOI = return.what(AOI, type = 'gagesII', report, vals = if (ids) {
      "STAID"
    })
  }

  return(AOI)
}
