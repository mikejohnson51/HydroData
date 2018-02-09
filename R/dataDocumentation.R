#' USGS station information
#'
#' Dataset containing information about USGS stations in the United States
#'
#' @docType data
#'
#' @format a \code{dataframe} instance, 1 row per station with columns:
#' \itemize{
#' \item 'OBJECTID':      A \code{character} Object id in the dataset
#' \item 'feature_id':    A \code{character}  NHD COMID of reach
#' \item 'site_no':       A \code{character}      USGS site number
#' \item 'site_name':     A \code{character}    USGS site name
#' \item 'da_sqkm':       A \code{numeric}        Area that drains to the location in square kilometers
#' \item 'lat_reachCent': A \code{numeric}  Latitude of NHD reach center
#' \item 'lon_reachCent': A \code{numeric}  Longitude of NHD reach center
#' }
#'
#' @source Derived from USGS and NHD datasets, provided by the National Water Center
#'
#' @examples
#' \dontrun{
#'  load("usgsStations.rda")
#' }

"usgsStations"

#' Koppen Climate Classification Raster, March 2017
#'
#' \code{kopRas} contains the March 2017 Global Koppen Climate Classification Dataset
#'
#' @docType data
#'
#' @format a \code{RasterLayer}
#'
#' @references  Kottek, M., J. Grieser, C. Beck, B. Rudolf, and F. Rubel, 2006:
#' World Map of the Köppen-Geiger climate classification updated. Meteorol. Z., 15, 259-263.
#' DOI: 10.1127/0941-2948/2006/0130.
#'
#' @source \href{http://koeppen-geiger.vu-wien.ac.at/present.htm}{Koppen Climate Data}
#'
#' @examples
#' \dontrun{
#'  load("koppen_raster.rda")
#' }

"kopRas"

#' DAYMET Tile Index
#'
#' \code{daymet_tiles} contains the Polygon tile index for DAYMET meterological Data
#'
#' @docType data
#'
#' @format a \code{SpatialPolygonsDataFrame}
#' \itemize{
#' \item 'TileID':  A \code{integer} Tile ID
#' \item 'XMin':    A \code{integer} minimum latitude
#' \item 'XMax':    A \code{integer} maximum latitude
#' \item 'YMin':    A \code{integer} minimum longitide
#' \item 'YMax':    A \code{integer} maximum longitude
#' }
#'
#' @source \href{https://daymet.ornl.gov/gridded.html}{DAYMET Tile Data}
#'
#' @examples
#' \dontrun{
#'  load("daymet_tiles.rda")
#' }

"daymet_tiles"


#' Snotel Stations
#'
#' \code{snotel} information regarding the metadata of NRCS SNOTEL stations
#'
#' @docType data
#'
#' @format a \code{dataframe} instance, 1 row per station with columns:
#' \itemize{
#' \item 'NETWORK':    A \code{integer} Network of interest
#' \item 'STATE':      A \code{integer}  State abbriviation
#' \item 'NAME':       A \code{character} Site name
#' \item 'START.DATE': A \code{integer}    Day of first measurement
#' \item 'LAT':        A \code{integer} Station Latitude
#' \item 'LONG':       A \code{character}  Station Longitude
#' \item 'ELEV':       A \code{character} Station Elevation
#' \item 'COUNTY':     A \code{character}  County
#' \item 'HUC12.NAME': A \code{character}  HUC 12 Name
#' \item 'HUC12.ID':   A \code{character}  HUC 12 code
#' }
#'
#' @source \href{https://wcc.sc.egov.usda.gov/nwcc/yearcount?network=sntl&state=&counttype=statelist}{SNOTEL INFORMATION}
#'
#' @examples
#' \dontrun{
#'  load("snotelStations.rda")
#' }

"snotel"

