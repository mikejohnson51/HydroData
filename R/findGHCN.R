#' Find NOAA Maintained GHCN Stations
#'
#' @description
#' \code{findGHCN} finds Global Historical Climatology Network (GHCN) stations within Area of Interest.
#' To better understand defining an AOI using '\emph{state}', '\emph{county}' and '\emph{clip_unit}' see \code{getAOI} and \code{getClipUnit}.\cr\cr
#' Returned \code{list} can be interactivly explored via \code{\link{explore}} and ID values (\code{ids = TRUE}) allow for GHCN data access via \code{getGHCN}.\cr\cr
#' All outputs are projected to \code{CRS '+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0+no_defs'} and station metadata is (\emph{down})load from a \href{ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt}{NOAA FTP}.
#'
#' @param state     Full name(s) or two character abbriviation(s). Not case senstive
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
#' @param parameters Select the variable of interest.The five core elemets are:\cr
#' PRCP = Precipitation (tenths of mm)\cr
#' SNOW = Snowfall (mm)\cr
#' SNWD = Snow depth (mm)\cr
#' TMAX = Maximum temperature (tenths of degrees C)\cr
#' TMIN = Minimum temperature (tenths of degrees C)\cr
#'
#' To see the other options consult the GHCN documentation \href{ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt}{here}\cr
#' If \code{parameters = NULL}, all recorded parameters in AOI will be returned
#' \cr
#'
#' @param ids  If TRUE, returns a list of IDs for selected stations
#' @param save If TRUE, data is written to a HydroData folder in users working directory.
#'
#' @export
#' @seealso  \code{\link{getAOI}}
#' @seealso  \code{\link{getGHCN}}
#' @seealso  \code{\link{explore}}
#'
#' @family HydroData 'find' functions
#'
#' @return
#' \code{findNHD} returns a list of minimum length 1:
#'
#' \enumerate{
#' '\item 'gchn': A \code{SpatialPointsDataFrame*}\cr
#'
#'
#' Pending parameterization, \code{findGCHN} can also return:
#'
#' \item 'basemap':   A \code{RasterLayer*} basemap if \code{basemap = TRUE}
#' \item 'boundry':   A \code{SpatialPolygon*} of AOI if \code{boundary = TRUE}
#' \item 'fiat':      A \code{SpatialPolygon*} of intersected county boundaries if \code{boundary = TRUE}
#' \item 'ids':       A vector of station IDs if \code{ids = TRUE}
#' }
#'
#' @examples
#' \dontrun{
#' # Find GHCN stations in El Paso County, Colorado with Preciptiation Readings:
#'
#' el.paso = findGHCN(state = 'CO',
#'                   county = 'El Paso',
#'                   boundary = TRUE,
#'                   basemap = 'r',
#'                   ids = TRUE,
#'                   paramerters = 'prcp')
#' # Static Mapping
#'
#' plot(el.paso$basemap)
#' plot(el.paso$boundary, add = T)
#' plot(el.paso$ghcn, add = T)
#'
#' # Generate Interactive Map
#'
#'  explore(el.paso, save = TRUE)
#'
#' # Get Precipiation data for stations
#'
#'  ppt = getGHCN(ids = el.paso$ids, parameters = 'prcp')
#'
#' # Interactivly explore timeseries data
#'
#'  inspect(ppt, save = TRUE)
#' }
#'
#' @export
#' @author
#' Mike Johnson
#'

state = NULL
county = NULL
clip_unit = list("UCSB", 10, 10)

findGHCN = function(state = NULL,
                    county = NULL,
                    clip_unit = NULL,
                    parameters = NULL,
                    boundary = FALSE,
                    basemap = FALSE,
                    ids = FALSE,
                    save = FALSE) {

  AOI = AOI::getAOI(state, county, clip_unit)

  stations = HydroData::ghcn_stations

  stations = stations[which(stations$LAT <= AOI@bbox[2, 2]),]
  stations = stations[which(stations$LAT >= AOI@bbox[2, 1]),]
  stations = stations[which(stations$LON >= AOI@bbox[1, 1]),]
  stations = stations[which(stations$LON <= AOI@bbox[1, 2]),]

  if(!is.null(parameters)) {
    parameters = toupper(parameters)
    stations = stations %>% dplyr::filter(PARAMETER %in% parameters)
  }

  if (dim(stations)[1] == 0) { stop("0 stations found in AOI") }

  sp = sp::SpatialPointsDataFrame(cbind(stations$LON, stations$LAT), stations, proj4string = AOI::HydroDataProj)

  sp = sp[AOI,]

  message(length(sp), " GHCN stations found")

  items = list(name = AOI::nameAOI(state, county, clip_unit),
               souce = "NOAA GHCN",
               ghcn = sp)

  report = "Returned list includes: NOAA GHCN shapefile"

  items = return.what(sp, items, report, AOI, boundary, clip_unit, ids = if(ids){ids = 'ID'})

  if (save) {
    save.file(
      data = items,
      state = state,
      county = county,
      clip_unit = clip_unit,
      agency  = 'NOAA',
      source  = "GHCN",
      dataset = "ghcn",
      other   = NULL
    )
  }

  class(items) = "HydroData"

  return(items)
}

