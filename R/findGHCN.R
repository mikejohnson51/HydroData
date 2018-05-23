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

findGHCN = function(state = NULL,
                    county = NULL,
                    clip_unit = NULL,
                    boundary = FALSE,
                    basemap = FALSE,
                    parameters = NULL,
                    ids = FALSE,
                    save = FALSE) {

  items =  list()
  report = vector(mode = 'character')

  AOI = getAOI(state = state,
             county = county,
             clip_unit = clip_unit)
  bb = AOI@bbox
  message ("AOI defined as the ",
    nameAOI (state = state, county = county, clip_unit = clip_unit),
    ". Loading global GHCN data...\n"
  )

  ghcn_stations = HydroData::ghcn_stations

  stations  = ghcn_stations %>% dplyr::filter(LAT <= bb[2, 2]) %>%
    dplyr::filter(LAT  >= bb[2, 1]) %>%
    dplyr::filter(LON >= bb[1, 1]) %>% dplyr::filter(LON <= bb[1, 2])

  if(!is.null(parameters)) {
    parameters = toupper(parameters)
    stations = stations %>% dplyr::filter(PARAMETER %in% parameters)
  }

  if (length(stations) == 0) {
    stop("0 stations found in specified AOI.")
  }

  sp = SpatialPointsDataFrame(cbind(stations$LON, stations$LAT), stations)
  sp@proj4string = HydroDataProj
  sp = sp[AOI,]



  message(length(sp), " GHCN stations found in ",
          nameAOI (state = state, county = county, clip_unit = clip_unit))



  items[['ghcn']] = sp
  report = append(report, "Returned list includes: NOAA GHCN shapefile")


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
    if (type == 'r') { name = 'roads'   }

    items[['basemap']] = getBasemap(AOI = AOI, type = type)
    report = append(report, paste(name, "basemap"))
  }


  if (boundary) { items[['boundary']] = AOI
  report = append(report, "AOI boundary")

  if (!is.null(clip_unit)) { items[['fiat']] = getFiatBoundary(clip_unit = AOI)
  report = append(report, "fiat boundary")
  }
  }

  if (ids) { items[['ids']] = sp$ID
  report = append(report, "list of station IDs")
  }

  if (length(report) > 1) { report[length(report)] = paste("and",  tail(report, n = 1)) }

  message(paste(report, collapse = ", "))

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
