#' Find all NHD flowlines within an Area of Interest
#'
#' \code{findNHD} finds all NHD flowlines for an Area of Interest. Metadata allows for easy data download via
#'  the \code{getNWM} function. To better understand how to define an AOI see \code{?getAOI}.
#'
#'
#'  \code{findNHD} returns a named list of minimum length 1:
#' \enumerate{
#' \item 'flowlines': A \code{SpatialLinesDataFrame} of NHD flowlines and metadata
#' \item 'basemap':   A \code{RasterLayer} basemap if 'basemap' is \code{TRUE}
#' \item 'fiat':      A \code{SpatialPolygon} of fiat boundaries if 'boundary' is \code{TRUE}
#' \item 'clip':      A \code{SpatialPolygon} of clip unit boundary if 'boundary' is \code{TRUE}
#' \item 'ids':       A vector of flowline COMIDs if 'ids' is \code{TRUE}
#' }
#'
#' @param state     character. Provide full name(s) or two character abbriviation(s). Not case senstive
#' @param county    character. Provide county name(s). Requires \code{state} input.
#' @param clip_unit SpatialObject* or list. For details see \code{?getClipUnit}
#' @param boundary  logical. If TRUE, the AOI \code{SpatialPolygon(s)} will be joined to returned list
#' @param basemap   logical. If TRUE, a basemap will be joined to returned list from \code{\link[dismo]{gmap}}.
#'
#'  #' If a user wants greater control over basemap apperance replace TRUE with either:
#' \itemize{
#' \item't':  a terrain imagery basemap
#' \item's':  a sattilite imagery basemap
#' \item'h':  a hybrid imagery basemap
#' \item'r':  a roadmap imagery basemap
#' }
#'
#' @param ids logical. If TRUE, returns a list of COMIDS for all stream reaches in AOI
#' @param save logical. If TRUE, all data is saved to a HydroData folder created in users working directory. Find working directory with \code{\link[getwd()]}
#'
#' @export
#' @seealso \itemize{
#'          \item \code{\link{getClipUnit}}
#'          \item \link{getAOI}
#'          \item \link[HydroData]{readNWM}
#'          }
#'
#' @family HydroData 'find' functions
#'
#' @return All HydroData outputs are projected to \emph{'+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0+no_defs'}
#'
#'
#'
#' @examples
#' \dontrun{
#' # Find NHD data in El Paso County, Colorado
#'
#' el.paso = findNHD(state = 'CO',
#'                   county = 'El Paso',
#'                   boundary = TRUE,
#'                   basemap = 'r')
#'
#' plot(el.paso$basemap)
#' plot(el.paso$fiat, add = TRUE, lwd = 5)
#' plot(el.paso$flowlines, add = TRUE, col = 'blue')
#'
#' }
#'
#' @author Mike Johnson

findNHD = function(state = NULL,
                   county = NULL,
                   clip_unit = NULL,
                   boundary = TRUE,
                   basemap = NULL,
                   ids = FALSE,
                   save = FALSE) {
  items =  list()
  report = vector(mode = 'character')
  AOI = getAOI(state = state,
               county = county,
               clip_unit = clip_unit)
  message(
    "AOI defined as the ",
    nameAOI(
      state = state,
      county = county,
      clip_unit = clip_unit
    ),
    ". Shapefile determined. Now loading NHD flowline data...\n"
  )

  bb = AOI@bbox

  URL = paste0(
    "https://cida.usgs.gov/nwc/geoserver/nhdplus/ows?service=WFS&version=2.0.0&request=GetFeature&typeNames=nhdplus:nhdflowline_network&srsName=EPSG:4326&bbox=",
    min(bb[2, ]),
    ",",
    min(bb[1, ]),
    ",",
    max(bb[2, ]),
    ",",
    max(bb[1, ]),
    "&outputFormat=SHAPE-ZIP"
  )

  sl = download.shp(URL = URL, type = 'NHD flowlines') %>% spTransform(HydroDataProj)
  sl = sl[AOI, ]


  items[['flowlines']] = sl
  report = append(report, "Returned list includes: flowline shapefile")

  if (boundary) {
    if (!is.null(clip_unit)) {
      items[['fiat']] = getFiatBoundary(clip_unit = sl)
      report = append(report, "fiat boundary shapefile")
      items[['clip']] = AOI
      report = append(report, "clip boundary shapefile")
    } else {
      items[['fiat']] = AOI
      report = append(report, "boundary shapefile")
    }
  }

    if (any(basemap, !is.null(basemap))) {
      items[['basemap']] =  getBasemap(sl, type = basemap)     ##AOI$bmap
      report = append(report, "basemap raster")
    }

    if (ids) {
      items[['ids']] = sl$comid
      report = append(report, "list of common identifiers
                      ")
    }

    if (length(report) > 1) {
      report[length(report)] = paste("and",  tail(report, n = 1))
    }

  message(paste(report, collapse = ", "))

    if (save) {
      save.file(
        data = items,
        state = state,
        county = county,
        clip_unit = clip_unit,
        agency  = 'USGS',
        source  = "NHD",
        dataset = "flowlines",
        other   = NULL
      )
    }

    return(items)
    }

