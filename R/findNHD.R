#' Find National Hydrography Data Stream Networks
#'
#' @description
#' \code{findNHD} returns a list of \code{Spatial*} Objects cropped to an Area of Interest.\cr\cr
#' To better understand defining an AOI using '\emph{state}', '\emph{county}' and '\emph{clip_unit}' see \code{getAOI} and \code{getClipUnit}.\cr\cr
#' Returned \code{list} can be interactivly explored via \code{\link{explore}} and COMID values (\code{ids = TRUE}) allow for National Water Model access via \code{getNWM}.\cr\cr
#' All outputs are projected to \code{CRS'+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0+no_defs'} and stream networks are (\emph{down})loaded from the \href{https://cida.usgs.gov}{USGS}.
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
#' @param ids  If TRUE, returns a list of COMIDS for NHD reaches
#' @param save If TRUE, data is written to a HydroData folder in users working directory.
#'
#' @seealso  \code{\link{getAOI}}
#' @seealso  \code{\link{getNWM}}
#' @seealso  \code{\link{explore}}
#'
#' @family HydroData 'find' functions
#'
#' @return
#' \code{findNHD} returns a list of minimum length 1:
#'
#' \enumerate{
#' \item 'flowlines': A \code{SpatialLinesDataFrame}\cr
#'
#'
#' Pending parameterization, \code{findNHD} can also return:
#'
#' \item 'basemap':   A \code{RasterLayer*} basemap if \code{basemap = TRUE}
#' \item 'boundry':   A \code{SpatialPolygon*} of AOI if \code{boundary = TRUE}
#' \item 'fiat':      A \code{SpatialPolygon*} of intersected county boundaries if \code{boundary = TRUE}
#' \item 'ids':       A vector of COMIDs if \code{ids = TRUE}
#' }
#'
#'
#' @examples
#' \dontrun{
#' # Find NHD data for El Paso County, Colorado
#'
#' el.paso = findNHD(state = 'CO',
#'                   county = 'El Paso',
#'                   boundary = TRUE,
#'                   basemap = 'r',
#'                   ids = TRUE)
#'
#' # Static Mapping
#'
#'  plot(el.paso$basemap)
#'  plot(el.paso$boundary, lwd = 5, add = T)
#'  plot(el.paso$flowlines, col = 'blue', lwd = el.paso$flowlines$streamorde, add = T)
#'
#' # Generate Interactive Map
#'
#'   explore(el.paso, save = TRUE)
#'
#' # Get flow data for reaches
#'
#' flows = getNWM(ids = el.paso$ids, startDate = '2017-01-01', endDate = '2017-12-31')
#'
#' # Interactivly explore timeseries data
#'
#' inspect(flows,   save = TRUE)
#' }
#'
#' @export
#' @author Mike Johnson

findNHD = function(state = NULL,
                   county = NULL,
                   clip_unit = NULL,
                   boundary = FALSE,
                   basemap = FALSE,
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

  if (ids) { items[['ids']] = as.numeric(sl$comid)
  report = append(report, "list of COMIDs")
  }

  if (length(report) > 1) { report[length(report)] = paste("and",  tail(report, n = 1)) }

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

