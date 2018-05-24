#' Find US Army Core Dams within Area of Interest
#'
#' @description  \code{findNID} finds all US Army Corps Dams for an Area of Interest from the National Inventory of Dams dataset.
#' This dataset is access through the dams package.
#'
#'  \code{findNID} returns a named list of minimum length 1:
#' \enumerate{
#' \item 'dams':      A \code{SpatialPointsDataFrame} of NID dams and metadata
#' \item 'basemap':   A \code{RasterLayer} basemap if 'basemap' is \code{TRUE}
#' \item 'fiat':      A \code{SpatialPolygon} of fiat boundaries if 'boundary' is \code{TRUE}
#' \item 'clip':      A \code{SpatialPolygon} of clip unit boundary if 'boundary' is \code{TRUE}
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
#' @param save logical. If TRUE, all data is saved to a HydroData folder created in users working directory. Find working directory with \code{getwd()}
#'
#' @seealso \itemize{
#'          \item \code{\link{getClipUnit}}
#'          \item \link{getAOI}
#'          \item \link[HydroData]{explore}
#'          }
#'
#' @family HydroData 'find' functions
#'
#' @return All HydroData outputs are projected to \emph{'+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0+no_defs'}
#'
#' @examples
#' \dontrun{
#' # Find all dams in Texas
#'
#' tx.dams = findNID(state = "TX", boundary = TRUE, basemap = 'r', save= TRUE)
#' plot(tx.dams$basmap)
#' plot(tx.dams$boundary, add = TRUE)
#' plot(tx.dams$dams, add = TRUE)
#'}
#' @export
#' @author
#' Mike Johnson

findNID = function(state = NULL, county = NULL, clip_unit = NULL, boundary = FALSE, basemap = FALSE, save = FALSE){

  require(dams)
  items =  list()
  report = vector(mode = 'character')

  A = getAOI(state = state, county = county, clip_unit = clip_unit)
    message("AOI defined as the ", nameAOI(state = state, county = county, clip_unit = clip_unit), ". Shapefile determined. Now loading loading NID database...")

  data(nid_cleaned, envir = environment())

  dams = nid_cleaned %>% tidyr::drop_na(Longitude, Latitude)

  sp = SpatialPointsDataFrame(cbind(dams$Longitude, dams$Latitude), data = dams)
  message("All dams in CONUS loaded: ", formatC(dim(sp)[1], format="d", big.mark=","), " dams in total")

  sp@proj4string = HydroDataProj
  sp = sp[A, ]
  message(formatC(as.numeric(length(sp)), format="d", big.mark=","), " NID dams found in ", nameAOI(state = state, county = county, clip_unit = clip_unit))

    items[['dams']] = sp
    report = append(report, "Returned list includes: NID dams shapefile")

    if (basemap)  {items[['basemap']] = getBasemap(AOI = A)
    report = append(report, "basemap")
    }

    if (boundary) {items[['boundary']] = A
    report = append(report, "boundary")
    }

    if(length(report) > 1) {report[length(report)] = paste("and",  tail(report, n = 1))
    }

    message(paste(report, collapse = ", "))


    if(save){
      save.file(data = items,
                state = state,
                county = county,
                clip_unit = clip_unit,
                agency  = 'USACE',
                source  = "NID",
                dataset = "dams",
                other   = NULL )
    }


  return(items)
}



