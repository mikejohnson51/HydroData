#' Find US Census Bureau TIGER Road Networks
#'
#' @description
#' \code{findRoads} returns a list of \code{Spatial*} Objects cropped to an Area of Interest.\cr\cr
#' To better understand defining an AOI using '\emph{state}', '\emph{county}' and '\emph{clip_unit}' see \code{getAOI} and \code{getClipUnit}.\cr\cr
#' Returned \code{list} can be interactivly explored via \code{\link{explore}}.\cr\cr
#' All outputs are projected to \code{CRS '+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0+no_defs'} and road networks are (\emph{down})loaded from the 2017 \href{https://www2.census.gov/geo/tiger/TIGER2017}{US Census servers}.
#'
#' @param state    Full name(s) or two character abbriviation(s). Not case senstive
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
#' @param ids  If TRUE, returns a list of road names in AOI
#' @param save If TRUE, data is written to a HydroData folder in users working directory.
#'
#' @seealso  \code{\link{getAOI}}
#' @seealso  \code{\link{explore}}
#'
#' @family HydroData 'find' functions
#'
#' @return
#' \code{findRoads} returns a named list of minimum length 1:
#'
#' \enumerate{
#' \item 'roads': A \code{SpatialLinesDataFrame*}\cr
#'
#' Pending parameterization, \code{findRoads} can also return:
#'
#' \item 'basemap':   A \code{RasterLayer*} basemap if \code{basemap = TRUE}
#' \item 'boundry':   A \code{SpatialPolygon*} of AOI if \code{boundary = TRUE}
#' \item 'fiat':      A \code{SpatialPolygon*} of intersected county boundaries if \code{boundary = TRUE}
#' \item 'ids':       A vector of road names if \code{ids = TRUE}
#' }
#'
#' @examples
#'\dontrun{
#' # Find Roads Near UCSB
#'
#' roads = findRoads(clip_unit = list("UCSB", 10, 10), basemap = T, boundary = T)
#'
#' # Static Mapping
#'
#' plot(roads$basemap)
#' plot(roads$boundary, add = T)
#' plot(roads$roads, add = T)
#'
#' # Generate Interactive Map
#'
#' explore(roads)
#'}
#'
#' @export
#' @author
#' Mike Johnson

findRoads = function(state = NULL,
                     county = NULL,
                     clip_unit = NULL,
                     boundary = FALSE,
                     basemap = FALSE,
                     ids = FALSE,
                     save = FALSE) {
  #tempd = tempdir()

  ########## 1. Define AOI ##########

  items =  list()
  report = vector(mode = 'character')

  input.shp = list()

  A = getAOI(state = state,
               county = county,
               clip_unit = clip_unit)


  if (!is.null(clip_unit)) {
    AOI = getFiatBoundary(clip_unit = A)
  } else {
    AOI = A
  }

  FIP = AOI$geoid

  if (is.null(FIP)) {
    FIP = AOI$map$FIP
  }

  if (is.null(FIP)) {
    FIP = AOI$shp$FIP
  }

  FIP = sprintf("%05d", as.numeric(FIP))

  message(
    "AOI defined as the ",
    nameAOI(
      state = state,
      county = county,
      clip_unit = clip_unit
    ),
    ". Loading TIGER roads database...\n"
  )

  ########## 2. Download Data ##########

  urls = vector()

  for (i in 1:length(FIP)) {
    urls[i] = paste0(
      "https://www2.census.gov/geo/tiger/TIGER2017/ROADS/tl_2017_",
      FIP[i],
      "_roads.zip"
    )
  }

  #list.files(tempd)
  #temp = tempfile(fileext = ".zip")

  if (length(urls) > 1) {
    verb = 'are'
    noun = 'files'
  } else {
    verb = 'is'
    noun = 'file'
  }
  message(paste("There", verb, length(urls), "TIGER", noun, "in this scene."))

  for (i in seq_along(urls)) {
   input.shp[[i]] = download.shp(URL = urls[1], type = paste('TIGER', i)) %>% spTransform(HydroDataProj)
   input.shp[[i]] = input.shp[[i]][A,]

  #for (i in seq_along(urls)) {
    #message(1)
    #temp = tempfile(fileext = ".zip")
    #message(2)
   # message(paste0("Downloading file (", i, "/", length(urls), ")"))
    #download.file(url = urls[i],
     #             destfile = temp,
      #            quiet = TRUE)
    #unzip(temp, exdir = tempd, overwrite = TRUE)
    #message(paste0("Finished downloading file (", i, "/", length(urls), ")"))

  }

  ########## 3. Process Data ##########

  #all.files = list.files(tempd,
   #                      pattern = paste0(FIP, "_roads.shp$"),
    #                     full.names = TRUE)
  #bounds =  raster::extent(A)

  #for (j in seq_along(all.files)) {
  # message("Reading in shapefile ", j)
  #  input.shp[[j]] = rgdal::readOGR(all.files[j], verbose = FALSE) %>% spTransform(HydroDataProj)
  #  message("Cropping shapefile ", j)
  #  input.shp[[j]] <- input.shp[[j]][A, ]

    #input.shp[[j]] <- raster::crop(x = input.shp[[j]],  y = bounds)

  #  message("Shapefile number ", j, " Finished")
  # }

  df <- do.call("rbind", input.shp)

  #unlink(temp, recursive = T)
  #unlink(tempd, recursive = T)

  items[['roads']] = df
  report = append(report, "Returned list includes: TIGER roads shapefile")

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
    if (type == 'r') { name = 'roadmap'   }

    items[['basemap']] = getBasemap(AOI = A, type = type)
    report = append(report, paste(name, "basemap"))
  }


  if (boundary) {
    items[['boundary']] = A
    report = append(report, "AOI boundary")

    if (!is.null(clip_unit)) {
      items[['fiat']] = getFiatBoundary(clip_unit = A)
      report = append(report, "fiat boundary")
    }
  }

  if (ids) {
    items[['ids']] = df$FULLNAME
    report = append(report, "list of COMIDs")
  }

  if (length(report) > 1) {
    report[length(report)] = paste("and", tail(report, n = 1))
  }
  message(paste(report, collapse = ", "))

  if (save) {
    save.file(
      data = items,
      state = state,
      county = county,
      clip_unit = clip_unit,
      agency  = 'USCensus',
      source  = "TIGER",
      dataset = "roads",
      other   = NULL
    )
  }

  return(items)
}
