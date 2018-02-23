#' Find GHCN Stations within Area of Interest
#'
#' \code{findGHCN} finds Global Historical Climatology Network (GHCN) stations within Area of Interest. Metadata allows for easy data download via
#'  the \code{getGHCN} function. To better understand how to define an AOI see \code{?getAOI}.
#'
#' \code{findGHCN} returns a named list of minimum length 1:
#' \enumerate{
#' \item 'ghcn':      A \code{SpatialPointsDataFrame} of GHCN stations and metadata
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
#' If a user wants greater control over basemap apperance choose from:
#' \itemize{
#' \item't':  a terrain imagery basemap
#' \item's':  a sattilite imagery basemap
#' \item'h':  a hybrid imagery basemap
#' \item'r':  a roadmap imagery basemap
#' }
#'
#'
#' @param parameters character. Specifiy the variable of interest.The fice principle elemets are:\cr
#' PRCP = Precipitation (tenths of mm)\cr
#' SNOW = Snowfall (mm)\cr
#' SNWD = Snow depth (mm)\cr
#' TMAX = Maximum temperature (tenths of degrees C)\cr
#' TMIN = Minimum temperature (tenths of degrees C)\cr
#' \cr
#' @param ids logical. If TRUE, returns a list of COMIDS for all stream reaches in AOI
#' @param save logical. If TRUE, all data is saved to a HydroData folder created in users working directory. Find working directory with \code{\link}

#' @examples
#' \dontrun{
#' #Find all GHCN stations in Harris County, Texas
#'
#' harris = findGHCN(state = 'TX',
#'                   county = 'Harris',
#'                   boundary = TRUE,
#'                   basemap = TRUE)
#'
#' plot(harris$basemap)
#' plot(harris$fiat, add = TRUE, lwd = 5)
#' plot(harris$ghcn, add = TRUE, pch = 16, col = 'blue')
#'
#' }
#'
#' @export
#' @seealso \itemize{
#'          \item \code{\link{getClipUnit}}
#'          \item \link{getAOI}
#'          \item \link[HydroData]{getGHCN}
#'          }
#'
#' @family HydroData 'find' functions
#'
#' @return All HydroData outputs are projected to \emph{'+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0+no_defs'}
#'
#' @author
#' Mike Johnson

findGHCN = function(state = NULL,
                    county = NULL,
                    clip_unit = NULL,
                    boundary = FALSE,
                    basemap = NULL,
                    parameters = NULL,
                    ids = FALSE,
                    save = FALSE) {

  items =  list()
  report = vector(mode = 'character')

  A = getAOI(state = state,
             county = county,
             clip_unit = clip_unit)
  bb = A@bbox
  message ("AOI defined as the ",
    nameAOI (state = state, county = county, clip_unit = clip_unit),
    ". Loading global GHCN data...\n"
  )

  load("data/ghcnStations.rda")

  stations  = ghcn_stations %>% dplyr::filter(LAT <= bb[2, 2]) %>%
    dplyr::filter(LAT  >= bb[2, 1]) %>%
    dplyr::filter(LON >= bb[1, 1]) %>% dplyr::filter(LON <= bb[1, 2])

  if(!is.null(parameters)) {
    stations = stations %>% dplyr::filter(PARAMETER %in% parameters)
  }

  if (length(stations) == 0) {
    stop("0 stations found in specified AOI.")
  }

  sp = SpatialPointsDataFrame(cbind(stations$LON, stations$LAT), stations)
  sp@proj4string = HydroDataProj
  sp = sp[A,]



  message(length(sp), " GHCN stations found in ",
          nameAOI (state = state, county = county, clip_unit = clip_unit))

  items[['ghcn']] = sp
  report = append(report, "Returned list includes: NOAA GHCN shapefile")

  if (boundary) {
    if (!is.null(clip_unit)) {
      items[['fiat']] = getFiatBoundary(clip_unit = sp)
        report = append(report, "fiat boundary shapefile")
      items[['clip']] = A
        report = append(report, "clip boundary shapefile")
    } else {
      items[['fiat']] = A
        report = append(report, "boundary shapefile")
    }
  }

  if (any(basemap == TRUE, !is.null(basemap))) {
    items[['basemap']] =  getBasemap(A, type = basemap)
    report = append(report, "basemap raster")
  }

  if (ids){
    items[['ids']] = unique(sp$ID)
    report = append(report, "list of station IDs")
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
      agency  = 'NOAA',
      source  = "GHCN",
      dataset = "ghcn",
      other   = NULL
    )
  }

  return(items)
}

#' Get GHCN data from list of station IDs
#'
#' Function to download a dataframe of meterological data from known GHCN station. See findGHCN function to find station ids by AOI.
#'
#' @param IDs a list or single of station IDs
#' @param parameters a character string. Should the returned data be subset to a particular parameter(s)?
#' @param years  a character or numeric string. Should the returned data be subset to a particular year(s)?
#'
#' @author
#' Mike Johnson
#' @export



getGHCN = function(IDs = NULL,
                   parameters = NULL,
                   years = NULL) {
  DATA = list()

  for (i in seq_along(IDs)) {
    url <-
      paste("ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/all/",
            IDs[i],
            ".dly",
            sep = "")
    dest3 = tempfile()
    download.file(url, destfile = dest3)

    daily = readr::read_fwf(
      dest3,
      col_positions = readr::fwf_positions(
        start = c(1, 12, 16, 18, seq(22, 262, 8)),
        end = c(11, 15, 17, 21, seq(26, 266, 8)),
        col_names = c("site_no", "YEAR", "MONTH", "PARAMETER", paste0(1:31))
      ),
      col_types = paste0(c("cicc", rep("i", 31)), collapse = "")
    )

    message("FILE READ IN. TIME TO MANIPULATE!")

    if (is.null(parameters)) {
      p <- unique(daily$PARAMETER)
    } else {
      p = parameters
    }


    daily = daily %>% dplyr::filter(PARAMETER %in% toupper(p))


    missing.elements <- setdiff(toupper(p), unique(daily$PARAMETER))

    if (length(missing.elements) > 0)
      warning(paste(missing.elements, collapse = ", "),
              "not avaialable for stations...")

    if (!is.null(years)) {
      daily = daily %>% filter(YEAR %in% years)
    }

    # Set missing values from -9999 to NA
    daily[daily == -9999] <- NA

    ## Separate by element
param = p
    fin <- lapply(p, function(param) {
    ## Select parameters of interset
      test =   daily %>%
        dplyr::filter(PARAMETER == param) %>%
        dplyr::select_(quote(-PARAMETER))

      test = tidyr::gather(test, DAY, param,-site_no,-YEAR,-MONTH)

      test2 = test %>%  dplyr::mutate(Date = as.Date(with(
        test, paste(YEAR, MONTH, DAY, sep = "-")
      ), "%Y-%m-%d"),
      agency_code = "NOAA") %>%
        dplyr::arrange(Date) # %>% dplyr::select(-YEAR,-MONTH,-DAY)

      test2 =  test2[c(1, 7, 6, 5)]
      names(test2) = c("site_no", "agency_code", "Date" , tolower(eval(param)))
      return(test2)
    })

    names(fin) <- paste0(p)

    DATA[[eval(IDs[i])]] = fin
    message("Sucess: ", i, " of ", length(IDs))

  }

  return(DATA)
}

