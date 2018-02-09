#' Locate all NOAA GCHN Stations within Area of Interest
#'
#' Function to find GCHN stations within Area of Interest
#'
#' @param state a character string. Can be full name or state abbriviation
#' @param county a character string. Can be full name or state abbriviation
#' @param clip_unit can be provided as a shapefile or as a vector defineing centroid and bounding box diminsion
#' @param get.boundary logical. If TRUE, the AOI shapefile will be returned with station data in a list
#' @param get.basemap logical. If TRUE, the google basemap will be returned with gage station in a list
#' @param ids logical. If TRUE, return a vector of station ids in AOI. Can be passed to \code{getGCHN}
#' @param save logical. If TRUE, all data is written to a HydroData folder in the working directory
#'
#' @examples
#' \dontrun{
#' #Find all GCHN stations in Harris County, Texas
#'
#' harris.clim = findGCHN(state = 'TX', county = 'Harris',
#'               keep.boundary = TRUE, keep.basemap = TRUE, ids = TRUE, save = TRUE)
#' plot(harris.clim$basemap)
#' plot(harris.clim$boundary, add = TRUE, lwd = 5)
#' plot(harris.clim$noaa, add = TRUE, pch = 16, col = 'blue')
#'
#' #Get Station IDs
#'
#' harris.clim$ids
#' }
#'
#' @author
#' Mike Johnson

findGCHN = function(state = NULL, county = NULL, clip_unit = NULL, get.boundary = FALSE, get.basemap = NULL, ids = FALSE, save = FALSE){

  items =  list()
  report = vector(mode = 'character')

  A = getAOI(state = state, county = county, clip_unit = clip_unit)
  bb = A@bbox
  message("AOI defined as the ", nameAOI(state = state, county = county, clip_unit = clip_unit), ". Loading global GHCN data...\n")

  tempfile = tempfile()

  url = "ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-stations.txt"
  download.file(url = url, destfile = tempfile)

  stations <- readr::read_fwf(tempfile,
                              readr::fwf_positions(start = c(1, 13, 22, 32, 42),
                              end = c(11, 20, 30, 38, 72),
                              col_names = c("ID", "LAT", "LON", "ELEV", "NAME")),
                              col_types = "cdddc")

  message("All GHCN Data loaded: ", formatC(dim(stations)[1], format="d", big.mark=","), " stations in total.\n")

  stations = stations %>% drop_na(stations$LAT) %>%
    drop_na(stations$LAT) %>%
    mutate(LAT = as.numeric(stations$LAT), LON = as.numeric(stations$LON)) %>%
    filter(LAT  <= bb[2,2]) %>%
    filter(LAT  >= bb[2,1]) %>%
    filter(LON >= bb[1,1]) %>% filter(LON <= bb[1,2])

  if(length(stations == 0)){stop("0 stations found in specified AOI.")}
  sp = SpatialPointsDataFrame(cbind(stations$LON, stations$LAT), stations)
  sp@proj4string = A@proj4string
  sp = sp[A, ]

  message(length(sp), " GHCN stations found in ", nameAOI(state = state, county = county, clip_unit = clip_unit),"\n")

  items[['stations']] = sp
  report = append(report, "Returned list includes: NOAA GCHN shapefile")

  if (all(get.boundary, !is.null(clip_unit))) {items[['boundary']] = getBoundary(A);
                                report = append(report, "boundary shapefile")
      }
  if (!is.null(get.basemap))  {items[['basemap']]  = getBasemap(A, type = get.basemap)
                                report = append(report, "basemap raster")}
  if (ids)                     {items[['ids']] = sp$ID
                                report = append(report, "list of station IDs")}

  if(length(report) > 1) {report[length(report)] = paste("and",  tail(report, n = 1))}
  message(paste(report, collapse = ", "))

  if(save){
    save.file(data = items,
              state = state,
              county = county,
              clip_unit = clip_unit,
              agency  = 'NOAA',
              source  = "GCHN",
              dataset = "stations",
              other   = NULL )
  }

  return(items)
}


#' Get GCHN data from list of station IDs
#'
#' Function to download a dataframe of meterological data from known GCHN station. See findGCHN function to find station ids by AOI.
#'
#' @param IDs a list or single of station IDs
#' @param parameters a character string. Should the returned data be subset to a particular parameter(s)?
#' @param years  a character or numeric string. Should the returned data be subset to a particular year(s)?
#'
#' @author
#' Mike Johnson
#' @export


getGCHN = function(IDs = NULL, parameters = NULL, years = NULL) {

  DATA = list()

  for(i in seq_along(IDs)){

    url <- paste("ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/all/", IDs[i], ".dly", sep = "")
    dest3 = tempfile()
    download.file(url, destfile = dest3)

    daily = readr::read_fwf(dest3,
                            col_positions = readr::fwf_positions(start = c(1, 12, 16, 18, seq(22, 262, 8)),
                                                                 end = c(11, 15, 17, 21, seq(26, 266, 8)),
                                                                 col_names = c("site_no", "YEAR", "MONTH", "PARAMETER", paste0( 1:31))),
                            col_types = paste0(c("cicc", rep("i", 31)), collapse = ""))

    message("FILE READ IN. TIME TO MANIPULATE!")

    if (is.null(parameters)) { p <- unique(daily$PARAMETER) } else {p = parameters}
    daily = daily %>% filter(PARAMETER %in% toupper(p))


    missing.elements <- setdiff(toupper(p), unique(daily$PARAMETER))
    if (length(missing.elements) > 0)
      warning(paste(missing.elements, collapse = ", "), "not avaialable for stations...")

    if (!is.null(years)) { daily = daily %>% filter(YEAR %in% years) }

    # Set missing values from -9999 to NA
    daily[daily==-9999] <- NA

    ## Separate by element

    fin <- lapply(p, function(param) {

      test =   daily %>%
        filter(PARAMETER == param) %>%
        dplyr::select_(quote(-PARAMETER))

      test = gather(test, DAY, param, -site_no, -YEAR, -MONTH)

      test2 = test %>%  mutate(Date = as.Date(with(test, paste(YEAR, MONTH, DAY, sep="-")), "%Y-%m-%d"), agency_code = "NOAA") %>%
        arrange(Date) # %>% dplyr::select(-YEAR,-MONTH,-DAY)

      test2 =  test2[c(1,4,3,2)]
      names(test2) = c("site_no", "agency_code", "Date" , tolower(eval(param)))
      return(test2)
    })

    names(fin) <- paste0(p)

    DATA[[eval(IDs[i])]] = fin
    message( "Sucess: ", i, " of ", length(IDs))

  }

  return(DATA)
}
