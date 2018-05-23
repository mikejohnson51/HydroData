#' Get Pixel level Daymet Data for a lat long pair
#'
#' @description
#' \code{getDaymet_pixel} returns a HydroData object and \code{SpatialPointsDataFrame*} of airports closest to a defined location\cr
#' Returned \code{list} can be interactivly explored via \code{\link{inspect}}.\cr\cr
#'
#' @details  Parmeters returned include:
#' \itemize{
#'  \item{"year"}{ The year}
#'  \item{"yday"}{ The Julien Day}
#'  \item{"dayl"}{ Duration of the daylight period for the day in seconds}
#'  \item{"prcp"}{ Daily total precipitation in mm}
#'  \item{"srad"}{ Avergae Incident shortwave radiation flux density in w/m2}
#'  \item{"swe"}{ Snow Water Equivilent in kg/m2 }
#'  \item{"tmax"}{ Daily maximum 2-meter air temperature in degrees C}
#'  \item{"tmin"}{ Daily minimum 2-meter air temperature in degrees C}
#'  \item{"vp"}{ Water Vapor Pressure in Pa}
#'  \item{"Date"}{ Date}
#'  \item{"agency_cd"}{ DAYMET}
#'  \item{"site_no"}{ 1}
#' }
#'
#' @seealso  \code{\link{getGHCN}}
#' @seealso  \code{\link{getDaymet}}
#' @seealso  \code{\link{inspect}}
#'
#' @param lat (numeric) a latitude location
#' @param lon (numeric) a longitude location
#' @param start (numeric) the first year to grab data
#' @param end (numeric) the end year to grab data to
#'
#' @return A HydroData object with lat, lon, elevation, tileID, and data table
#' @export
#' @examples
#' \dontrun{
#'  # Get all Daymet Data for UCSB from 2015 - 2016
#'  ucsb.daymet =  getDaymet_pixel( lat = 34.4208,
#'                                  lon = -119.6982,
#'                                  start = 2015,
#'                                  end = 2016)
#'
#'  inspect(data = ucsb.daymet$data, param = "srad")
#' }
#'
#' @family HydroData 'get' functions
#' @author Mike Johnson
#'

getDaymet_pixel = function(lat = NULL,
                           lon = NULL,
                           start = NULL,
                           end = start + 1){

  base = "https://daymet.ornl.gov/single-pixel/api/data"
  max_year = as.numeric(format(Sys.time(), "%Y")) - 1

  if (start < 1980){ stop("Start year preceeds valid data range!")}

  if (end > (as.numeric(format(Sys.time(), "%Y")) - 1)){ stop(paste0(end, " not available.")) }

  range = seq(start, end, by = 1)
  vars = c("tmax","tmin","dayl","prcp","srad","swe","vp")


  file = file.path(normalizePath(tempdir()), paste0(start,"_",end,".csv"))

  message(paste0('Downloading data for: ', lat,'/',lon, ' (lat/long)\n'))

  url = paste0(base, "?lat=", lat, "&lon=", lon, "&vars=", paste0(vars, collapse  = "%2C"), "&year=", paste0(range, collapse  = "%2C"))

  download.file(url, file, quiet = TRUE)

  meta = try(readLines(file, n = 30), silent = TRUE)

  col_start = grep("year,yday", meta)

  meta = tolower(meta[1:(col_start-1)])

  pattern = "(\\d)+|(-\\d)+|(\\d+\\.\\d+)|(-\\d+\\.\\d+)|(\\d+.\\d+e\\d+)|(\\d+\\.\\d+e-\\d+)|(-\\d+.\\d+e\\d+)|(-\\d+\\.\\d+e-\\d+)"

  coords = as.numeric(unlist(regmatches(meta[grep("latitude", meta)],gregexpr(pattern, meta[grep("latitude", meta)]))))

  tile = as.numeric(gsub("[^\\d]+", "", meta[grep("tile", meta)], perl=TRUE))

  elev = as.numeric(gsub("[^\\d]+", "", meta[grep("elevation", meta)], perl=TRUE))

  lat = coords[1]

  lon = coords[2]

  data = data.table::fread(file, sep = ',',
                           skip = (col_start - 1),
                           header = TRUE)

  data$Date = as.Date(format(strptime(paste0(data$year,"-",data$yday), format="%Y-%j"), format="%Y-%m-%d"))
  data$agency_cd = "DAYMET"
  data$site_no = 1

  names(data) = gsub(' .*', '', names(data))

  message(paste0('Success!\n'))

  # put all data in a list
  items = list( 'lat' = lat,
                'long' = lon,
                'elev' = elev,
                'tileID' = tile,
                'data' = data)

  class(items) = "HydroData"

  return(items)
}


