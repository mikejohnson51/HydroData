#' @title Find stations from the Automated Surface Observing System
#' @description Query station information from the automated surface observing systems (ASOS) API. \code{findASOS} returns a \code{SpatailPointsDataframe} of all
#' stations within an AOI. Some stations have data dating back 1901.
#' All data comes from  \href{http://www.nws.noaa.gov/ost/asostech.html}{NOAA} and includes the following attributes:
#' \itemize{
#' \item 'USAF'   : \code{character}  Air Force station ID. May contain a letter in the first position.
#' \item 'WBAN'   : \code{character}  NCDC WBAN number
#' \item 'NAME'    : \code{integer}    Station Name
#' \item 'CTRY'    : \code{integer}    FIPS country ID
#' \item 'ST'   : \code{character}   State for US stations
#' \item 'ICAO'    : \code{integer}   ICAO Airport ID
#' \item 'Start'    : \code{integer}    Beginning Period Of Record (YYYY-MM-DD)
#' \item 'End'    : \code{integer}    Ending Period Of Record (YYYY-MM-DD)
#' }\cr
#' @param AOI  A Spatial* or simple features geometry, can be piped from \link[AOI]{getAOI}
#' @param ids  If TRUE,  a vector of unique Air Forces Station IDs is added to retuned list (default = \code{FALSE})
#' @return a list() of minimum length 2: AOI and and asos
#' @examples
#' \dontrun{
#' sta = getAOI(state = "CO", county = "El Paso") %>% findACIS()
#' }
#' @author Mike Johnson
#' @export

findASOS = function(AOI, ids = FALSE){

  ## index: ftp://ftp.ncdc.noaa.gov/pub/data/noaa/

  if(!(class(AOI) %in% c("list","HydroData"))){AOI = list(AOI = AOI)}

  x = readLines("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.txt")

  x = x[23:NROW(x)]

  mydata <- data.frame(USAF      = substr(x, 1,  6),
                       WBAN     = substr(x, 8, 12),
                       NAME     = substr(x, 13, 42),
                       CTRY    =  substr(x, 44, 45),
                       ST  = substr(x, 47, 48),
                       CALL    = substr(x, 51, 55),
                       LAT = as.numeric(gsub("+", "", substr(x, 57, 64))),
                       LON = as.numeric(gsub("+", "", substr(x, 66, 73))),
                       ELEV =  as.numeric(gsub("+", "", substr(x, 75, 81))),
                       Start   = as.Date(substr(x, 83, 90), format = "%Y%m%d"),
                       End   = as.Date(substr(x, 92, 99), format = "%Y%m%d"),
                       stringsAsFactors = FALSE
  )

   mydata = mydata[!is.na(mydata$LAT),]
   mydata = mydata[!is.na(mydata$LON),]
   sum(mydata =="")

   sp = sf::st_as_sf(x = mydata, coords = c("LON", "LAT"))
   sf::st_crs(sp) = 4269
   sp = suppressMessages( sp[st_as_sf(AOI$AOI), ] )

  if(!is.null(sp)){

    AOI[["asos"]] = sf::as_Spatial(sp)

    report = paste(length(sp), "ACOS Stations")

    AOI = return.what(AOI, type = 'asos', report, vals = if(ids){"USAF"})
  }

  return(AOI)
}





