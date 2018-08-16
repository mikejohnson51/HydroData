#' @title Find Meterologic Stations from the U.S. Climate Reference Network
#' @description Query station information from U.S. Climate Reference Network ([USCRN](https://www.ncdc.noaa.gov/crn/)) and the U.S. Regional Climate Reference Network ([USRCRN](https://www.ncdc.noaa.gov/crn/usrcrn/)).
#' This networks has been established to monitor present and future climatic trends and variability to better define climatic change over the United States.
#' The rogram estimates a fully developed network of about 250 geographical locations are in place. \code{findUSCRN} returns a \code{SpatailPointsDataframe} of all
#' stations within an AOI. Data comes from the \href{http://www.rcc-acis.org/index.html}{NOAA} and includes the following attributes:
#' \itemize{
#' \item 'WBAN'   : \code{character}  Weather Bureau–Army–Navy (WBAN) identifiers
#' \item 'COUNTRY'   : \code{character}  Country FIPS code
#' \item 'STATE'    : \code{integer}    USA state
#' \item 'LOCATION'    : \code{integer}    Location description
#' \item 'VECTOR'   : \code{character}   Location descriptive vecotr
#' \item 'NAME'    : \code{integer}   Station names
#' \item 'ELEVATION'    : \code{integer}    Elavation
#' \item 'STATUS'    : \code{integer}    Is the station currently commissioned
#' \item 'COMMISSIONING'    : \code{integer}    Date of station the station commissioning
#' \item 'CLOSING'   : \code{character}   Date of station closing (blank = still running)
#' \item 'OPERATION'    : \code{integer}   Is station operational?
#' \item 'PAIRING'    : \code{integer}    Is the station paired with another networks
#' \item 'NETWORK'    : \code{integer}    USCRN or USRCRN station
#' }\cr
#' @param AOI  A Spatial* or simple features geometry, can be piped from \link[AOI]{getAOI}
#' @param ids  If TRUE,  a vector of unique acis station IDs is added to retuned list (default = \code{FALSE})
#' @return a list() of minimum length 2: AOI and and acis
#' @examples
#' \dontrun{
#' sta = getAOI(state = "CO", county = "El Paso") %>% findUSCRN()
#' }
#' @author Mike Johnson
#' @export

findUSCRN = function(AOI, ids = FALSE){

## index: ftp://ftp.ncdc.noaa.gov/pub/data/uscrn/products

if(!(class(AOI) %in% c("list","HydroData"))){AOI = list(AOI = AOI)}

x = read.table("ftp://ftp.ncdc.noaa.gov/pub/data/uscrn/products/stations.tsv", sep = '\t',header = T,quote='', comment.char='')

sp = sf::st_as_sf(x= x, coords = c("LONGITUDE", "LATITUDE"))
sf::st_crs(sp) <- 4269
sp = suppressMessages( sp[sf::st_as_sf(AOI$AOI), ])

if(!is.null(sp)){

  AOI[["uscrn"]] = sp

  report = paste(length(sp), "USCRN Stations")

  AOI = return.what(AOI, type = 'nnorms', report, vals = if(ids){"WBAN"})
}
return(AOI)
}
