#' Airports
#'
#' Dataset containing information global Airports subset to the Daymet Domain
#'
#' @docType data
#'
#' @format a \code{dataframe} instance, 1 row per station with columns:
#' \itemize{
#' \item 'name':    A \code{character}  Name of airport. May or may not contain the City name.
#' \item 'city':    A \code{character}  Main city served by airport. May be spelled differently from Name.
#' \item 'country': A \code{character}  Country or territory where airport is located.
#' \item 'IATA':    A \code{character}  3-letter IATA code
#' \item 'ICAO':    A \code{numeric}    4-letter ICAO code
#' \item 'lat':     A \code{numeric}    Latitude of airport
#' \item 'lon':     A \code{numeric}    Longitude of airport
#' }
#'
#' @source  \href{https://openflights.org/data.html}{OpenFlights}
#'
#' @examples
#' \dontrun{
#'  airports = HydroData::ap
#' }
#'

"ap"

#' USGS NWIS station information
#'
#' Dataset containing information about USGS stations in the United States
#'
#' @docType data
#'
#' @format a \code{dataframe} instance, 1 row per station with columns:
#' \itemize{
#' \item 'OBJECTID':      A \code{character} Object id in the dataset
#' \item 'feature_id':    A \code{character} NHD COMID of reach
#' \item 'site_no':       A \code{character} USGS site number
#' \item 'site_name':     A \code{character} USGS site name
#' \item 'da_sqkm':       A \code{numeric}   Area that drains to the location in square kilometers
#' \item 'lat_reachCent': A \code{numeric}   Latitude of NHD reach center
#' \item 'lon_reachCent': A \code{numeric}   Longitude of NHD reach center
#' }
#'
#' @source Compiled from USGS and NHD datasets
#'
#' @examples
#' \dontrun{
#'  usgs = HydroData::usgsStations
#' }

"usgsStations"

#' Snotel Stations
#'
#' \code{snotel}NRCS SNOTEL station metadata
#'
#' @docType data
#'
#' @format a \code{dataframe} instance, 1 row per station with columns:
#' \itemize{
#' \item 'NETWORK':    A \code{integer} Network of interest
#' \item 'STATE':      A \code{integer}  State abbriviation
#' \item 'NAME':       A \code{character} Site name
#' \item 'START.DATE': A \code{integer}    Day of first measurement
#' \item 'LAT':        A \code{integer} Station Latitude
#' \item 'LONG':       A \code{character}  Station Longitude
#' \item 'ELEV':       A \code{character} Station Elevation
#' \item 'COUNTY':     A \code{character}  County
#' \item 'HUC12.NAME': A \code{character}  HUC 12 Name
#' \item 'HUC12.ID':   A \code{character}  HUC 12 code
#' }
#'
#' @source \href{https://wcc.sc.egov.usda.gov/nwcc/yearcount?network=sntl&state=&counttype=statelist}{SNOTEL INFORMATION}
#'
#' @examples
#' \dontrun{
#'  tiles = HydroData::snotelStations.rda
#' }

"snotel"
