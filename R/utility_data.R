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

#' Global Historical Climatology Network (GHCN) daily data
#'
#' Dataset containing the GCHN daily station network
#'
#' @docType data
#'
#' @format a \code{dataframe} instance, 1 row per station with columns:
#' \itemize{
#' \item 'ID':         A \code{character} Station ID
#' \item 'NAME':       A \code{character} Station name
#' \item 'LAT':        A \code{numeric}   Station latitude
#' \item 'LON':        A \code{numeric}   Station longitude
#' \item 'PARAMETER':  A \code{character} Parameter being recorded
#' \item 'START_YEAR': A \code{integer}   Latitude of NHD reach center
#' \item 'END_YEAR':   A \code{integer}   Longitude of NHD reach center
#' }
#'
#' @source \href{
#' https://www.ncdc.noaa.gov/data-access/land-based-station-data/land-based-datasets/global-historical-climatology-network-ghcn}{
#' NOAA National Centers for Environmental Information}
#'
#' @examples
#' \dontrun{
#'  gchn = HydroData::ghcn_stations
#' }

"ghcn_stations"


#' NHD COMIDs outlets
#'
#' Dataset containing the NHD outlets
#'
#' @docType data
#'
#' @format a \code{dataframe} instance, 1 row per station with columns:
#' \itemize{
#' \item 'COMID':         A \code{integer} Station COMID
#' \item 'LAT':        A \code{numeric}   Station latitude
#' \item 'LON':        A \code{numeric}   Station longitude
#' }
#'
#' @source \href{
#' https://www.ncdc.noaa.gov/data-access/land-based-station-data/land-based-datasets/global-historical-climatology-network-ghcn}{
#' NOAA National Centers for Environmental Information}
#'
#' @examples
#' \dontrun{
#'  gchn = HydroData::comids
#' }

"nhd_outlet"
