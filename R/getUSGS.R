#' Get USGS data
#'
#' @description
#' \code{findUSGS} returns a list of \code{data.frame*} Object for stations of interst.\cr\cr
#'
#' THIS IS A SHAMELESS WRAPPER AROUND THE USGS dataRetrival \code{readNWISdv} function CREATED TO FIT THE HYDRODATA FRAMEWORK !!! \cr\cr
#'
#' By default this function will return streamflow data for a list of stations found with \code{getUSGS}
#' If you want a parameter other then steamflow, see list of parameters \href{https://help.waterdata.usgs.gov/codes-and-parameters/parameters}{here}\cr\cr
#' Returned \code{data.frame} can be interactivly explored via \code{\link{inspect}}
#'
#' @param IDs        NWIS station ID(s)
#' @param paramater  Default is streamflow (00060), but can be altered
#' @param save       If TRUE, data is written as a csv to a HydroData folder in users working directory.
#'
#' @seealso  \code{\link{findUSGS}}
#' @seealso  \code{\link{inspect}}
#'
#' @family HydroData 'get' functions
#'
#' @return
#' \code{getUSGS} returns a \code{data.frame*} of observation data
#'
#'
#' @examples
#'\dontrun{
#' # Find USGS station near UCSB
#'
#'  sta = findUSGS(clip_unit = list("UCSB", 10, 10), ids = T)
#'
#' # Generate Interactive Map
#'
#'  explore(sta)
#'
#' # Get histroic streamflow data for those stations
#'
#'  data = getUSGS(IDs = sta$ids, save = T)
#'
#' # View interactive timeseries of daily, and yearly averages
#'
#'  inspect(data, param = 'Flow', timestep = "daily")
#'  inspect(data, param = 'Flow', timestep = "yearly")
#'
#'}
#'
#' @export
#' @author
#' Mike Johnson


getUSGS = function(IDs = NULL, save = FALSE, parameter = "00060"){

  Q = dataRetrieval::readNWISdv(siteNumbers = IDs, parameterCd = parameter)

  Q = dataRetrieval::renameNWISColumns(Q)

  return(Q)
}



