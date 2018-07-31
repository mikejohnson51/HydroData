#' Choose what to return for HydroData Calls
#'
#' @description  A function defining what should be returned in a HydroData object
#'
#' @param sp Spatial object
#' @param items an items list
#' @param report a report vector
#' @param AOI the defined AOI
#' @param boundary the boundary call
#' @param clip the clip defintion
#' @param ids the ids call
#'
#' @family HydroData 'helper' functions
#'
#' @return a list of HydroData components
#' @export
#' @author Mike Johnson

return.what = function(items,
                       report,
                       AOI,
                       ids) {
  # if (boundary) {
  #   items[['boundary']] = AOI
  #   report = append(report, "AOI boundary")
  #
  #   if (!is.null(clip)) {
  #     items[['fiat']] = AOI::getFiat(clip = AOI)
  #     report = append(report, "fiat boundary")
  #   }
  # }

  if (!is.null(ids)) {
    items[['ids']] = ids
    report = append(report, "list of station IDs")
  }

  if (length(report) > 1) {
    report[length(report)] = paste("and",  tail(report, n = 1))
  }

  message(paste(report, collapse = ", "))

  class(items) = "HydroData"

  return(items)

}
