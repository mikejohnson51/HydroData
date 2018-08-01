#' Choose what to return for HydroData Calls
#'
#' @description  A function defining what should be returned in a HydroData object
#'
#' @param AOI Spatial object
#' @param report a report vector
#' @param AOI the defined AOI
#' @param vals the ids call
#'
#' @family HydroData 'helper' functions
#'
#' @return a list of HydroData components
#' @export
#' @author Mike Johnson

return.what = function(AOI,
                       type,
                       report,
                       vals) {

  if (!is.null(vals)) {
    AOI[[vals]] = eval(parse(text = paste("AOI",type, vals, sep = "$")))
    report = append(report, "list of station IDs")
  }

  if (length(report) > 1) {
    report[length(report)] = paste("and",  tail(report, n = 1))
  }

  message(paste(report, collapse = ", "))

  class(AOI) = "HydroData"

  return(AOI)

}
