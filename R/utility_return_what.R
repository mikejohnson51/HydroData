#' @title Choose what to return for HydroData Calls
#' @description  A function defining what should be returned in a HydroData object
#' @param AOI Spatial object
#' @param type the index of the AOI object to report on
#' @param report character sting to append values to
#' @param vals the ids call
#' @return a list of HydroData components
#' @export
#' @author Mike Johnson

return.what = function(AOI,
                       type,
                       report,
                       vals) {

  `%+%` = crayon::`%+%`

  if (!is.null(vals)) {
    AOI[[vals]] = eval(parse(text = paste("AOI",type, vals, sep = "$")))
    report = append(report, paste("list of", vals, "IDs"))
  }

  if (length(report) > 1) {
    report[length(report)] = paste("and",  tail(report, n = 1))
  }

  cat(crayon::white("Returned object contains: ") %+% crayon::green(paste(report, collapse = ", "), "\n"))

  class(AOI) = "HydroData"

  return(AOI)

}
