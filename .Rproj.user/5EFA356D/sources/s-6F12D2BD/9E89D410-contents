#' Choose what to return for HydroData Calls
#'
#' @description  A function defining what should be returned in a HydroData object
#'
#' @param sp Spatial object
#' @param items an items list
#' @param report a report vector
#' @param AOI the defined AOI
#' @param basemap the basemap call
#' @param boundary the boundary call
#' @param clip_unit the clip_unit defintion
#' @param ids the ids call
#'
#' @family HydroData 'helper' functions
#'
#' @return a list of HydroData components
#' @export
#' @author Mike Johnson

return.what = function(sp,
                       items,
                       report,
                       AOI,
                       boundary,
                       clip_unit,
                       ids) {
  if (boundary) {
    items[['boundary']] = AOI
    report = append(report, "AOI boundary")

    if (!is.null(clip_unit)) {
      items[['fiat']] = AOI::getFiatBoundary(clip_unit = AOI)
      report = append(report, "fiat boundary")
    }
  }

  if (!is.null(ids)) {
    items[['ids']] = sp@data[, ids]
    report = append(report, "list of station IDs")
  }



  if (length(report) > 1) {
    report[length(report)] = paste("and",  tail(report, n = 1))
  }

  message(paste(report, collapse = ", "))
  return(items)

}
